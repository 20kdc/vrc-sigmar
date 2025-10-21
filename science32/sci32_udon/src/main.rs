use anyhow::*;
use base64::prelude::*;
use sci32_ingest::*;
use std::collections::HashMap;
use std::fmt::Write;

mod asmwr;
use asmwr::*;

mod externs;
use externs::*;

/// Tries to determine that a string is reasonably safe for inclusion in an Udon symbol.
/// (Doesn't check that the start of a token isn't a digit; for various reasons, you'd have to do that on purpose.)
fn is_udon_safe(x: &str) -> bool {
    for v in x.chars() {
        if v.is_ascii_alphanumeric() {
            continue;
        } else if v == '_' {
            continue;
        }
        return false;
    }
    true
}

/// Resolves a fixed jump.
fn resolve_jump(img: &Sci32Image, to: u32) -> String {
    if img.is_instruction_at(to) {
        format!("_code_{:08X}", to)
    } else {
        // to prevent assembly failure if this happens to come up, abort
        // this can happen if, say, data is in .text
        // or if the compiler is feeling silly
        format!("0xFFFFFFFC # {:08X}", to)
    }
}

// To implement x0 properly:
// 1. `idec` tries to NOP and generally remove x0 writes as much as possible
// 2. Reads from `x0` are transformed into reads from a constant, while writes to `x0` are transformed into writes to a dummy.
// Registers marked with _ are not exported.
// REGISTERS_W has heap indices autocreated; REGISTERS_R does not.
// Except X0, they should match.
const REGISTERS_W: [&'static str; 32] = [
    "_vm_zero_nopwriteshadow",
    "vm_ra",
    "vm_sp",
    "_vm_x3",
    "_vm_x4",
    "_vm_t0",
    "_vm_t1",
    "_vm_t2",
    // x8/fp
    "_fp",
    "_s1",
    // For convenience/sanity, a0-a7 are not marked with any prefix at all.
    "a0",
    "a1",
    "a2",
    "a3",
    "a4",
    "a5", // x16
    "a6",
    "a7",
    "_vm_s2",
    "_vm_s3",
    "_vm_s4",
    "_vm_s5",
    "_vm_s6",
    "_vm_s7",
    // x24
    "_vm_s8",
    "_vm_s9",
    "_vm_s10",
    "_vm_s11",
    "_vm_t3",
    "_vm_t4",
    "_vm_t5",
    "_vm_t6",
];
// keep in sync!!!
const REGISTERS_R: [&'static str; 32] = [
    "_vm_zero", "vm_ra", "vm_sp", "_vm_x3", "_vm_x4", "_vm_t0", "_vm_t1", "_vm_t2",
    // x8/fp
    "_fp", "_s1", // For convenience/sanity, a0-a7 are not marked with any prefix at all.
    "a0", "a1", "a2", "a3", "a4", "a5", // x16
    "a6", "a7", "_vm_s2", "_vm_s3", "_vm_s4", "_vm_s5", "_vm_s6", "_vm_s7", // x24
    "_vm_s8", "_vm_s9", "_vm_s10", "_vm_s11", "_vm_t3", "_vm_t4", "_vm_t5", "_vm_t6",
];

fn resolve_alur(asm: &mut UdonAsm, value: Sci32ALUSource) -> String {
    match value {
        Sci32ALUSource::Immediate(v) => asm.ensure_i32(v as i32),
        Sci32ALUSource::Register(rs) => REGISTERS_R[rs as usize].to_string(),
    }
}

// WORKAROUND:
// We're now at the point where the workarounds themselves have more workarounds.
// Basically, forcing people to use System.Convert to change types around is known as 'cruel and unusual punishment'.
// We can't just use SByte and backconvert because of... well, a shit-ton of reasons.
// Basically anything smaller than a word has to be handled using unsigned.
struct LoadPipe {
    reader: String,
    /// Converter. For values smaller than a word, we need to run it through System.Convert to get it into an int.
    conv1: Option<(&'static str, String)>,
    /// Source high bit. We mask this off (to act as a detector) and then multiply the result by -1.
    /// For our purposes, we care about these three test cases:
    /// For 00000000, this becomes 00000000.
    /// For 00000080, this becomes FFFFFF80.
    /// For 00008000, this becomes FFFF8000.
    /// We can then OR the result with the original value, and we've just done an unsigned-signed conversion!
    signed_conv_high_bit: Option<String>,
}

fn main() -> Result<()> {
    let a = std::fs::read("science.elf")?;
    let mut img = Sci32Image::default();
    img.from_elf(&a).unwrap();
    let stack_words: u32 = 0x1000;
    let initial_sp = ((img.data.len() as u32) + stack_words) * 4;
    let abort_vec = (img.instructions as u32) * 4;
    let data = BASE64_STANDARD.encode(&img.initialized_bytes());

    let mut asm = UdonAsm::default();
    asm.declare_heap("_null", "SystemObject", "null", false);
    asm.declare_heap("_vm_tmp_bool", "SystemBoolean", "null", false);
    asm.declare_heap_i32("_vm_tmp_r1", 0, false);
    asm.declare_heap_i32("_vm_tmp_r2", 0, false);
    asm.declare_heap_u32("_vm_tmp_u1", 0, false);
    // WORKAROUND: Due to Udon Assembly bugs, we have to initialize some of these to null rather than 0.
    asm.declare_heap("_vm_tmp_u8", "SystemByte", "null", false);
    asm.declare_heap("_vm_tmp_u16", "SystemUInt16", "null", false);
    let highbit = asm.ensure_i32(0x80000000u32 as i32);
    asm.declare_heap(
        "_vm_initdata",
        "SystemString",
        &format!("\"{}\"", data),
        false,
    );
    asm.declare_heap("_vm_initdata_dec", "SystemByteArray", "null", false);
    // WORKAROUND: Udon has a habit of deciding it's gonna initialize vm_memory ITSELF.
    // Obviously this is bad for us, so we hold a second, hidden field of type Object.
    // It shouldn't initialize *that* behind our backs.
    asm.declare_heap("_vm_memory_chk", "SystemObject", "null", false);
    asm.declare_heap("vm_memory", "SystemByteArray", "null", true);
    // Declared public, but only initialized with memory.
    asm.declare_heap_i32("vm_initsp", 0, true);
    let const_initsp = asm.ensure_i32(initial_sp as i32);
    asm.declare_heap_i32("vm_abort", 0, true);
    let const_abort = asm.ensure_i32(abort_vec as i32);
    asm.declare_heap_u32("vm_indirect_jump_target", 0, true);

    for i in 0..32 {
        let authentic = REGISTERS_W[i];
        asm.declare_heap_i32(&authentic, 0, !authentic.starts_with("_"));
    }
    asm.declare_heap_i32("_vm_zero", 0, false);

    let ext = UdonExterns::new(&mut asm);

    let constn1 = asm.ensure_i32(-1);
    let const0 = asm.ensure_i32(0);
    let const1 = asm.ensure_i32(1);

    writeln!(asm.code, "\t# -- JUMP TABLE --").unwrap();
    for i in 0..img.instructions {
        writeln!(asm.code, "\tJUMP, _code_{:08X}", ((i * 4) as u32)).unwrap();
    }
    writeln!(asm.code, "\t# Thunk Abort ({:08X})", abort_vec).unwrap();
    writeln!(asm.code, "\tJUMP, 0xFFFFFFFC").unwrap();
    writeln!(asm.code).unwrap();

    writeln!(asm.code, "\t# -- MEMORY INIT / RESET / INDIRECT JUMP --").unwrap();

    writeln!(asm.code, ".export _vm_reset").unwrap();
    writeln!(asm.code, "_vm_reset:").unwrap();
    // The external reset just sets us up to indirect jump straight to abort.
    // This means you can just poke _vm_reset and get a nicely reset VM.
    writeln!(asm.code, "\tPUSH, {}", const_abort).unwrap();
    writeln!(asm.code, "\tPUSH, vm_indirect_jump_target").unwrap();
    writeln!(asm.code, "\tEXTERN, {}", ext.u32_fromi32).unwrap();

    // FALLTHROUGH: we've just setup the indirect jump to abort, so do a reset-and-jump

    writeln!(asm.code, ".export _vm_reset_and_jump").unwrap();
    writeln!(asm.code, "_vm_reset_and_jump:").unwrap();
    // set vm_initsp and vm_abort
    writeln!(asm.code, "\tPUSH, {}", const_initsp).unwrap();
    writeln!(asm.code, "\tPUSH, vm_initsp").unwrap();
    writeln!(asm.code, "\tPUSH, {}", const_abort).unwrap();
    writeln!(asm.code, "\tPUSH, vm_abort").unwrap();
    writeln!(asm.code, "\tCOPY").unwrap();
    writeln!(asm.code, "\tCOPY").unwrap();
    // setup like a thunk would. we're called by thunks when the machine hasn't been setup yet
    // and we can also be called by external code that won't know what the initsp/abort values are yet
    writeln!(asm.code, "\tPUSH, vm_initsp").unwrap();
    writeln!(asm.code, "\tPUSH, vm_sp").unwrap();
    writeln!(asm.code, "\tPUSH, vm_abort").unwrap();
    writeln!(asm.code, "\tPUSH, vm_ra").unwrap();
    writeln!(asm.code, "\tCOPY").unwrap();
    writeln!(asm.code, "\tCOPY").unwrap();
    // create the array
    writeln!(asm.code, "\tPUSH, {}", const_initsp).unwrap();
    writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
    writeln!(asm.code, "\tEXTERN, {}", ext.bytearray_create).unwrap();
    // copy to check field
    writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
    writeln!(asm.code, "\tPUSH, _vm_memory_chk").unwrap();
    writeln!(asm.code, "\tCOPY").unwrap();
    // decode the data
    writeln!(asm.code, "\tPUSH, _vm_initdata").unwrap();
    writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
    writeln!(asm.code, "\tEXTERN, {}", ext.base64_decode).unwrap();
    // copy
    writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
    writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
    writeln!(asm.code, "\tPUSH, {}", const0).unwrap();
    writeln!(asm.code, "\tEXTERN, {}", ext.bytearray_copy).unwrap();
    // clean up
    writeln!(asm.code, "\tPUSH, _null").unwrap();
    writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
    writeln!(asm.code, "\tCOPY").unwrap();

    // FALLTHROUGH: we now move right on to performing an indirect jump

    writeln!(asm.code, ".export _vm_indirect_jump").unwrap();
    writeln!(asm.code, "_vm_indirect_jump:").unwrap();
    // The jump table is laid out so that a simple multiplication will fix up the target.
    writeln!(asm.code, "\tPUSH, vm_indirect_jump_target").unwrap();
    writeln!(asm.code, "\tPUSH, vm_indirect_jump_target").unwrap();
    writeln!(asm.code, "\tPUSH, vm_indirect_jump_target").unwrap();
    writeln!(asm.code, "\tEXTERN, {}", ext.u32_add).unwrap();
    writeln!(asm.code, "\tJUMP_INDIRECT, vm_indirect_jump_target").unwrap();
    writeln!(asm.code).unwrap();

    let mut symbol_marking: HashMap<u32, String> = HashMap::new();
    writeln!(asm.code, "\t# -- THUNKS --").unwrap();
    for sym in img.symbols.values() {
        if !is_udon_safe(&sym.st_name) {
            continue;
        }
        symbol_marking.insert(sym.st_addr, sym.st_name.clone());
        // if the symbol starts with "Udon", assume this is code we want to be able to call from Udon.
        if sym.st_name.starts_with("Udon") {
            let cut_name = &sym.st_name[4..];
            writeln!(asm.code, ".export {}", cut_name).unwrap();
            writeln!(asm.code, "{}:", cut_name).unwrap();
            // check if the VM has inited; if it has, we speedrun init
            writeln!(asm.code, "\tPUSH, _vm_memory_chk").unwrap();
            writeln!(asm.code, "\tPUSH, _null").unwrap();
            writeln!(asm.code, "\tPUSH, _vm_tmp_bool").unwrap();
            writeln!(asm.code, "\tEXTERN, {}", ext.obj_equality).unwrap();
            writeln!(asm.code, "\tPUSH, _vm_tmp_bool").unwrap();
            writeln!(asm.code, "\tJUMP_IF_FALSE, _thunk_{}_fastpath", cut_name).unwrap();
            // Slowpath: The machine hasn't been setup yet!
            // Set indirect jump target and then run Reset-And-Jump.
            let myconst = asm.ensure_i32(sym.st_addr as i32);
            writeln!(asm.code, "\tPUSH, {}", myconst).unwrap();
            writeln!(asm.code, "\tPUSH, vm_indirect_jump_target").unwrap();
            writeln!(asm.code, "\tEXTERN, {}", ext.u32_fromi32).unwrap();
            writeln!(asm.code, "\tJUMP, _vm_reset_and_jump").unwrap();
            // Fastpath: Machine is ready. Copy registers and directly jump into code.
            writeln!(asm.code, "_thunk_{}_fastpath:", cut_name).unwrap();
            // Setup registers...
            writeln!(asm.code, "\tPUSH, vm_initsp").unwrap();
            writeln!(asm.code, "\tPUSH, vm_sp").unwrap();
            writeln!(asm.code, "\tPUSH, vm_abort").unwrap();
            writeln!(asm.code, "\tPUSH, vm_ra").unwrap();
            writeln!(asm.code, "\tCOPY").unwrap();
            writeln!(asm.code, "\tCOPY").unwrap();
            // Jump
            writeln!(
                asm.code,
                "\tJUMP, {}",
                resolve_jump(&img, sym.st_addr as u32)
            )
            .unwrap();
        }
    }
    writeln!(asm.code).unwrap();

    // ClientSim UI shows symbols in declaration order (presumably Udon preserves this somehow and we're seeing the results)
    // For this reason, make sure that symbol getters are written after the actual important stuff
    writeln!(asm.code, "\t# -- SYMGET --").unwrap();
    for sym in img.symbols.values() {
        if !is_udon_safe(&sym.st_name) {
            continue;
        }
        // either way, export as a 'data symbol'
        writeln!(asm.code, ".export _sym_{}", sym.st_name).unwrap();
        writeln!(asm.code, "_sym_{}:", sym.st_name).unwrap();
        let val = asm.ensure_i32(sym.st_addr as i32);
        writeln!(asm.code, "\tPUSH, {}", val).unwrap();
        writeln!(asm.code, "\tPUSH, a0").unwrap();
        writeln!(asm.code, "\tCOPY").unwrap();
        writeln!(asm.code, "\tJUMP, 0xFFFFFFFC").unwrap();
    }
    writeln!(asm.code).unwrap();

    writeln!(asm.code, "\t# -- CODE --").unwrap();
    for i in 0..img.instructions {
        let pc = (i * 4) as u32;
        if let Some(sym) = symbol_marking.get(&pc) {
            writeln!(asm.code).unwrap();
            writeln!(asm.code, "# SYMBOL: {}", sym).unwrap();
        }
        let istr = Sci32Instr::decode(pc, img.data[i]);
        writeln!(asm.code, "_code_{:08X}: # {:?}", pc, istr).unwrap();
        match istr {
            Sci32Instr::JumpAndLink {
                rd,
                rd_value,
                value,
            } => {
                if rd != 0 {
                    let si = asm.ensure_i32(rd_value as i32);
                    writeln!(asm.code, "\tPUSH, {}", si).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", REGISTERS_W[rd as usize]).unwrap();
                    writeln!(asm.code, "\tCOPY").unwrap();
                }
                writeln!(asm.code, "\tJUMP, {}", resolve_jump(&img, value)).unwrap();
            }
            Sci32Instr::JumpAndLinkRegister {
                rd,
                rd_value,
                rs1,
                offset,
            } => {
                let si = REGISTERS_R[rs1 as usize].to_string();
                if offset == 0 {
                    writeln!(asm.code, "\tPUSH, {}", si).unwrap();
                    writeln!(asm.code, "\tPUSH, vm_indirect_jump_target").unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", ext.u32_fromi32).unwrap();
                } else {
                    let ov = asm.ensure_i32(offset as i32);
                    writeln!(asm.code, "\tPUSH, {}", si).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", ov).unwrap();
                    writeln!(asm.code, "\tPUSH, _vm_tmp_r1").unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", ext.i32_add).unwrap();
                    writeln!(asm.code, "\tPUSH, _vm_tmp_r1").unwrap();
                    writeln!(asm.code, "\tPUSH, vm_indirect_jump_target").unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", ext.u32_fromi32).unwrap();
                }
                if rd != 0 {
                    let si = asm.ensure_i32(rd_value as i32);
                    writeln!(asm.code, "\tPUSH, {}", si).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", REGISTERS_W[rd as usize]).unwrap();
                    writeln!(asm.code, "\tCOPY").unwrap();
                }
                writeln!(asm.code, "\tJUMP, _vm_indirect_jump").unwrap();
            }
            Sci32Instr::SetRegister { rd, value } => {
                let si = resolve_alur(&mut asm, value);
                writeln!(asm.code, "\tPUSH, {}", si).unwrap();
                writeln!(asm.code, "\tPUSH, {}", REGISTERS_W[rd as usize]).unwrap();
                writeln!(asm.code, "\tCOPY").unwrap();
            }
            Sci32Instr::NOP => {
                writeln!(asm.code, "\tNOP").unwrap();
            }
            Sci32Instr::Load {
                rd,
                rs1,
                kind,
                unsigned,
                offset,
            } => {
                let mut s_addr = REGISTERS_R[rs1 as usize].to_string();
                let dst = REGISTERS_W[rd as usize].to_string();
                if offset != 0 {
                    let adj = asm.ensure_i32(offset as i32);
                    if rs1 == 0 {
                        s_addr = adj;
                    } else {
                        writeln!(asm.code, "\tPUSH, {}", s_addr).unwrap();
                        writeln!(asm.code, "\tPUSH, {}", adj).unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_tmp_r1").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext.i32_add).unwrap();
                        s_addr = "_vm_tmp_r1".to_string();
                    }
                }
                let pipe = match kind {
                    Sci32LSType::Byte => {
                        if unsigned {
                            LoadPipe {
                                reader: ext.read_byte.clone(),
                                conv1: Some(("_vm_tmp_u8", ext.i32_fromu8.clone())),
                                signed_conv_high_bit: None,
                            }
                        } else {
                            LoadPipe {
                                reader: ext.read_byte.clone(),
                                conv1: Some(("_vm_tmp_u8", ext.i32_fromu8.clone())),
                                signed_conv_high_bit: Some(asm.ensure_i32(0x80)),
                            }
                        }
                    }
                    Sci32LSType::Half => {
                        if unsigned {
                            LoadPipe {
                                reader: ext.read_u16.clone(),
                                conv1: Some(("_vm_tmp_u16", ext.i32_fromu16.clone())),
                                signed_conv_high_bit: None,
                            }
                        } else {
                            LoadPipe {
                                reader: ext.read_u16.clone(),
                                conv1: Some(("_vm_tmp_u16", ext.i32_fromu16.clone())),
                                signed_conv_high_bit: Some(asm.ensure_i32(0x8000)),
                            }
                        }
                    }
                    Sci32LSType::Word => LoadPipe {
                        reader: ext.read_i32.clone(),
                        conv1: None,
                        signed_conv_high_bit: None,
                    },
                };
                // Reader. Goes to destination or to conv1 temporary.
                writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
                writeln!(asm.code, "\tPUSH, {}", s_addr).unwrap();
                if let Some(conv1) = &pipe.conv1 {
                    writeln!(asm.code, "\tPUSH, {}", conv1.0).unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", pipe.reader).unwrap();
                    // Handle conversion to i32.
                    writeln!(asm.code, "\tPUSH, {}", conv1.0).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", dst).unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", conv1.1).unwrap();
                } else {
                    writeln!(asm.code, "\tPUSH, {}", dst).unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", pipe.reader).unwrap();
                }
                // Signed? well, System.Convert beat us up, so do things this way
                if let Some(bit) = &pipe.signed_conv_high_bit {
                    // tmp1 has served us well, use it again to store the AND result...
                    writeln!(asm.code, "\tPUSH, {}", bit).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", dst).unwrap();
                    writeln!(asm.code, "\tPUSH, _vm_tmp_r1").unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", ext.i32_and).unwrap();
                    // multiply by -1 (this is a very carefully calculated trick, trust me)
                    writeln!(asm.code, "\tPUSH, _vm_tmp_r1").unwrap();
                    writeln!(asm.code, "\tPUSH, {}", constn1).unwrap();
                    writeln!(asm.code, "\tPUSH, _vm_tmp_r1").unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", ext.i32_mul).unwrap();
                    // and OR back into the destination. signed!
                    writeln!(asm.code, "\tPUSH, _vm_tmp_r1").unwrap();
                    writeln!(asm.code, "\tPUSH, {}", dst).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", dst).unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", ext.i32_or).unwrap();
                }
            }
            Sci32Instr::Store {
                rs1,
                rs2,
                kind,
                offset,
            } => {
                let mut s_addr = REGISTERS_R[rs1 as usize].to_string();
                let s_value = REGISTERS_R[rs2 as usize].to_string();
                if offset != 0 {
                    let adj = asm.ensure_i32(offset as i32);
                    if rs1 == 0 {
                        s_addr = adj;
                    } else {
                        writeln!(asm.code, "\tPUSH, {}", s_addr).unwrap();
                        writeln!(asm.code, "\tPUSH, {}", adj).unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_tmp_r1").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext.i32_add).unwrap();
                        s_addr = "_vm_tmp_r1".to_string();
                    }
                }
                match kind {
                    Sci32LSType::Byte => {
                        // WORKAROUND (CRUEL AND UNUSUAL PUNISHMENT EDITION):
                        // AND with 0xFF into temp 2 to make Convert happy
                        // this is about the same op-count as ToBytes,Get,Set and doesn't hurt GC as much
                        // mask so System.Convert doesn't stab us
                        let const_ff = asm.ensure_i32(0xFF);
                        writeln!(asm.code, "\tPUSH, {}", s_value).unwrap();
                        writeln!(asm.code, "\tPUSH, {}", const_ff).unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_tmp_r2").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext.i32_and).unwrap();
                        // convert to u8
                        writeln!(asm.code, "\tPUSH, _vm_tmp_r2").unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_tmp_u8").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext.u8_fromi32).unwrap();
                        // write to target address
                        writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
                        writeln!(asm.code, "\tPUSH, {}", s_addr).unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_tmp_u8").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext.write_byte).unwrap();
                    }
                    Sci32LSType::Half => {
                        // ok, so, we can either:
                        //  ToBytes,Get,Set,Add,Get,Set
                        // or
                        //  And,Conv,ToBytes,CopyTo
                        // or
                        //  *insert absurdly complicated shifting sequence*
                        // the middle option sounds best, tbh.
                        // mask so System.Convert doesn't stab us
                        let const_ffff = asm.ensure_i32(0xFFFF);
                        writeln!(asm.code, "\tPUSH, {}", s_value).unwrap();
                        writeln!(asm.code, "\tPUSH, {}", const_ffff).unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_tmp_r2").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext.i32_and).unwrap();
                        // convert to u16
                        writeln!(asm.code, "\tPUSH, _vm_tmp_r2").unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_tmp_u16").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext.u16_fromi32).unwrap();
                        // convert to bytes
                        writeln!(asm.code, "\tPUSH, _vm_tmp_u16").unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext.tobytes_u16).unwrap();
                        // copy to target address
                        writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
                        writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
                        writeln!(asm.code, "\tPUSH, {}", s_addr).unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext.bytearray_copy).unwrap();
                    }
                    Sci32LSType::Word => {
                        // so Word really should NOT be one of our worst-case scenario situations.
                        // but, uh, it is.
                        // convert to bytes
                        writeln!(asm.code, "\tPUSH, {}", s_value).unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext.tobytes_i32).unwrap();
                        // copy to target address
                        writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
                        writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
                        writeln!(asm.code, "\tPUSH, {}", s_addr).unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext.bytearray_copy).unwrap();
                    }
                }
            }
            Sci32Instr::Branch {
                rs1,
                rs2,
                kind,
                value,
            } => {
                let mut s1 = REGISTERS_R[rs1 as usize].to_string();
                let mut s2 = REGISTERS_R[rs2 as usize].to_string();
                if kind == Sci32BranchType::BLTU || kind == Sci32BranchType::BGEU {
                    // was copy/pasted to SLTU code
                    writeln!(asm.code, "\tPUSH, {}", s1).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", highbit).unwrap();
                    writeln!(asm.code, "\tPUSH, _vm_tmp_r1").unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", ext.i32_xor).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", s2).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", highbit).unwrap();
                    writeln!(asm.code, "\tPUSH, _vm_tmp_r2").unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", ext.i32_xor).unwrap();
                    s1 = "_vm_tmp_r1".to_string();
                    s2 = "_vm_tmp_r2".to_string();
                }
                // Must be inverted.
                let comptype = match kind {
                    Sci32BranchType::BEQ => &ext.i32_neq,
                    Sci32BranchType::BNE => &ext.i32_eq,
                    // We place dummy values here in case we're interpreting data as code.
                    Sci32BranchType::B2 => &ext.i32_neq,
                    Sci32BranchType::B3 => &ext.i32_eq,
                    Sci32BranchType::BLT => &ext.i32_ge,
                    Sci32BranchType::BGE => &ext.i32_lt,
                    // The above conversion will make this work correctly re: signedness.
                    Sci32BranchType::BLTU => &ext.i32_ge,
                    Sci32BranchType::BGEU => &ext.i32_lt,
                };
                writeln!(asm.code, "\tPUSH, {}", s1).unwrap();
                writeln!(asm.code, "\tPUSH, {}", s2).unwrap();
                writeln!(asm.code, "\tPUSH, _vm_tmp_bool").unwrap();
                writeln!(asm.code, "\tEXTERN, {}", comptype).unwrap();
                writeln!(asm.code, "\tPUSH, _vm_tmp_bool").unwrap();
                writeln!(asm.code, "\tJUMP_IF_FALSE, {}", resolve_jump(&img, value)).unwrap();
            }
            Sci32Instr::ALU(alu) => {
                let mut s1 = resolve_alur(&mut asm, alu.s1);
                let mut s2 = resolve_alur(&mut asm, alu.s2);
                let rd = REGISTERS_W[alu.rd as usize];
                let trivial: Option<&str> = match alu.kind {
                    Sci32ALUType::ADD => Some(&ext.i32_add),
                    Sci32ALUType::SUB => Some(&ext.i32_sub),
                    Sci32ALUType::XOR => Some(&ext.i32_xor),
                    Sci32ALUType::OR => Some(&ext.i32_or),
                    Sci32ALUType::AND => Some(&ext.i32_and),
                    Sci32ALUType::SLL => Some(&ext.i32_shl),
                    Sci32ALUType::SRA => Some(&ext.i32_shr),
                    _ => None,
                };
                if let Some(trivial) = trivial {
                    writeln!(asm.code, "\tPUSH, {}", s1).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", s2).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", rd).unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", trivial).unwrap();
                } else {
                    match alu.kind {
                        Sci32ALUType::SRL => {
                            // This one's complicated; must do this unsigned, no cheating.
                            writeln!(asm.code, "\tPUSH, {}", s1).unwrap();
                            writeln!(asm.code, "\tPUSH, _vm_tmp_u1").unwrap();
                            writeln!(asm.code, "\tEXTERN, {}", ext.u32_fromi32).unwrap();

                            writeln!(asm.code, "\tPUSH, _vm_tmp_u1").unwrap();
                            writeln!(asm.code, "\tPUSH, {}", s2).unwrap();
                            writeln!(asm.code, "\tPUSH, _vm_tmp_u1").unwrap();
                            writeln!(asm.code, "\tEXTERN, {}", ext.u32_shr).unwrap();

                            writeln!(asm.code, "\tPUSH, _vm_tmp_u1").unwrap();
                            writeln!(asm.code, "\tPUSH, {}", rd).unwrap();
                            writeln!(asm.code, "\tEXTERN, {}", ext.i32_fromu32).unwrap();
                        }
                        Sci32ALUType::SLT(unsigned) => {
                            // These are *obscenely* complicated. You know, as opposed to the other kinds.
                            if unsigned {
                                // yup, copy/pasted from above
                                writeln!(asm.code, "\tPUSH, {}", s1).unwrap();
                                writeln!(asm.code, "\tPUSH, {}", highbit).unwrap();
                                writeln!(asm.code, "\tPUSH, _vm_tmp_r1").unwrap();
                                writeln!(asm.code, "\tEXTERN, {}", ext.i32_xor).unwrap();
                                writeln!(asm.code, "\tPUSH, {}", s2).unwrap();
                                writeln!(asm.code, "\tPUSH, {}", highbit).unwrap();
                                writeln!(asm.code, "\tPUSH, _vm_tmp_r2").unwrap();
                                writeln!(asm.code, "\tEXTERN, {}", ext.i32_xor).unwrap();
                                s1 = "_vm_tmp_r1".to_string();
                                s2 = "_vm_tmp_r2".to_string();
                            }
                            // alright, now actually evaluate
                            writeln!(asm.code, "\tPUSH, {}", s1).unwrap();
                            writeln!(asm.code, "\tPUSH, {}", s2).unwrap();
                            writeln!(asm.code, "\tPUSH, _vm_tmp_bool").unwrap();
                            writeln!(asm.code, "\tEXTERN, {}", ext.i32_lt).unwrap();
                            writeln!(asm.code, "\tPUSH, _vm_tmp_bool").unwrap();
                            writeln!(asm.code, "\tJUMP_IF_FALSE, _code_{:08X}_0", pc).unwrap();
                            writeln!(asm.code, "_code_{:08X}_1:", pc).unwrap();
                            writeln!(asm.code, "\tPUSH, {}", const1).unwrap();
                            writeln!(asm.code, "\tJUMP, _code_{:08X}_end", pc).unwrap();
                            writeln!(asm.code, "_code_{:08X}_0:", pc).unwrap();
                            writeln!(asm.code, "\tPUSH, {}", const0).unwrap();
                            writeln!(asm.code, "_code_{:08X}_end:", pc).unwrap();
                            writeln!(asm.code, "\tPUSH, {}", rd).unwrap();
                            writeln!(asm.code, "\tCOPY").unwrap();
                        }
                        _ => {
                            writeln!(asm.code, "\tJUMP, 0xFFFFFFFC").unwrap();
                        }
                    }
                }
            }
            // unrecognized, break, etc.
            _ => {
                writeln!(asm.code, "\tJUMP, 0xFFFFFFFC").unwrap();
            }
        }
    }

    print!("{}", asm);
    Ok(())
}
