use anyhow::*;
use base64::prelude::*;
use sci32_ingest::*;
use std::fmt::Write;

mod asmwr;
use asmwr::*;

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
        format!("0xFFFFFFFC ; {:08X}", to)
    }
}

// To implement x0 properly:
// 1. `idec` tries to NOP and generally remove x0 writes as much as possible
// 2. Reads from `x0` are transformed into reads from a constant, while writes to `x0` are transformed into writes to a dummy.
// Registers marked with _ are not exported.
// REGISTERS_W has heap indices autocreated; REGISTERS_R does not.
// Except X0, they should match.
const REGISTERS_W: [&'static str; 32] = [
    "_vm_zero_nopwriteshadow", "vm_ra", "vm_sp", "_vm_x3", "_vm_x4", "_vm_t0", "_vm_t1", "_vm_t2",
    // x8/fp
    "_fp", "_s1",
    // For convenience/sanity, a0-a7 are not marked with any prefix at all.
    "a0", "a1", "a2", "a3", "a4", "a5", // x16
    "a6", "a7", "_vm_s2", "_vm_s3", "_vm_s4", "_vm_s5", "_vm_s6", "_vm_s7",
    // x24
    "_vm_s8", "_vm_s9", "_vm_s10", "_vm_s11", "_vm_t3", "_vm_t4", "_vm_t5", "_vm_t6",
];
// keep in sync!!!
const REGISTERS_R: [&'static str; 32] = [
    "_vm_zero", "vm_ra", "vm_sp", "_vm_x3", "_vm_x4", "_vm_t0", "_vm_t1", "_vm_t2",
    // x8/fp
    "_fp", "_s1",
    // For convenience/sanity, a0-a7 are not marked with any prefix at all.
    "a0", "a1", "a2", "a3", "a4", "a5", // x16
    "a6", "a7", "_vm_s2", "_vm_s3", "_vm_s4", "_vm_s5", "_vm_s6", "_vm_s7",
    // x24
    "_vm_s8", "_vm_s9", "_vm_s10", "_vm_s11", "_vm_t3", "_vm_t4", "_vm_t5", "_vm_t6",
];

fn resolve_alur(asm: &mut UdonAsm, value: Sci32ALUSource) -> String {
    match value {
        Sci32ALUSource::Immediate(v) => {
            asm.ensure_i32(v as i32)
        },
        Sci32ALUSource::Register(rs) => {
            REGISTERS_R[rs as usize].to_string()
        }
    }
}

struct LoadPipe {
    reader: String,
    // (tmpvar, converter)
    convs: Vec<(&'static str, String)>,
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
    asm.declare_heap("_vm_tmp_u8", "SystemByte", "0", false);
    asm.declare_heap("_vm_tmp_i8", "SystemSByte", "0", false);
    asm.declare_heap("_vm_tmp_u16", "SystemUInt16", "0", false);
    asm.declare_heap("_vm_tmp_i16", "SystemInt16", "0", false);
    let highbit = asm.ensure_i32(0x80000000u32 as i32);
    asm.declare_heap("_vm_initdata", "SystemString", &data, false);
    asm.declare_heap("_vm_initdata_dec", "SystemByteArray", "null", false);
    asm.declare_heap("vm_memory", "SystemByteArray", "null", true);
    asm.declare_heap_i32("_vm_initsp", initial_sp as i32, false);
    asm.declare_heap_i32("_vm_abort", abort_vec as i32, false);
    asm.declare_heap_i32("vm_indirect_jump_target", 0, true);

    for i in 0..32 {
        let authentic = REGISTERS_W[i];
        asm.declare_heap_i32(&authentic, 0, authentic.starts_with("_"));
    }
    asm.declare_heap_i32("_vm_zero", 0, false);

    let ext_obj_equality =
        asm.ensure_extern("SystemObject.__op_Equality__SystemObject_SystemObject__SystemBoolean");
    let ext_bytearray_create =
        asm.ensure_extern("SystemByteArray.__ctor__SystemInt32__SystemByteArray");
    let ext_i32_eq =
        asm.ensure_extern("SystemInt32.__op_Equality__SystemInt32_SystemInt32__SystemBoolean");
    let ext_i32_neq =
        asm.ensure_extern("SystemInt32.__op_Inequality__SystemInt32_SystemInt32__SystemBoolean");
    let ext_i32_ge =
        asm.ensure_extern("SystemInt32.__op_GreaterThanOrEqual__SystemInt32_SystemInt32__SystemBoolean");
    let ext_i32_lt =
        asm.ensure_extern("SystemInt32.__op_LessThan__SystemInt32_SystemInt32__SystemBoolean");
    let ext_i32_add =
        asm.ensure_extern("SystemInt32.__op_Addition__SystemInt32_SystemInt32__SystemInt32");
    let ext_i32_sub =
        asm.ensure_extern("SystemInt32.__op_Subtraction__SystemInt32_SystemInt32__SystemInt32");
    let ext_i32_xor =
        asm.ensure_extern("SystemInt32.__op_LogicalXor__SystemInt32_SystemInt32__SystemInt32");
    let ext_i32_or =
        asm.ensure_extern("SystemInt32.__op_LogicalOr__SystemInt32_SystemInt32__SystemInt32");
    let ext_i32_and =
        asm.ensure_extern("SystemInt32.__op_LogicalAnd__SystemInt32_SystemInt32__SystemInt32");
    let ext_i32_shl =
        asm.ensure_extern("SystemInt32.__op_LeftShift__SystemInt32_SystemInt32__SystemInt32");
    let ext_i32_shr =
        asm.ensure_extern("SystemInt32.__op_RightShift__SystemInt32_SystemInt32__SystemInt32");
    // Not a typo.
    let ext_u32_fromi32 = asm.ensure_extern("SystemConvert.__ToUInt32__SystemInt32__SystemUInt32");
    let ext_i32_fromu32 = asm.ensure_extern("SystemConvert.__ToInt32__SystemUInt32__SystemInt32");
    let ext_u32_shr =
        asm.ensure_extern("SystemUInt32.__op_RightShift__SystemUInt32_SystemInt32__SystemUInt32");
    // init, etc.
    let ext_base64_decode = asm.ensure_extern("SystemConvert.__FromBase64String__SystemString__SystemByteArray");
    let ext_bytearray_copy = asm.ensure_extern("SystemByteArray.__CopyTo__SystemArray_SystemInt32__SystemVoid");
    // readers (byte-array, offset)
    let ext_read_i32 = asm.ensure_extern("SystemBitConverter.__ToInt32__SystemByteArray_SystemInt32__SystemInt32");
    let ext_read_u16 = asm.ensure_extern("SystemBitConverter.__ToUInt16__SystemByteArray_SystemInt32__SystemUInt16");
    let ext_read_i16 = asm.ensure_extern("SystemBitConverter.__ToInt16__SystemByteArray_SystemInt32__SystemInt16");
    let ext_read_byte = asm.ensure_extern("SystemByteArray.__Get__SystemInt32__SystemByte");
    // reader unsigned conversions
    let ext_i32_fromu16 = asm.ensure_extern("SystemConvert.__ToInt32__SystemUInt16__SystemInt32");
    let ext_i32_fromi16 = asm.ensure_extern("SystemConvert.__ToInt32__SystemInt16__SystemInt32");
    let ext_i32_fromu8 = asm.ensure_extern("SystemConvert.__ToInt32__SystemByte__SystemInt32");
    // reader signed conversions, stg. 1
    let ext_i8_fromu8 = asm.ensure_extern("SystemConvert.__ToSByte__SystemByte__SystemSByte");
    // reader signed conversions, stg. 2
    let ext_i32_fromi8 = asm.ensure_extern("SystemConvert.__ToInt32__SystemSByte__SystemInt32");
    // writer conversions
    let ext_u8_fromi32 = asm.ensure_extern("SystemConvert.__ToByte__SystemInt32__SystemByte");
    let ext_u16_fromi32 = asm.ensure_extern("SystemConvert.__ToUInt16__SystemInt32__SystemUInt16");
    let ext_tobytes_i32 = asm.ensure_extern("SystemBitConverter.__ToBytes__SystemInt32__SystemByteArray");
    let ext_tobytes_u16 = asm.ensure_extern("SystemBitConverter.__ToBytes__SystemUInt16__SystemByteArray");
    let ext_write_byte = asm.ensure_extern("SystemByteArray.__Set__SystemInt32_SystemByte__SystemVoid");

    let const0 = asm.ensure_i32(0);
    let const1 = asm.ensure_i32(1);

    writeln!(asm.code, "\t; -- JUMP TABLE --").unwrap();
    for i in 0..img.instructions {
        writeln!(asm.code, "\tJUMP, _code_{:08X}", ((i * 4) as u32)).unwrap();
    }
    writeln!(asm.code, "\t; Thunk Abort ({:08X})", abort_vec).unwrap();
    writeln!(asm.code, "\tJUMP, 0xFFFFFFFC").unwrap();
    writeln!(asm.code).unwrap();

    // Indirect jump function
    writeln!(asm.code, ".export _vm_indirect_jump").unwrap();
    writeln!(asm.code, "_vm_indirect_jump:").unwrap();
    // The jump table is laid out so that a simple multiplication will fix up the target.
    writeln!(asm.code, "\tPUSH, vm_indirect_jump_target").unwrap();
    writeln!(asm.code, "\tPUSH, vm_indirect_jump_target").unwrap();
    writeln!(asm.code, "\tPUSH, vm_indirect_jump_target").unwrap();
    writeln!(asm.code, "\tEXTERN, {}", ext_i32_add).unwrap();
    writeln!(asm.code, "\tJUMP_INDIRECT, vm_indirect_jump_target").unwrap();
    writeln!(asm.code).unwrap();

    writeln!(asm.code, "\t; -- THUNKS --").unwrap();
    for sym in img.symbols.values() {
        if !is_udon_safe(&sym.st_name) {
            continue;
        }
        if sym.st_type == sci32_ingest::STT_FUNC || sym.st_type == sci32_ingest::STT_NOTYPE {
            if sym.st_name.starts_with("Udon") {
                writeln!(asm.code, ".export _sym_{}", sym.st_name).unwrap();
                writeln!(asm.code, "_sym_{}:", sym.st_name).unwrap();
                // setup registers
                writeln!(asm.code, "\tPUSH, vm_abort").unwrap();
                writeln!(asm.code, "\tPUSH, vm_ra").unwrap();
                writeln!(asm.code, "\tPUSH, vm_initsp").unwrap();
                writeln!(asm.code, "\tPUSH, vm_sp").unwrap();
                writeln!(asm.code, "\tCOPY").unwrap();
                writeln!(asm.code, "\tCOPY").unwrap();
                // check if the VM has inited. if it has, we jump straight into code
                writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
                writeln!(asm.code, "\tPUSH, _null").unwrap();
                writeln!(asm.code, "\tPUSH, _vm_tmp_bool").unwrap();
                writeln!(asm.code, "\tEXTERN, {}", ext_obj_equality).unwrap();
                writeln!(asm.code, "\tPUSH, _vm_tmp_bool").unwrap();
                writeln!(
                    asm.code,
                    "\tJUMP_IF_FALSE, _code_{:08X}",
                    (sym.st_addr as u32)
                )
                .unwrap();
                // VM has NOT inited. we need to init it
                // start by creating the array
                writeln!(asm.code, "\tPUSH, _vm_initsp").unwrap();
                writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
                writeln!(asm.code, "\tEXTERN, {}", ext_bytearray_create).unwrap();
                // decode the data
                writeln!(asm.code, "\tPUSH, _vm_initdata").unwrap();
                writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
                writeln!(asm.code, "\tEXTERN, {}", ext_base64_decode).unwrap();
                // copy
                writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
                writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
                writeln!(asm.code, "\tPUSH, {}", const0).unwrap();
                writeln!(asm.code, "\tEXTERN, {}", ext_bytearray_copy).unwrap();
                // clean up
                writeln!(asm.code, "\tPUSH, _null").unwrap();
                writeln!(asm.code, "\tPUSH, _vm_initdata").unwrap();
                writeln!(asm.code, "\tPUSH, _null").unwrap();
                writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
                writeln!(asm.code, "\tCOPY").unwrap();
                writeln!(asm.code, "\tCOPY").unwrap();
                // jump
                writeln!(asm.code, "\tJUMP, {}", resolve_jump(&img, sym.st_addr as u32)).unwrap();
            }
        }
    }
    writeln!(asm.code).unwrap();

    writeln!(asm.code).unwrap();
    writeln!(asm.code, "\t; -- CODE --").unwrap();
    for i in 0..img.instructions {
        let istr = Sci32Instr::decode((i * 4) as u32, img.data[i]);
        writeln!(asm.code, "_code_{:08X}: ; {:?}", ((i * 4) as u32), istr).unwrap();
        match istr {
            Sci32Instr::JumpAndLink { rd, rd_value, value } => {
                if rd != 0 {
                    let si = asm.ensure_i32(rd_value as i32);
                    writeln!(asm.code, "\tPUSH, {}", si).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", REGISTERS_W[rd as usize]).unwrap();
                    writeln!(asm.code, "\tCOPY").unwrap();
                }
                writeln!(asm.code, "\tJUMP, {}", resolve_jump(&img, value)).unwrap();
            }
            Sci32Instr::JumpAndLinkRegister { rd, rd_value, rs1, offset } => {
                let si = REGISTERS_R[rs1 as usize].to_string();
                if offset == 0 {
                    writeln!(asm.code, "\tPUSH, {}", si).unwrap();
                    writeln!(asm.code, "\tPUSH, vm_indirect_jump_target").unwrap();
                    writeln!(asm.code, "\tCOPY").unwrap();
                } else {
                    let ov = asm.ensure_i32(offset as i32);
                    writeln!(asm.code, "\tPUSH, {}", si).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", ov).unwrap();
                    writeln!(asm.code, "\tPUSH, vm_indirect_jump_target").unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", ext_i32_add).unwrap();
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
            },
            Sci32Instr::NOP => {
                writeln!(asm.code, "\tNOP").unwrap();
            }
            Sci32Instr::Load { rd, rs1, kind, unsigned, offset } => {
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
                        writeln!(asm.code, "\tEXTERN, {}", ext_i32_add).unwrap();
                        s_addr = "_vm_tmp_r1".to_string();
                    }
                }
                let pipe = match kind {
                    Sci32LSType::Byte => if unsigned {
                        LoadPipe {
                            reader: ext_read_byte.clone(),
                            convs: vec![
                                ("_vm_tmp_u8", ext_i32_fromu8.clone())
                            ]
                        }
                    } else {
                        LoadPipe {
                            reader: ext_read_byte.clone(),
                            convs: vec![
                                ("_vm_tmp_u8", ext_i8_fromu8.clone()),
                                ("_vm_tmp_i8", ext_i32_fromi8.clone())
                            ]
                        }
                    },
                    Sci32LSType::Half => if unsigned {
                        LoadPipe {
                            reader: ext_read_u16.clone(),
                            convs: vec![
                                ("_vm_tmp_u16", ext_i32_fromu16.clone())
                            ]
                        }
                    } else {
                        LoadPipe {
                            reader: ext_read_i16.clone(),
                            convs: vec![
                                ("_vm_tmp_i16", ext_i32_fromi16.clone())
                            ]
                        }
                    },
                    Sci32LSType::Word => LoadPipe {
                        reader: ext_read_i32.clone(),
                        convs: vec![]
                    }
                };
                // Reader. Goes to destination (if no convs) or to conv0 temporary.
                writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
                writeln!(asm.code, "\tPUSH, {}", s_addr).unwrap();
                if pipe.convs.len() == 0 {
                    writeln!(asm.code, "\tPUSH, {}", dst).unwrap();
                } else {
                    writeln!(asm.code, "\tPUSH, {}", pipe.convs[0].0).unwrap();
                }
                writeln!(asm.code, "\tEXTERN, {}", pipe.reader).unwrap();
                // Execute each converter in order.
                for i in 0 .. pipe.convs.len() {
                    let this_conv = &pipe.convs[i];
                    let next_conv = pipe.convs.get(i + 1);
                    writeln!(asm.code, "\tPUSH, {}", this_conv.0).unwrap();
                    if let Some(conv2) = next_conv {
                        writeln!(asm.code, "\tPUSH, {}", conv2.0).unwrap();
                    } else {
                        writeln!(asm.code, "\tPUSH, {}", dst).unwrap();
                    }
                    writeln!(asm.code, "\tEXTERN, {}", this_conv.1).unwrap();
                }
            }
            Sci32Instr::Store { rs1, rs2, kind, offset } => {
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
                        writeln!(asm.code, "\tEXTERN, {}", ext_i32_add).unwrap();
                        s_addr = "_vm_tmp_r1".to_string();
                    }
                }
                match kind {
                    Sci32LSType::Byte => {
                        // convert to u8
                        writeln!(asm.code, "\tPUSH, {}", s_value).unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_tmp_u8").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext_u8_fromi32).unwrap();
                        // write to target address
                        writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
                        writeln!(asm.code, "\tPUSH, {}", s_addr).unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_tmp_u8").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext_write_byte).unwrap();
                    }
                    Sci32LSType::Half => {
                        // convert to u16
                        writeln!(asm.code, "\tPUSH, {}", s_value).unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_tmp_u16").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext_u16_fromi32).unwrap();
                        // convert to bytes
                        writeln!(asm.code, "\tPUSH, _vm_tmp_u16").unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext_tobytes_u16).unwrap();
                        // copy to target address
                        writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
                        writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
                        writeln!(asm.code, "\tPUSH, {}", s_addr).unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext_bytearray_copy).unwrap();
                    }
                    Sci32LSType::Word => {
                        // convert to bytes
                        writeln!(asm.code, "\tPUSH, {}", s_value).unwrap();
                        writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext_tobytes_i32).unwrap();
                        // copy to target address
                        writeln!(asm.code, "\tPUSH, _vm_initdata_dec").unwrap();
                        writeln!(asm.code, "\tPUSH, vm_memory").unwrap();
                        writeln!(asm.code, "\tPUSH, {}", s_addr).unwrap();
                        writeln!(asm.code, "\tEXTERN, {}", ext_bytearray_copy).unwrap();
                    }
                }
            }
            Sci32Instr::Branch { rs1, rs2, kind, value } => {
                let mut s1 = REGISTERS_R[rs1 as usize].to_string();
                let mut s2 = REGISTERS_R[rs2 as usize].to_string();
                if kind == Sci32BranchType::BLTU || kind == Sci32BranchType::BGEU {
                    // was copy/pasted to SLTU code
                    writeln!(asm.code, "\tPUSH, {}", s1).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", highbit).unwrap();
                    writeln!(asm.code, "\tPUSH, _vm_tmp_r1").unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", ext_i32_xor).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", s2).unwrap();
                    writeln!(asm.code, "\tPUSH, {}", highbit).unwrap();
                    writeln!(asm.code, "\tPUSH, _vm_tmp_r2").unwrap();
                    writeln!(asm.code, "\tEXTERN, {}", ext_i32_xor).unwrap();
                    s1 = "_vm_tmp_r1".to_string();
                    s2 = "_vm_tmp_r2".to_string();
                }
                // Must be inverted.
                let comptype = match kind {
                    Sci32BranchType::BEQ => &ext_i32_neq,
                    Sci32BranchType::BNE => &ext_i32_eq,
                    // We place dummy values here in case we're interpreting data as code.
                    Sci32BranchType::B2 => &ext_i32_neq,
                    Sci32BranchType::B3 => &ext_i32_eq,
                    Sci32BranchType::BLT => &ext_i32_ge,
                    Sci32BranchType::BGE => &ext_i32_lt,
                    // The above conversion will make this work correctly re: signedness.
                    Sci32BranchType::BLTU => &ext_i32_ge,
                    Sci32BranchType::BGEU => &ext_i32_lt,
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
                    Sci32ALUType::ADD => Some(&ext_i32_add),
                    Sci32ALUType::SUB => Some(&ext_i32_sub),
                    Sci32ALUType::XOR => Some(&ext_i32_xor),
                    Sci32ALUType::OR => Some(&ext_i32_or),
                    Sci32ALUType::AND => Some(&ext_i32_and),
                    Sci32ALUType::SLL => Some(&ext_i32_shl),
                    Sci32ALUType::SRA => Some(&ext_i32_shr),
                    _ => None
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
                            writeln!(asm.code, "\tEXTERN, {}", ext_u32_fromi32).unwrap();

                            writeln!(asm.code, "\tPUSH, _vm_tmp_u1").unwrap();
                            writeln!(asm.code, "\tPUSH, {}", s2).unwrap();
                            writeln!(asm.code, "\tPUSH, _vm_tmp_u1").unwrap();
                            writeln!(asm.code, "\tEXTERN, {}", ext_u32_shr).unwrap();

                            writeln!(asm.code, "\tPUSH, _vm_tmp_u1").unwrap();
                            writeln!(asm.code, "\tPUSH, {}", rd).unwrap();
                            writeln!(asm.code, "\tEXTERN, {}", ext_i32_fromu32).unwrap();
                        }
                        Sci32ALUType::SLT(unsigned) => {
                            // These are *obscenely* complicated. You know, as opposed to the other kinds.
                            if unsigned {
                                // yup, copy/pasted from above
                                writeln!(asm.code, "\tPUSH, {}", s1).unwrap();
                                writeln!(asm.code, "\tPUSH, {}", highbit).unwrap();
                                writeln!(asm.code, "\tPUSH, _vm_tmp_r1").unwrap();
                                writeln!(asm.code, "\tEXTERN, {}", ext_i32_xor).unwrap();
                                writeln!(asm.code, "\tPUSH, {}", s2).unwrap();
                                writeln!(asm.code, "\tPUSH, {}", highbit).unwrap();
                                writeln!(asm.code, "\tPUSH, _vm_tmp_r2").unwrap();
                                writeln!(asm.code, "\tEXTERN, {}", ext_i32_xor).unwrap();
                                s1 = "_vm_tmp_r1".to_string();
                                s2 = "_vm_tmp_r2".to_string();
                            }
                            // alright, now actually evaluate
                            writeln!(asm.code, "\tPUSH, {}", s1).unwrap();
                            writeln!(asm.code, "\tPUSH, {}", s2).unwrap();
                            writeln!(asm.code, "\tPUSH, _vm_tmp_bool").unwrap();
                            writeln!(asm.code, "\tEXTERN, {}", ext_i32_lt).unwrap();
                            let xd = (i * 4) as u32;
                            writeln!(asm.code, "\tPUSH, _vm_tmp_bool").unwrap();
                            writeln!(asm.code, "\tJUMP_IF_FALSE, _code_{:08X}_0", xd).unwrap();
                            writeln!(asm.code, "_code_{:08X}_1:", xd).unwrap();
                            writeln!(asm.code, "\tPUSH, {}", const1).unwrap();
                            writeln!(asm.code, "\tJUMP, _code_{:08X}_end", xd).unwrap();
                            writeln!(asm.code, "_code_{:08X}_0:", xd).unwrap();
                            writeln!(asm.code, "\tPUSH, {}", const0).unwrap();
                            writeln!(asm.code, "_code_{:08X}_end:", xd).unwrap();
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
