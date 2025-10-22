use anyhow::*;
use base64::prelude::*;
use lexopt::prelude::*;
use sci32_ingest::*;
use std::collections::HashMap;

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

fn code_addr(pc: u32, sfx: &str) -> String {
    format!("_code_{:08X}{}", pc, sfx)
}

/// Resolves a fixed jump.
fn resolve_jump(img: &Sci32Image, to: u32) -> String {
    if img.is_instruction_at(to) {
        code_addr(to, "")
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

fn resolve_alur(asm: &UdonAsm, value: Sci32ALUSource) -> String {
    match value {
        Sci32ALUSource::Immediate(v) => asm.ensure_i32(v as i32),
        Sci32ALUSource::Register(rs) => REGISTERS_R[rs as usize].to_string(),
    }
}

// WORKAROUND:
// We're now at the point where the workarounds themselves have more workarounds.
// System.Buffer.BlockCopy unironically saved performance here, because it's the only way to *efficiently* force certain type conversions.
// The project has been through a few different attempts at doing these, and this is the only decently okay one.
// In any case, there are three kinds of load pipeline...
enum LoadPipe {
    /// 1 extern call; use BitConverter directly
    Word,
    /// 3 extern calls; set to 0, BlockCopy, get from word array
    ZeroPad(i32),
    /// 3 extern calls; BlockCopy, get from array, convert
    Convert {
        holding_cell: &'static str,
        type_size: i32,
        holding_cell_element: &'static str,
        array_get: String,
        converter: String,
    },
}

fn main() -> Result<()> {
    let mut arg_parser = lexopt::Parser::from_env();
    let mut asm = UdonAsm::default();
    let mut img = Sci32Image::default();
    let mut out_filename: Option<std::ffi::OsString> = None;
    let mut inc_code: Vec<String> = Vec::new();
    let mut ecall: String = "0xFFFFFFFC".to_string();
    let mut auto_stack: usize = 0x1000;
    while let Some(arg) = arg_parser.next()? {
        match arg {
            Short('?') | Short('h') | Long("help") => {
                println!("rvelf2udon some.elf");
                println!(" meant to be used with 'microcontroller'-like ELFs");
                println!(" section headers are used, not program headers; but no relocations");
                println!(" passing multiple ELF files is 'supported' but probably isn't helpful");
                println!(" special ELF symbols:");
                println!("  _stack_start: initial value of SP");
                println!("                if not found, stack space will be auto-allocated");
                println!("  Udon*: exposed as custom events w/ wrapping code to setup SP and RA");
                println!(" additional options:");
                println!(" --out/-o FILE: output .uasm file (default stdout)");
                println!(" --inc FILE: splice this .uasm into the output");
                println!("             looks for '.(data/code)_(start/end)' on lines to cut it up");
                println!("             these directives must be the *sole* line content,");
                println!("             or they won't be recognized");
                println!("             data is added at the start, code after the vector table.");
                println!(" --ecall SYMBOL: by default, ECALL halts; you can override this");
                println!("                 you can use this to implement logging, sbrk(), etc.");
                println!("                 run 'JUMP, _vm_indirect_jump' to return to VM");
                println!(" --auto-stack WORDS: size of the auto-allocated stack in 32-bit words");
                println!("                     has no effect if _stack_start is found");
                std::process::exit(0);
            }
            Short('o') | Long("out") => match arg_parser.next() {
                Result::Ok(Some(Value(v))) => out_filename = Some(v),
                _ => return Err(anyhow!("-o/--out expects a filename value")),
            },
            Long("ecall") => match arg_parser.next() {
                Result::Ok(Some(Value(v))) => {
                    ecall = v
                        .into_string()
                        .expect("--ecall symbol should be reasonably sane")
                }
                _ => return Err(anyhow!("--ecall expects a symbol")),
            },
            Long("auto-stack") => match arg_parser.next() {
                Result::Ok(Some(Value(v))) => {
                    auto_stack = v.parse().expect("--auto-stack number should parse")
                }
                _ => return Err(anyhow!("--auto-stack expects a number")),
            },
            Long("inc") => match arg_parser.next() {
                Result::Ok(Some(Value(v))) => {
                    let a = std::fs::read_to_string(&v).expect(&format!("{:?} invalid", v));
                    let mut dump_code = false;
                    let mut dump_data = false;
                    for line in a.split("\n") {
                        let line_strip_cr = line.strip_suffix("\r").unwrap_or(line);
                        if line_strip_cr.eq(".data_start") {
                            dump_data = true;
                        } else if line_strip_cr.eq(".data_end") {
                            dump_data = false;
                        } else if line_strip_cr.eq(".code_start") {
                            dump_code = true;
                        } else if line_strip_cr.eq(".code_end") {
                            dump_code = false;
                        } else {
                            if dump_data {
                                asm.data(line_strip_cr);
                            } else if dump_code {
                                inc_code.push(line_strip_cr.to_string());
                            }
                        }
                    }
                }
                _ => return Err(anyhow!("--udon-data expects a filename value")),
            },
            Value(v) => {
                let a = std::fs::read(&v)?;
                img.from_elf(&a).expect(&format!("{:?} parsing", v));
            }
            _ => return Err(arg.unexpected().into()),
        }
    }
    let initial_sp = match img.symbols.get("_stack_start") {
        Some(initial_sp_sym) => initial_sp_sym.st_addr as i32,
        None => {
            // auto stack
            let new_size = img.data.len() + auto_stack;
            img.data.resize(new_size, 0);
            (new_size * 4) as i32
        }
    };
    // So this is a pretty nonsensical value to have here, probably needs explaining.
    // Basically, we calculate indirect jump addresses as vec * 2 to hit the appropriate points in the jump table.
    // And the Udon abort vector is at 0xFFFFFFFC.
    // Therefore, we can shift that to the right by 1 to get a value which both:
    // 1. Doesn't trigger a not-really-a-bug we have with negative jump addresses
    // 2. Conveniently converts into the Udon abort vector
    // We then jump here and it's all AOK.
    let abort_vec = 0x7FFFFFFE;
    let data = BASE64_STANDARD.encode(&img.initialized_bytes());

    asm.declare_heap("_null", "SystemObject", "null", false);
    asm.declare_heap("_vm_tmp_bool", "SystemBoolean", "null", false);
    asm.declare_heap_i32("_vm_tmp_r1", 0, false);
    asm.declare_heap_i32("_vm_tmp_r2", 0, false);
    // WORKAROUND: Due to Udon Assembly bugs, we have to initialize some of these to null rather than 0.
    asm.declare_heap("_vm_tmp_u8", "SystemByte", "null", false);
    asm.declare_heap("_vm_tmp_u16", "SystemUInt16", "null", false);
    asm.declare_heap("_vm_tmp_i8", "SystemSByte", "null", false);
    asm.declare_heap("_vm_tmp_i16", "SystemInt16", "null", false);
    let highbit = asm.ensure_i32(0x80000000u32 as i32);
    asm.declare_heap(
        "_vm_initdata",
        "SystemString",
        &format!("\"{}\"", data),
        false,
    );

    // Scratch buffers for loads, stores, etc.
    asm.declare_heap("_vm_bcopy_i32", "SystemInt32Array", "null", false);
    asm.declare_heap("_vm_bcopy_i16", "SystemInt16Array", "null", false);
    asm.declare_heap("_vm_bcopy_i8", "SystemSByteArray", "null", false);

    asm.declare_heap("_vm_initdata_dec", "SystemByteArray", "null", false);
    // WORKAROUND: Udon has a habit of deciding it's gonna initialize vm_memory ITSELF.
    // Obviously this is bad for us, so we hold a second, hidden field of type Object.
    // It shouldn't initialize *that* behind our backs.
    asm.declare_heap("_vm_memory_chk", "SystemObject", "null", false);
    asm.declare_heap("vm_memory", "SystemByteArray", "null", true);
    // Declared public, but only initialized with memory.
    asm.declare_heap_i32("vm_initsp", 0, true);
    let const_initsp = asm.ensure_i32(initial_sp);
    asm.declare_heap_i32("vm_initra", 0, true);
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

    asm.code("\t# -- JUMP TABLE (MUST BE AT START OF CODE) --");
    for i in 0..img.instructions {
        let pc = (i * 4) as u32;
        asm.jump(code_addr(pc, ""));
    }
    asm.code("");

    asm.code("\t# -- USER --");
    for line in inc_code {
        asm.code(line);
    }
    asm.code("");

    asm.code("\t# -- MEMORY INIT / RESET / INDIRECT JUMP --");

    asm.code_label("_vm_reset", true);
    // The external reset just sets us up to indirect jump straight to abort.
    // This means you can just poke _vm_reset and get a nicely reset VM.
    ext.u32_fromi32(&asm, &const_abort, "vm_indirect_jump_target");

    // FALLTHROUGH: we've just setup the indirect jump to abort, so do a reset-and-jump

    asm.code_label("_vm_reset_and_jump", true);
    // setup VM settings to match the binary.
    asm.copy_static(&const_initsp, "vm_initsp");
    asm.copy_static(&const_abort, "vm_initra");
    // create scratch buffers
    ext.i32array_create(&asm, &const1, "_vm_bcopy_i32");
    ext.i16array_create(&asm, &const1, "_vm_bcopy_i16");
    ext.i8array_create(&asm, &const1, "_vm_bcopy_i8");
    // setup like a thunk would. we're called by thunks when the machine hasn't been setup yet
    // and we can also be called by external code that won't know what the initsp/abort values are yet
    asm.copy_static("vm_initsp", "vm_sp");
    asm.copy_static("vm_initra", "vm_ra");
    // create the array & copy to check field so we don't do this twice
    ext.u8array_create(
        &asm,
        asm.ensure_i32((img.data.len() * 4) as i32),
        "vm_memory",
    );
    asm.copy_static("vm_memory", "_vm_memory_chk");
    // decode the data and copy it into the start of the memory array
    ext.base64_decode(&asm, "_vm_initdata", "_vm_initdata_dec");
    ext.bytearray_copy(&asm, "_vm_initdata_dec", "vm_memory", &const0);
    // clean up
    asm.copy_static("_null", "_vm_initdata_dec");

    // FALLTHROUGH: we now move right on to performing an indirect jump

    asm.code_label("_vm_indirect_jump", true);
    // The jump table is laid out so that a simple multiplication by 2 will fix up the target.
    ext.u32_add(
        &asm,
        "vm_indirect_jump_target",
        "vm_indirect_jump_target",
        "vm_indirect_jump_target",
    );
    asm.jump_indirect("vm_indirect_jump_target");
    asm.code("");

    let mut symbol_marking: HashMap<u32, String> = HashMap::new();
    asm.code("\t# -- THUNKS --");
    for sym in img.symbols.values() {
        if !is_udon_safe(&sym.st_name) {
            continue;
        }
        symbol_marking.insert(sym.st_addr, sym.st_name.clone());
        // if the symbol starts with "Udon", assume this is code we want to be able to call from Udon.
        if sym.st_name.starts_with("Udon") {
            let cut_name = &sym.st_name[4..];
            let fastpath_name = format!("_thunk_{}_fastpath", cut_name);

            asm.code_label(cut_name, true);
            // check if the VM has inited; if it has, we speedrun init
            ext.obj_equality(&asm, "_vm_memory_chk", "_null", "_vm_tmp_bool");
            asm.jump_if_false_static("_vm_tmp_bool", &fastpath_name);

            // Slowpath: The machine hasn't been setup yet!
            // Set indirect jump target and then run Reset-And-Jump.
            ext.u32_fromi32(
                &asm,
                asm.ensure_i32(sym.st_addr as i32),
                "vm_indirect_jump_target",
            );
            asm.jump("_vm_reset_and_jump");

            // Fastpath: Machine is ready. Copy registers and directly jump into code.
            asm.code_label(fastpath_name, false);
            // Setup registers...
            asm.copy_static("vm_initsp", "vm_sp");
            asm.copy_static("vm_initra", "vm_ra");
            // Jump directly into the code.
            asm.jump(resolve_jump(&img, sym.st_addr as u32));
        }
    }
    asm.code("");

    // ClientSim UI shows symbols in declaration order (presumably Udon preserves this somehow and we're seeing the results)
    // For this reason, make sure that symbol getters are written after the actual important stuff
    asm.code("\t# -- SYMGET --");
    for sym in img.symbols.values() {
        if !is_udon_safe(&sym.st_name) {
            continue;
        }
        // either way, export as a 'data symbol'
        asm.code_label(format!("_sym_{}", sym.st_name), true);
        // just set this now
        asm.copy_static(asm.ensure_i32(sym.st_addr as i32), "a0");

        // if the VM hasn't inited, we should probably init it
        ext.obj_equality(&asm, "_vm_memory_chk", "_null", "_vm_tmp_bool");
        // if it's not null, the machine already inited and we can stop now.
        asm.jump_if_false_static("_vm_tmp_bool", "0xFFFFFFFC");
        // jump to _vm_reset, which sets the indirect jump target to the true abort vector.
        asm.jump("_vm_reset");
    }
    asm.code("");

    asm.code("\t# -- CODE --");
    for i in 0..img.instructions {
        let pc = (i * 4) as u32;
        if let Some(sym) = symbol_marking.get(&pc) {
            asm.code("");
            asm.code(format!("# SYMBOL: {}", sym));
        }
        let istr = Sci32Instr::decode(pc, img.data[i]);
        asm.code(format!("_code_{:08X}: # {:?}", pc, istr));
        match istr {
            Sci32Instr::JumpAndLink {
                rd,
                rd_value,
                value,
            } => {
                if rd != 0 {
                    asm.copy_static(asm.ensure_i32(rd_value as i32), REGISTERS_W[rd as usize]);
                }
                asm.jump(resolve_jump(&img, value));
            }
            Sci32Instr::JumpAndLinkRegister {
                rd,
                rd_value,
                rs1,
                offset,
            } => {
                let si = REGISTERS_R[rs1 as usize].to_string();
                if offset == 0 {
                    ext.u32_fromi32(&asm, si, "vm_indirect_jump_target");
                } else {
                    ext.i32_add(&asm, si, asm.ensure_i32(offset as i32), "_vm_tmp_r1");
                    ext.u32_fromi32(&asm, "_vm_tmp_r1", "vm_indirect_jump_target");
                }
                if rd != 0 {
                    asm.copy_static(asm.ensure_i32(rd_value as i32), REGISTERS_W[rd as usize]);
                }
                asm.jump("_vm_indirect_jump");
            }
            Sci32Instr::SetRegister { rd, value } => {
                asm.copy_static(resolve_alur(&asm, value), REGISTERS_W[rd as usize]);
            }
            Sci32Instr::NOP => {
                asm.nop();
            }
            Sci32Instr::Load {
                rd,
                rs1,
                kind,
                offset,
            } => {
                let mut s_addr = REGISTERS_R[rs1 as usize].to_string();
                let dst = REGISTERS_W[rd as usize].to_string();
                if offset != 0 {
                    let adj = asm.ensure_i32(offset as i32);
                    if rs1 == 0 {
                        s_addr = adj;
                    } else {
                        ext.i32_add(&asm, s_addr, adj, "_vm_tmp_r1");
                        s_addr = "_vm_tmp_r1".to_string();
                    }
                }
                let pipe = match kind {
                    Sci32LSType::Word => LoadPipe::Word,
                    Sci32LSType::Byte(true) => LoadPipe::ZeroPad(1),
                    Sci32LSType::Half(true) => LoadPipe::ZeroPad(2),
                    Sci32LSType::Byte(false) => LoadPipe::Convert {
                        holding_cell: "_vm_bcopy_i8",
                        type_size: 1,
                        holding_cell_element: "_vm_tmp_i8",
                        array_get: ext.i8array_get.clone(),
                        converter: ext.i32_fromi8.clone(),
                    },
                    Sci32LSType::Half(false) => LoadPipe::Convert {
                        holding_cell: "_vm_bcopy_i16",
                        type_size: 2,
                        holding_cell_element: "_vm_tmp_i16",
                        array_get: ext.i16array_get.clone(),
                        converter: ext.i32_fromi16.clone(),
                    },
                };
                match pipe {
                    LoadPipe::Word => ext.read_i32(&asm, "vm_memory", s_addr, dst),
                    LoadPipe::ZeroPad(size) => {
                        // Initialize the target to 0 (the zero-padding).
                        ext.i32array_set(&asm, "_vm_bcopy_i32", &const0, &const0);
                        // Copy in the starting bytes (which are also the low bytes aka what we want).
                        ext.bcopy(
                            &asm,
                            "vm_memory",
                            s_addr,
                            "_vm_bcopy_i32",
                            &const0,
                            asm.ensure_i32(size),
                        );
                        // Retrieve the result, which has just been zero-padded.
                        ext.i32array_get(&asm, "_vm_bcopy_i32", &const0, dst);
                    }
                    LoadPipe::Convert {
                        holding_cell,
                        type_size,
                        holding_cell_element,
                        array_get,
                        converter,
                    } => {
                        // Copy in the target.
                        ext.bcopy(
                            &asm,
                            "vm_memory",
                            s_addr,
                            holding_cell,
                            &const0,
                            asm.ensure_i32(type_size),
                        );
                        // Pull from array.
                        asm.push(holding_cell);
                        asm.push(&const0);
                        asm.push(holding_cell_element);
                        asm.ext(&array_get);
                        // Convert.
                        asm.push(holding_cell_element);
                        asm.push(dst);
                        asm.ext(&converter);
                    }
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
                        ext.i32_add(&asm, s_addr, adj, "_vm_tmp_r1");
                        s_addr = "_vm_tmp_r1".to_string();
                    }
                }
                // New writer works like this:
                // Register -> Scratch Buffer -> Destination
                // This is a constant 2 externs, which was the best-case number before.
                // Code is also obviously much cleaner.
                let length = match kind {
                    Sci32LSType::Byte(_) => 1,
                    Sci32LSType::Half(_) => 2,
                    Sci32LSType::Word => 4,
                };
                ext.i32array_set(&asm, "_vm_bcopy_i32", &const0, s_value);
                ext.bcopy(
                    &asm,
                    "_vm_bcopy_i32",
                    &const0,
                    "vm_memory",
                    &s_addr,
                    asm.ensure_i32(length),
                );
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
                    ext.i32_xor(&asm, s1, &highbit, "_vm_tmp_r1");
                    ext.i32_xor(&asm, s2, &highbit, "_vm_tmp_r2");
                    s1 = "_vm_tmp_r1".to_string();
                    s2 = "_vm_tmp_r2".to_string();
                }
                // Must be inverted.
                let comptype = match kind {
                    Sci32BranchType::BEQ => &ext.i32_neq,
                    Sci32BranchType::BNE => &ext.i32_eq,
                    Sci32BranchType::BLT => &ext.i32_ge,
                    Sci32BranchType::BGE => &ext.i32_lt,
                    // The above conversion will make this work correctly re: signedness.
                    Sci32BranchType::BLTU => &ext.i32_ge,
                    Sci32BranchType::BGEU => &ext.i32_lt,
                };
                asm.push(s1);
                asm.push(s2);
                asm.push("_vm_tmp_bool");
                asm.ext(comptype);
                asm.jump_if_false_static("_vm_tmp_bool", resolve_jump(&img, value));
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
                    _ => None,
                };
                if let Some(trivial) = trivial {
                    asm.push(s1);
                    asm.push(s2);
                    asm.push(rd);
                    asm.ext(trivial);
                } else {
                    match alu.kind {
                        // PERFORMANCE TRICK: So to make this performant, we really have to push the envelope on how much we abuse quirks.
                        // RISC-V wants us to AND off the upper bits of the shift amount.
                        // Luckily, C# will also do this, as evidenced by entering `-2 >> 32` and `-2 << 32` into `csi`.
                        // Therefore, for the simplest cases, SRA and SLL, we can consider those done trivially.
                        // We implement them here for clear reference.
                        Sci32ALUType::SRA => {
                            ext.i32_shr(&asm, s1, s2, rd);
                        }
                        Sci32ALUType::SLL => {
                            ext.i32_shl(&asm, s1, s2, rd);
                        }
                        // WORKAROUND: SRL, however, is COMPLICATED.
                        // Really, really complicated.
                        // We can't just convert to unsigned U32 because System.Convert hates us, and we can't get it back cleanly either.
                        // (We could use memory tricks not perfected yet at the time of writing, but I want to get rewrite 1 done first before I do rewrite 2.)
                        // The good news is that for a constant shift, we can calculate a mask and simply apply it afterwards.
                        // Two-extern job, even smaller than the U32 code actually.
                        // The bad news is that for something that isn't a constant shift, we're now down one mask.
                        // Looking over the options, the course is clear - we SRA down 0x80000000 by the same shift amount.
                        // The maximum shift is 31, so that extra bit won't get lost; we have a clean set of 32 unique values from the original down to 0xFFFFFFFF.
                        // We then shift it up by 1, taking the mask from 1-32 set upper bits to 0-31, covering solely the area that may have been sign-extended.
                        // We can now proceed to invert the mask and use it.
                        // (We also do this whole mess in immediate calcs.)
                        Sci32ALUType::SRL => {
                            ext.i32_shr(&asm, s1, &s2, "_vm_tmp_r1");
                            let mask_src = if let Sci32ALUSource::Immediate(s2v) = alu.s2 {
                                let mut mask: i32 = 0x80000000u32 as i32;
                                mask >>= s2v & 0x1F;
                                mask <<= 1;
                                mask ^= -1;
                                asm.ensure_i32(mask)
                            } else {
                                ext.i32_and(&asm, &s2, asm.ensure_i32(0x1F), "_vm_tmp_r2");
                                ext.i32_shr(
                                    &asm,
                                    asm.ensure_i32(0x80000000u32 as i32),
                                    "_vm_tmp_r2",
                                    "_vm_tmp_r2",
                                );
                                ext.i32_shl(&asm, "_vm_tmp_r2", &const1, "_vm_tmp_r2");
                                ext.i32_xor(&asm, "_vm_tmp_r2", &constn1, "_vm_tmp_r2");
                                "_vm_tmp_r2".to_string()
                            };
                            ext.i32_and(&asm, "_vm_tmp_r1", mask_src, rd);
                        }

                        // SLT is messy. My guess is that it exists to implement certain C operations in a completely branchless manner.
                        // (Consider `int something = a == b;`.)
                        // For example, you can synthesize != as (0 SLTU (A ^ B)) without branching.
                        // As it so happens, System.Convert has an operation which mirrors this, so it can be implemented relatively sanely.
                        Sci32ALUType::SLT(unsigned) => {
                            // These are *obscenely* complicated. You know, as opposed to the shifts...
                            if unsigned {
                                // yup, copy/pasted from above
                                ext.i32_xor(&asm, s1, &highbit, "_vm_tmp_r1");
                                ext.i32_xor(&asm, s2, &highbit, "_vm_tmp_r2");
                                s1 = "_vm_tmp_r1".to_string();
                                s2 = "_vm_tmp_r2".to_string();
                            }
                            // alright, now actually evaluate
                            ext.i32_lt(&asm, s1, s2, "_vm_tmp_bool");
                            ext.i32_frombool(&asm, "_vm_tmp_bool", rd);
                        }
                        _ => {
                            asm.stop();
                        }
                    }
                }
            }
            Sci32Instr::ECALL => {
                // This is for the sake of custom ECALL handlers.
                // This allows them to return back to normal program execution just using _vm_indirect_jump.
                ext.u32_fromi32(
                    &asm,
                    asm.ensure_i32((pc + 4) as i32),
                    "vm_indirect_jump_target",
                );
                asm.jump(&ecall);
            }
            // unrecognized, break, etc.
            _ => {
                asm.stop();
            }
        }
    }

    if let Some(out_filename) = out_filename {
        std::fs::write(out_filename, asm.to_string())?;
    } else {
        print!("{}", asm);
    }
    Ok(())
}
