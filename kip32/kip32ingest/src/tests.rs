//! When errors are found in idec, add them to this file so they don't happen again.

use crate::*;
use capstone::prelude::*;

fn do_capstone_disasm(capstone: &Capstone, ci: u32, expected: String) {
    let data_mod = ci.to_le_bytes();
    let disasm = capstone
        .disasm_count(&data_mod, 0, 1)
        .expect("Capstone should decode these.")
        .iter()
        .map(|v| v.to_string())
        .next()
        .expect("There should be an instruction here.");
    assert_eq!(expected, disasm, "{:#x}", ci);
}

fn do_capstone_numfmt(v: u32) -> String {
    let nv = v as i32;
    if nv >= -9 && nv <= 9 {
        return format!("{}", nv);
    }
    if v >= 0x80000000 {
        return format!("-0x{:x}", 0u32.wrapping_sub(v));
    }
    return format!("{:#x}", v as i32);
}

fn do_setup(v: u32) -> (Kip32Split, u32) {
    return (Kip32Split::from(v), v);
}

/// Checks for subtle errors in the isplit immediate decoding by comparing Capstone output.
/// Most idec errors can be caught just with a decent test program (this is what science.c is for), but these are much more subtle.
/// This test is EXCEPTIONALLY BRITTLE and may be worth putting behind a feature flag; you are expected to see failures when Capstone changes.
#[test]
fn idec_imm_test() {
    let capstone = Capstone::new()
        .riscv()
        .mode(arch::riscv::ArchMode::RiscV32)
        .build()
        .expect("Capstone is necessary to run this test");
    for iteration in 0..0x100000u32 {
        let funct7 = (iteration & 0xFE000) >> 13;
        // test 1: LUI
        let (dec, data) = do_setup((iteration << 12) | 0x37);
        do_capstone_disasm(
            &capstone,
            data,
            format!("0x0: lui zero, {}", do_capstone_numfmt(dec.imm_u >> 12)),
        );
        // test 2: JAL
        let (dec, data) = do_setup((iteration << 12) | 0x6F);
        do_capstone_disasm(
            &capstone,
            data,
            format!("0x0: j {}", do_capstone_numfmt(dec.imm_j)),
        );
        // test 3: BEQ
        let (dec, data) = do_setup((funct7 << 25) | ((iteration & 0x1F) << 7) | 0x63);
        do_capstone_disasm(
            &capstone,
            data,
            format!("0x0: beqz zero, {}", do_capstone_numfmt(dec.imm_b)),
        );
        // test 4: SW
        let (dec, data) = do_setup((funct7 << 25) | ((iteration & 0x1F) << 7) | 0x2023);
        do_capstone_disasm(
            &capstone,
            data,
            format!("0x0: sw zero, {}(zero)", do_capstone_numfmt(dec.imm_s)),
        );
        // test 5: ADDI
        let (dec, data) = do_setup((iteration << 20) | 0x80C13);
        // if it is exactly equal, then Capstone sees this as the mv instruction.
        if data != 0x80C13 {
            do_capstone_disasm(
                &capstone,
                data,
                format!("0x0: addi s8, a6, {}", do_capstone_numfmt(dec.imm_i)),
            );
        }
    }
    // panic!("{} iterations", iterations);
}

#[test]
fn idec_checks() {
    //  250:   eddff0ef                jal     12c <phyto_surrounded>
    assert_eq!(
        crate::Sci32Instr::decode(0x250u32, 0xeddff0efu32),
        Sci32Instr::JumpAndLink {
            rd: Kip32Reg::RA,
            rd_value: 0x254u32,
            value: 0x12Cu32
        }
    );
    //   e4:   fee7ae23                sw      a4,-4(a5)
    assert_eq!(
        crate::Sci32Instr::decode(0xe4u32, 0xfee7ae23u32),
        Sci32Instr::Store {
            rs1: Kip32Reg::A5,
            rs2: Kip32Reg::A4,
            kind: Sci32LSType::Word,
            offset: (-4i32) as u32
        }
    )
}
