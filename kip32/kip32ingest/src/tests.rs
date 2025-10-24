//! When errors are found in idec, add them to this file so they don't happen again.

use crate::*;

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
