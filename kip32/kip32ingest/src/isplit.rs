//! Instruction Splitter
//! This is used to split fields out of instructions.
//! It also sign-extends immediates.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Kip32Split {
    // R-type fields
    pub funct7: u32,
    pub rs2: u32,
    pub rs1: u32,
    pub funct3: u32,
    pub rd: u32,
    pub opcode: u32,
    // Immediate types
    pub imm_i: u32,
    pub imm_s: u32,
    pub imm_b: u32,
    pub imm_u: u32,
    pub imm_j: u32,
}

fn sign_extend(x: u32, bits: u32) -> u32 {
    let mask = 0xFFFFFFFFu32 << (bits - 1);
    if (x & mask) != 0 { x | mask } else { x }
}

impl From<u32> for Kip32Split {
    fn from(ci: u32) -> Self {
        let opcode = ci & 0x7F;
        let rd = (ci >> 7) & 0x1F;
        let rs1 = (ci >> 15) & 0x1F;
        let rs2 = (ci >> 20) & 0x1F;
        let funct7 = (ci & 0xFE000000) >> 25;
        let funct3 = (ci & 0x00007000) >> 12;
        // these two are a mess and are imported from the previous pre-split code
        // they are dangerously easy to break
        let imm_j = ((ci >> 20) & 0x000007FE) | // [10:1]
            ((ci >> 9)        & 0x00000800) | // [11]
            (ci               & 0x000FF000) | // [19:12]
            ((((ci as i32) >> 11) as u32) & 0xFFF00000); // [20]
        // ...
        let imm_b = (rd & 0x1E) | // [4:1]
            ((rd & 1) << 11) | // [11]
            sign_extend((ci & 0x80000000u32) >> 19, 13) | // [31:12]
            ((ci >> 20) & 0x000007E0); // [10:5]
        Kip32Split {
            funct7,
            rs2,
            rs1,
            funct3,
            rd,
            opcode,
            imm_i: sign_extend((funct7 << 5) | rs2, 12),
            imm_s: sign_extend((funct7 << 5) | rd, 12),
            imm_b,
            imm_u: ci & 0xFFFFF000,
            imm_j,
        }
    }
}
