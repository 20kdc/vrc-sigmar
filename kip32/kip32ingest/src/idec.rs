//! Instruction Decoder
//!
//! Decodes instructions and determines NOPs.

use crate::Kip32Reg;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Sci32LSType {
    // Unsigned flag.
    Byte(bool),
    Half(bool),
    Word,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Sci32BranchType {
    BEQ,
    BNE,
    BLT,
    BGE,
    BLTU,
    BGEU,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Sci32MULHType {
    MULH,
    MULHSU,
    MULHU,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Sci32ALUType {
    ADD,
    SUB,
    SLL,
    /// The boolean represents unsigned.
    SLT(bool),
    XOR,
    SRL,
    SRA,
    OR,
    AND,
    MUL,
    MULH(Sci32MULHType),
    /// Unsigned boolean.
    DIV(bool),
    /// Unsigned boolean.
    REM(bool),
}

impl Sci32ALUType {
    /// Simulates the ALU operation.
    /// If the ALU operation is not supported for simulation, returns None.
    pub fn simulate(&self, a: u32, b: u32) -> Option<u32> {
        match self {
            Self::ADD => Some(a.wrapping_add(b)),
            Self::SUB => Some(a.wrapping_sub(b)),
            Self::SLL => Some(a.wrapping_shl(b)),
            Self::SLT(unsigned) => {
                if *unsigned {
                    if a < b { Some(1) } else { Some(0) }
                } else {
                    if (a as i32) < (b as i32) {
                        Some(1)
                    } else {
                        Some(0)
                    }
                }
            }
            Self::XOR => Some(a ^ b),
            Self::SRL => Some(a.wrapping_shr(b)),
            Self::SRA => Some((a as i32).wrapping_shr(b) as u32),
            Self::OR => Some(a | b),
            Self::AND => Some(a & b),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Sci32ALUSource {
    Immediate(u32),
    /// This will never point to x0; that is read as Immediate.
    Register(Kip32Reg),
}

/// ALU operation.
/// These get a special layer of simplification applied to them.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Sci32ALUOp {
    pub rd: Kip32Reg,
    pub s1: Sci32ALUSource,
    pub s2: Sci32ALUSource,
    pub kind: Sci32ALUType,
}

/// Instructions.
/// Notably, these have been 'de-relativized'; that is, internal computations for PC+ have been resolved already.
/// Some sets to R0 are also omitted, but not all; the relevant factor is based on if it actually makes the backend worse to keep it a separate op.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Sci32Instr {
    /// Unrecognized instruction.
    Unrecognized(u32),
    /// EBREAK, breakpoint instruction.
    /// We'll use a fake EBREAK at the end of the program, at the start of the data segment.
    /// This fake EBREAK lets the environment reasonably synthesize function calls by setting x1 (ra) to point at it.
    EBREAK,
    /// ECALL, syscall instruction.
    ECALL,
    /// If the instruction would be a NOP, it is replaced with this.
    NOP,
    /// This is used to implement AUIPC, LUI, certain kinds of 'MOV-likes', etc.
    SetRegister {
        rd: Kip32Reg,
        value: Sci32ALUSource,
    },
    JumpAndLink {
        rd: Kip32Reg,
        rd_value: u32,
        value: u32,
    },
    JumpAndLinkRegister {
        rd: Kip32Reg,
        rd_value: u32,
        rs1: Kip32Reg,
        offset: u32,
    },
    Load {
        /// Beware: rd = 0 is possible
        rd: Kip32Reg,
        rs1: Kip32Reg,
        kind: Sci32LSType,
        offset: u32,
    },
    Store {
        /// Address
        rs1: Kip32Reg,
        /// Data
        rs2: Kip32Reg,
        kind: Sci32LSType,
        offset: u32,
    },
    Branch {
        rs1: Kip32Reg,
        rs2: Kip32Reg,
        kind: Sci32BranchType,
        value: u32,
    },
    ALU(Sci32ALUOp),
}

/// Number from 0-7 inclusive.
/// Useful for matches.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Funct3r {
    V000,
    V001,
    V010,
    V011,
    V100,
    V101,
    V110,
    V111,
}

impl Funct3r {
    fn from_num(n: u32) -> Funct3r {
        match n & 7 {
            0b000 => Funct3r::V000,
            0b001 => Funct3r::V001,
            0b010 => Funct3r::V010,
            0b011 => Funct3r::V011,
            0b100 => Funct3r::V100,
            0b101 => Funct3r::V101,
            0b110 => Funct3r::V110,
            0b111 => Funct3r::V111,
            _ => unreachable!(),
        }
    }
    fn to_ls(&self) -> Sci32LSType {
        match self {
            Funct3r::V000 => Sci32LSType::Byte(false),
            Funct3r::V001 => Sci32LSType::Half(false),
            Funct3r::V010 => Sci32LSType::Word,
            // In RV64, this manages a 64-bit value.
            Funct3r::V011 => Sci32LSType::Word,
            // unsigned
            Funct3r::V100 => Sci32LSType::Byte(true),
            Funct3r::V101 => Sci32LSType::Half(true),
            Funct3r::V110 => Sci32LSType::Word,
            Funct3r::V111 => Sci32LSType::Word,
        }
    }
    fn to_branch_type(&self) -> Sci32BranchType {
        match self {
            Funct3r::V000 => Sci32BranchType::BEQ,
            Funct3r::V001 => Sci32BranchType::BNE,
            // We place dummy values here in case we're interpreting data as code.
            // Doing this in idec makes it not the backend's problem.
            Funct3r::V010 => Sci32BranchType::BEQ,
            Funct3r::V011 => Sci32BranchType::BNE,
            // Continuing as normal.
            Funct3r::V100 => Sci32BranchType::BLT,
            Funct3r::V101 => Sci32BranchType::BGE,
            Funct3r::V110 => Sci32BranchType::BLTU,
            Funct3r::V111 => Sci32BranchType::BGEU,
        }
    }
}

impl Sci32Instr {
    /// Remaps an ALU instruction to clean up NOPS/etc.
    pub fn from_alu(op: Sci32ALUOp) -> Sci32Instr {
        // If no side-effects are possible from this ALU op, we can NOP it.
        // Notably, the rationale for division by zero not trapping indicates that ALU ops are *never* supposed to have side-effects.
        if op.rd == Kip32Reg::Zero {
            return Sci32Instr::NOP;
        }
        if let Sci32ALUSource::Immediate(i1) = op.s1 {
            if let Sci32ALUSource::Immediate(i2) = op.s2 {
                // (imm, imm)
                if let Some(value) = op.kind.simulate(i1, i2) {
                    // Fuse into an immediate set.
                    return Sci32Instr::SetRegister {
                        rd: op.rd,
                        value: Sci32ALUSource::Immediate(value),
                    };
                }
            } else if i1 == 0 && op.kind == Sci32ALUType::ADD {
                // 0 + X = X
                return Sci32Instr::SetRegister {
                    rd: op.rd,
                    value: op.s2,
                };
            }
        } else if let Sci32ALUSource::Immediate(i2) = op.s2 {
            if i2 == 0 && op.kind == Sci32ALUType::ADD {
                // X + 0 = X
                return Sci32Instr::SetRegister {
                    rd: op.rd,
                    value: op.s1,
                };
            }
        }
        Sci32Instr::ALU(op)
    }
    /// Decodes a RISC-V instruction.
    pub fn decode(pc: u32, ci: u32) -> Sci32Instr {
        // more work than it's worth to not fully decode this field
        let opcode = ci & 0x7F;
        // all decoded opcodes at least require the rd *slot*
        let rd = (ci >> 7) & 0x1F;
        let rs1 = (ci >> 15) & 0x1F;
        let rs2 = (ci >> 20) & 0x1F;
        let funct7r = ci & 0xFE000000;
        let funct3r: Funct3r = Funct3r::from_num((ci & 0x00007000) >> 12);
        if opcode == 0x17 {
            // AUIPC
            let value = pc.wrapping_add(ci & 0xFFFFF000);
            if rd == 0 {
                Sci32Instr::NOP
            } else {
                Sci32Instr::SetRegister {
                    rd: rd.into(),
                    value: Sci32ALUSource::Immediate(value),
                }
            }
        } else if opcode == 0x37 {
            // LUI
            let value = ci & 0xFFFFF000;
            if rd == 0 {
                Sci32Instr::NOP
            } else {
                Sci32Instr::SetRegister {
                    rd: rd.into(),
                    value: Sci32ALUSource::Immediate(value),
                }
            }
        } else if opcode == 0x6F {
            // JAL
            let ofs = ((ci >> 20) & 0x000007FE) | // [10:1]
                ((ci >> 9)        & 0x00000800) | // [11]
                (ci               & 0x000FF000) | // [19:12]
                ((((ci as i32) >> 11) as u32) & 0xFFF00000); // [20]
            let value = pc.wrapping_add(ofs);
            Sci32Instr::JumpAndLink {
                rd: rd.into(),
                rd_value: pc.wrapping_add(4),
                value,
            }
        } else if opcode == 0x0F {
            // FENCE
            return Sci32Instr::NOP;
        } else if opcode == 0x67 {
            // JALR
            let tgt = ci >> 20;
            Sci32Instr::JumpAndLinkRegister {
                rd: rd.into(),
                rd_value: pc.wrapping_add(4),
                rs1: rs1.into(),
                offset: tgt,
            }
        } else if opcode == 0x03 {
            let addr_ofs = ((ci as i32) >> 20) as u32;
            Sci32Instr::Load {
                rd: rd.into(),
                rs1: rs1.into(),
                kind: funct3r.to_ls(),
                offset: addr_ofs,
            }
        } else if opcode == 0x23 {
            let addr_ofs = (((funct7r as i32) >> 20) as u32) | rd;
            Sci32Instr::Store {
                rs1: rs1.into(),
                rs2: rs2.into(),
                kind: funct3r.to_ls(),
                offset: addr_ofs,
            }
        } else if opcode == 0x63 {
            let ofs = (rd & 0x1E) | // [4:1]
                ((rd & 1) << 11) | // [11]
                ((((funct7r as i32) >> 19) as u32) & 0xFFFFF000) | // [31:12]
                ((funct7r >> 20) & 0x000007E0); // [10:5]
            let value = pc.wrapping_add(ofs);
            Sci32Instr::Branch {
                rs1: rs1.into(),
                rs2: rs2.into(),
                kind: funct3r.to_branch_type(),
                value,
            }
        } else if opcode == 0x13 {
            let s1 = if rs1 == 0 {
                Sci32ALUSource::Immediate(0)
            } else {
                Sci32ALUSource::Register(rs1.into())
            };
            let imm = ((ci as i32) >> 20) as u32;
            let (kind, imm_override) = match (funct3r, funct7r) {
                (Funct3r::V000, _) => (Sci32ALUType::ADD, imm),
                // *shift instructions use rs2 not imm*
                (Funct3r::V001, _) => (Sci32ALUType::SLL, rs2),
                (Funct3r::V010, _) => (Sci32ALUType::SLT(false), imm),
                (Funct3r::V011, _) => (Sci32ALUType::SLT(true), imm),
                (Funct3r::V100, _) => (Sci32ALUType::XOR, imm),
                // *shift instructions use rs2 not imm*
                (Funct3r::V101, 0x40000000) => (Sci32ALUType::SRA, rs2),
                (Funct3r::V101, _) => (Sci32ALUType::SRL, rs2),
                (Funct3r::V110, _) => (Sci32ALUType::OR, imm),
                (Funct3r::V111, _) => (Sci32ALUType::AND, imm),
            };
            // Refine.
            Sci32Instr::from_alu(Sci32ALUOp {
                kind,
                rd: rd.into(),
                s1,
                s2: Sci32ALUSource::Immediate(imm_override),
            })
        } else if opcode == 0x33 {
            let kind = match (funct3r, funct7r) {
                // M extension
                (Funct3r::V000, 0x02000000) => Sci32ALUType::MUL,
                (Funct3r::V001, 0x02000000) => Sci32ALUType::MULH(Sci32MULHType::MULH),
                (Funct3r::V010, 0x02000000) => Sci32ALUType::MULH(Sci32MULHType::MULHSU),
                (Funct3r::V011, 0x02000000) => Sci32ALUType::MULH(Sci32MULHType::MULHU),
                (Funct3r::V100, 0x02000000) => Sci32ALUType::DIV(false),
                (Funct3r::V101, 0x02000000) => Sci32ALUType::DIV(true),
                (Funct3r::V110, 0x02000000) => Sci32ALUType::REM(false),
                (Funct3r::V111, 0x02000000) => Sci32ALUType::REM(true),
                // base ISA & fallbacks
                (Funct3r::V000, 0x40000000) => Sci32ALUType::SUB,
                (Funct3r::V000, _) => Sci32ALUType::ADD,
                (Funct3r::V001, _) => Sci32ALUType::SLL,
                (Funct3r::V010, _) => Sci32ALUType::SLT(false),
                (Funct3r::V011, _) => Sci32ALUType::SLT(true),
                (Funct3r::V100, _) => Sci32ALUType::XOR,
                (Funct3r::V101, 0x40000000) => Sci32ALUType::SRA,
                (Funct3r::V101, _) => Sci32ALUType::SRL,
                (Funct3r::V110, _) => Sci32ALUType::OR,
                (Funct3r::V111, _) => Sci32ALUType::AND,
            };
            // Refine.
            let s1 = if rs1 == 0 {
                Sci32ALUSource::Immediate(0)
            } else {
                Sci32ALUSource::Register(rs1.into())
            };
            let s2 = if rs2 == 0 {
                Sci32ALUSource::Immediate(0)
            } else {
                Sci32ALUSource::Register(rs2.into())
            };
            Sci32Instr::from_alu(Sci32ALUOp {
                rd: rd.into(),
                s1,
                s2,
                kind,
            })
        } else if ci == 0x00000073 {
            Sci32Instr::ECALL
        } else if ci == 0x00100073 {
            Sci32Instr::EBREAK
        } else {
            Sci32Instr::Unrecognized(ci)
        }
    }
}
