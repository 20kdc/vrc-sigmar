//! Instruction Decoder
//!
//! Decodes instructions and determines NOPs.

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Sci32LSType {
    Byte,
    Half,
    Word,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Sci32BranchType {
    BEQ,
    BNE,
    /// Not in spec.
    B2,
    /// Still not in spec.
    B3,
    BLT,
    BGE,
    BLTU,
    BGEU,
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
}

impl Sci32ALUType {
    /// Simulates the ALU operation.
    /// If an exception would be thrown, returns None.
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
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Sci32ALUSource {
    Immediate(u32),
    /// This will never point to x0; that is read as Immediate.
    Register(u32),
}

/// ALU operation.
/// These get a special layer of simplification applied to them.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Sci32ALUOp {
    pub rd: u32,
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
        rd: u32,
        value: Sci32ALUSource,
    },
    JumpAndLink {
        rd: u32,
        rd_value: u32,
        value: u32,
    },
    JumpAndLinkRegister {
        rd: u32,
        rd_value: u32,
        rs1: u32,
        offset: u32,
    },
    Load {
        /// Beware: rd = 0 is possible
        rd: u32,
        rs1: u32,
        kind: Sci32LSType,
        unsigned: bool,
        offset: u32,
    },
    Store {
        /// Address
        rs1: u32,
        /// Data
        rs2: u32,
        kind: Sci32LSType,
        offset: u32,
    },
    Branch {
        rs1: u32,
        rs2: u32,
        kind: Sci32BranchType,
        value: u32,
    },
    ALU(Sci32ALUOp),
}

impl Sci32Instr {
    /// Remaps an ALU instruction to clean up NOPS/etc.
    pub fn from_alu(op: Sci32ALUOp) -> Sci32Instr {
        // If no side-effects are possible from this ALU op, we can NOP it.
        if op.rd == 0 {
            return Sci32Instr::NOP;
        }
        if let Sci32ALUSource::Immediate(i1) = op.s1 {
            if let Sci32ALUSource::Immediate(i2) = op.s2 {
                // (imm, imm)
                if let Some(value) = op.kind.simulate(i1, i2) {
                    // Special case: If a side-effect is possible, but cannot possibly occur, we can still NOP it.
                    if op.rd == 0 {
                        return Sci32Instr::NOP;
                    }
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
        let funct3r = ci & 0x00007000;
        if opcode == 0x17 {
            // AUIPC
            let value = pc.wrapping_add(ci & 0xFFFFF000);
            if rd == 0 {
                Sci32Instr::NOP
            } else {
                Sci32Instr::SetRegister {
                    rd,
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
                    rd,
                    value: Sci32ALUSource::Immediate(value),
                }
            }
        } else if opcode == 0x6F {
            // JAL
            let ofs = ((ci >> 20) & 0x000007FE) | // [10:1]
                ((ci >> 9)        & 0x00000800) | // [11]
                (ci               & 0x000FF000) | // [19:12]
                ((ci >> 11)       & 0xFFF00000); // [20]
            let value = pc.wrapping_add(ofs);
            Sci32Instr::JumpAndLink {
                rd,
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
                rd,
                rd_value: pc.wrapping_add(4),
                rs1,
                offset: tgt,
            }
        } else if opcode == 0x03 {
            let addr_ofs = ci >> 20;
            let kind = match funct3r & 0x00003000 {
                0x0000 => Sci32LSType::Byte,
                0x1000 => Sci32LSType::Half,
                _ => Sci32LSType::Word,
            };
            // if not unsigned, do sign extension
            let unsigned = (funct3r & 0x00004000) != 0;
            Sci32Instr::Load {
                rd,
                rs1,
                kind,
                unsigned,
                offset: addr_ofs,
            }
        } else if opcode == 0x23 {
            let addr_ofs = (funct7r >> 20) | rd;
            let kind = match funct3r & 0x00003000 {
                0x0000 => Sci32LSType::Byte,
                0x1000 => Sci32LSType::Half,
                _ => Sci32LSType::Word,
            };
            Sci32Instr::Store {
                rs1,
                rs2,
                kind,
                offset: addr_ofs,
            }
        } else if opcode == 0x63 {
            let kind = match funct3r {
                0x00000000 => Sci32BranchType::BEQ,
                0x00001000 => Sci32BranchType::BNE,
                0x00002000 => Sci32BranchType::B2,
                0x00003000 => Sci32BranchType::B3,
                0x00004000 => Sci32BranchType::BLT,
                0x00005000 => Sci32BranchType::BGE,
                0x00006000 => Sci32BranchType::BLTU,
                0x00007000 => Sci32BranchType::BGEU,
                _ => unreachable!(),
            };
            let ofs = (rd & 0x1E) | // [4:1]
                ((rd & 1) << 11) | // [11]
                ((((funct7r as i32) >> 19) as u32) & 0xFFFFF000) | // [31:12]
                ((funct7r >> 20) & 0x000007E0); // [10:5]
            let value = pc.wrapping_add(ofs);
            Sci32Instr::Branch {
                rs1,
                rs2,
                kind,
                value,
            }
        } else if opcode == 0x13 {
            let s1 = if rs1 == 0 {
                Sci32ALUSource::Immediate(0)
            } else {
                Sci32ALUSource::Register(rs1)
            };
            let rv2 = ((ci as i32) >> 20) as u32;
            let dec1 = if funct3r == 0x00000000 {
                Sci32ALUOp {
                    kind: Sci32ALUType::ADD,
                    rd,
                    s1,
                    s2: Sci32ALUSource::Immediate(rv2),
                }
            } else if funct3r == 0x00001000 {
                // *shift instructions use rs2 not rv2*
                Sci32ALUOp {
                    kind: Sci32ALUType::SLL,
                    rd,
                    s1,
                    s2: Sci32ALUSource::Immediate(rs2),
                }
            } else if funct3r == 0x00002000 {
                Sci32ALUOp {
                    kind: Sci32ALUType::SLT(false),
                    rd,
                    s1,
                    s2: Sci32ALUSource::Immediate(rv2),
                }
            } else if funct3r == 0x00003000 {
                Sci32ALUOp {
                    kind: Sci32ALUType::SLT(true),
                    rd,
                    s1,
                    s2: Sci32ALUSource::Immediate(rv2),
                }
            } else if funct3r == 0x00004000 {
                Sci32ALUOp {
                    kind: Sci32ALUType::XOR,
                    rd,
                    s1,
                    s2: Sci32ALUSource::Immediate(rv2),
                }
            } else if funct3r == 0x00005000 {
                // *shift instructions use rs2 not rv2*
                if funct7r == 0x40000000 {
                    Sci32ALUOp {
                        kind: Sci32ALUType::SRA,
                        rd,
                        s1,
                        s2: Sci32ALUSource::Immediate(rs2),
                    }
                } else {
                    Sci32ALUOp {
                        kind: Sci32ALUType::SRL,
                        rd,
                        s1,
                        s2: Sci32ALUSource::Immediate(rs2),
                    }
                }
            } else if funct3r == 0x00006000 {
                Sci32ALUOp {
                    kind: Sci32ALUType::OR,
                    rd,
                    s1,
                    s2: Sci32ALUSource::Immediate(rv2),
                }
            } else if funct3r == 0x00007000 {
                Sci32ALUOp {
                    kind: Sci32ALUType::AND,
                    rd,
                    s1,
                    s2: Sci32ALUSource::Immediate(rv2),
                }
            } else {
                unreachable!();
            };
            // Refine.
            Sci32Instr::from_alu(dec1)
        } else if opcode == 0x33 {
            let kind = if funct3r == 0x00000000 {
                if funct7r == 0x40000000 {
                    Sci32ALUType::SUB
                } else {
                    Sci32ALUType::ADD
                }
            } else if funct3r == 0x00001000 {
                Sci32ALUType::SLL
            } else if funct3r == 0x00002000 {
                Sci32ALUType::SLT(false)
            } else if funct3r == 0x00003000 {
                Sci32ALUType::SLT(true)
            } else if funct3r == 0x00004000 {
                Sci32ALUType::XOR
            } else if funct3r == 0x00005000 {
                if funct7r == 0x40000000 {
                    Sci32ALUType::SRA
                } else {
                    Sci32ALUType::SRL
                }
            } else if funct3r == 0x00006000 {
                Sci32ALUType::OR
            } else if funct3r == 0x00007000 {
                Sci32ALUType::AND
            } else {
                unreachable!();
            };
            // Refine.
            let s1 = if rs1 == 0 {
                Sci32ALUSource::Immediate(0)
            } else {
                Sci32ALUSource::Register(rs1)
            };
            let s2 = if rs2 == 0 {
                Sci32ALUSource::Immediate(0)
            } else {
                Sci32ALUSource::Register(rs2)
            };
            Sci32Instr::from_alu(Sci32ALUOp { rd, s1, s2, kind })
        } else if ci == 0x00000073 {
            Sci32Instr::ECALL
        } else if ci == 0x00100073 {
            Sci32Instr::EBREAK
        } else {
            Sci32Instr::Unrecognized(ci)
        }
    }
}
