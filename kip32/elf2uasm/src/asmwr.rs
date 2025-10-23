#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Display;
use std::fmt::Write;

/// Udon Assembly program wrapper.
#[derive(Clone, Default)]
pub struct UdonAsm {
    pub data: RefCell<String>,
    pub code: RefCell<String>,
    /// For convenience, integer constants are managed here.
    pub consts_i32: RefCell<HashSet<i32>>,
    /// Externs are managed here.
    pub externs: RefCell<HashSet<String>>,
}

impl Display for UdonAsm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(".data_start\n\n")?;
        f.write_str(&self.data.borrow())?;
        f.write_str("\n.data_end\n")?;
        f.write_str(".code_start\n\n")?;
        f.write_str(&self.code.borrow())?;
        f.write_str("\n.code_end\n")?;
        Ok(())
    }
}

impl UdonAsm {
    pub fn declare_heap(&self, id: &str, ut: &str, ival: &str, export: bool) {
        if export {
            writeln!(self.data.borrow_mut(), "\t.export {}", id).unwrap();
        }
        writeln!(self.data.borrow_mut(), "\t{}: %{}, {}", id, ut, ival).unwrap();
    }
    pub fn declare_heap_i32(&self, id: &str, ival: i32, export: bool) {
        self.declare_heap(
            &id,
            "SystemInt32",
            &format!("0x{:08x}", ival as u32),
            export,
        );
    }
    pub fn declare_heap_u32(&self, id: &str, ival: u32, export: bool) {
        self.declare_heap(&id, "SystemUInt32", &format!("0x{:08x}", ival), export);
    }
    pub fn ensure_i32(&self, x: i32) -> String {
        let id = format!("_c_i32_{:08X}", x as u32);
        if self.consts_i32.borrow_mut().insert(x) {
            self.declare_heap_i32(&id, x, false);
        }
        id
    }
    pub fn ensure_extern(&self, x: &str) -> String {
        let id = format!("_c_extern_{}", x.replace(".", "_"));
        if self.externs.borrow_mut().insert(x.to_string()) {
            self.declare_heap(&id, "SystemString", &format!("\"{}\"", x), false);
        }
        id
    }

    // -- Writer Wrapping --

    /// Writes to the code block.
    pub fn code(&self, v: impl std::fmt::Display) {
        writeln!(self.code.borrow_mut(), "{}", v).unwrap();
    }
    /// Writes to the data block.
    pub fn data(&self, v: impl std::fmt::Display) {
        writeln!(self.data.borrow_mut(), "{}", v).unwrap();
    }

    // -- Instructions --

    /// NOP instruction.
    pub fn nop(&self) {
        writeln!(self.code.borrow_mut(), "\tNOP").unwrap();
    }

    /// PUSH instruction.
    pub fn push(&self, index: impl std::fmt::Display) {
        writeln!(self.code.borrow_mut(), "\tPUSH, {}", index).unwrap();
    }

    /// POP instruction.
    pub fn pop(&self) {
        writeln!(self.code.borrow_mut(), "\tPOP").unwrap();
    }

    /// JUMP_IF_FALSE instruction.
    pub fn jump_if_false(&self, target: impl std::fmt::Display) {
        writeln!(self.code.borrow_mut(), "\tJUMP_IF_FALSE, {}", target).unwrap();
    }

    /// JUMP instruction.
    pub fn jump(&self, target: impl std::fmt::Display) {
        writeln!(self.code.borrow_mut(), "\tJUMP, {}", target).unwrap();
    }

    /// EXTERN instruction.
    pub fn ext(&self, index: impl std::fmt::Display) {
        writeln!(self.code.borrow_mut(), "\tEXTERN, {}", index).unwrap();
    }

    /// ANNOTATION instruction.
    pub fn annotation(&self, v: impl std::fmt::Display) {
        writeln!(self.code.borrow_mut(), "\tANNOTATION, {}", v).unwrap();
    }

    /// JUMP_INDIRECT instruction.
    pub fn jump_indirect(&self, index: impl std::fmt::Display) {
        writeln!(self.code.borrow_mut(), "\tJUMP_INDIRECT, {}", index).unwrap();
    }

    /// COPY instruction.
    pub fn copy(&self) {
        writeln!(self.code.borrow_mut(), "\tCOPY").unwrap();
    }

    // -- Idioms --

    pub fn stop(&self) {
        self.code("\tJUMP, 0xFFFFFFFC");
    }

    pub fn copy_static(&self, from: impl std::fmt::Display, to: impl std::fmt::Display) {
        self.push(from);
        self.push(to);
        self.copy();
    }

    pub fn jump_if_false_static(&self, test: impl std::fmt::Display, to: impl std::fmt::Display) {
        self.push(test);
        self.jump_if_false(to);
    }

    pub fn code_label(&self, id: impl std::fmt::Display, export: bool) {
        if export {
            writeln!(self.code.borrow_mut(), ".export {}", id).unwrap();
        }
        writeln!(self.code.borrow_mut(), "{}:", id).unwrap();
    }
}
