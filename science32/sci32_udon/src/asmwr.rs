use std::collections::HashSet;
use std::fmt::Display;
use std::fmt::Write;

/// Udon Assembly program wrapper.
#[derive(Clone, Default)]
pub struct UdonAsm {
    pub data: String,
    pub code: String,
    /// For convenience, integer constants are managed here.
    pub consts_i32: HashSet<i32>,
    /// Externs are managed here.
    pub externs: HashSet<String>,
}

impl Display for UdonAsm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(".data_start\n\n")?;
        f.write_str(&self.data)?;
        f.write_str("\n.data_end\n")?;
        f.write_str(".code_start\n\n")?;
        f.write_str(&self.code)?;
        f.write_str("\n.code_end\n")?;
        Ok(())
    }
}

impl UdonAsm {
    pub fn declare_heap(&mut self, id: &str, ut: &str, ival: &str, export: bool) {
        if export {
            writeln!(self.data, "\t.export {}", id).unwrap();
        }
        writeln!(self.data, "\t{}: %{}, {}", id, ut, ival).unwrap();
    }
    pub fn declare_heap_i32(&mut self, id: &str, ival: i32, export: bool) {
        self.declare_heap(&id, "SystemInt32", &format!("{}", ival), export);
    }
    pub fn declare_heap_u32(&mut self, id: &str, ival: u32, export: bool) {
        self.declare_heap(&id, "SystemUInt32", &format!("{}", ival), export);
    }
    pub fn ensure_i32(&mut self, x: i32) -> String {
        let id = format!("_c_i32_{}", x);
        if self.consts_i32.insert(x) {
            self.declare_heap_i32(&id, x, false);
        }
        id
    }
    pub fn ensure_extern(&mut self, x: &str) -> String {
        let id = format!("_c_extern_{}", x);
        if self.externs.insert(x.to_string()) {
            self.declare_heap(&id, "SystemString", &format!("\"{}\"", x), false);
        }
        id
    }
}
