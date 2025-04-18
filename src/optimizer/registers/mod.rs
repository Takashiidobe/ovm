pub mod linear_scan;

use super::Instr;
use std::collections::HashMap;

pub const AVAILABLE_REGS: [&str; 6] = ["%rbx", "%rbp", "%r12", "%r13", "%r14", "%r15"];

#[derive(Debug, Clone, PartialEq)]
pub enum Location {
    Register(String),
    Spill,
}

pub trait RegisterAllocator {
    fn allocate(&self, instrs: &[Instr]) -> (Vec<Instr>, HashMap<String, Location>);
}
