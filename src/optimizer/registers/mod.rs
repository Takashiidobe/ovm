use std::collections::HashMap;

use super::Instr;

pub const AVAILABLE_REGS: [&str; 6] = ["%r10", "%r11", "%r12", "%r13", "%r14", "%r15"];

#[derive(Debug, Clone)]
pub enum Location {
    Register(String),
    Spill,
}

pub trait RegisterAllocator {
    fn allocate(&self, instrs: &[Instr]) -> (Vec<Instr>, HashMap<String, Location>);
}

pub mod linear_scan;
