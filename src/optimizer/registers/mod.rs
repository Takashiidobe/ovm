pub mod linear_scan;

use crate::optimizer::CFG;
use std::collections::BTreeMap;

pub const AVAILABLE_REGS: [&str; 6] = ["%rbx", "%rbp", "%r12", "%r13", "%r14", "%r15"];

#[derive(Debug, Clone, PartialEq)]
pub enum Location {
    Register(String),
    Spill,
}

pub trait RegisterAllocator {
    fn allocate(&self, instrs: &CFG) -> (CFG, BTreeMap<String, Location>);
}
