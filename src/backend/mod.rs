use std::collections::HashMap;

use crate::optimizer::{Instr, Op, registers::Location};

use x86_64::Codegen;

pub mod x86_64;

pub trait Backend {
    fn generate_assembly(&self, program: &[Instr], reg_map: &HashMap<String, Location>) -> String;
}
