use std::collections::HashMap;

use crate::optimizer::{Instr, registers::Location};

pub mod x86_64;

pub trait Backend {
    fn generate_assembly(&self, program: &[Instr], reg_map: &HashMap<String, Location>) -> String;
}
