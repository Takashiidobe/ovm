use std::collections::BTreeMap;
use crate::optimizer::{CFG, registers::Location};

pub mod x86_64;

pub trait Backend {
    fn generate_assembly(
        &mut self,
        program: &CFG,
        reg_map: &BTreeMap<String, Location>,
    ) -> String;
}
