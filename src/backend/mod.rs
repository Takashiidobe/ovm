use crate::optimizer::Instr;

pub mod x86_64;

pub trait Backend {
    fn generate_assembly(&self, program: &[Instr]) -> String;
}
