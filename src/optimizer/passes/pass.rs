use crate::optimizer::Instr;

/// Trait for compiler optimization passes
pub trait Pass {
    /// Apply the optimization pass to the given instructions
    fn optimize(&self, instrs: Vec<Instr>) -> Vec<Instr>;

    fn name(&self) -> &'static str;
}
