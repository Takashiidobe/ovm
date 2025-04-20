use crate::optimizer::CFG;

pub trait Pass {
    fn optimize(&self, instrs: CFG) -> CFG;

    fn name(&self) -> &'static str;
}
