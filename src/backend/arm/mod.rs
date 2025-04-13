use crate::backend::Backend;
pub struct ARM;

impl Backend for ARM {
    fn generate_assembly(&self, _: i64) -> String {
        unimplemented!()
    }
}
