pub mod arm;
pub mod x86_64;

pub trait Backend {
    fn generate_assembly(&self, value: i64) -> String;
}
