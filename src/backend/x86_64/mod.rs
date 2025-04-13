use crate::backend::Backend;

pub struct X86_64;

impl Backend for X86_64 {
    fn generate_assembly(&self, value: i64) -> String {
        format!(
            ".globl main
main:
    mov ${}, %rax        # Load our value into rax (return value)
    ret
",
            value
        )
    }
}
