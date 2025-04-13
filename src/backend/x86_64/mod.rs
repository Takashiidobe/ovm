pub struct X86_64Backend;

impl X86_64Backend {
    pub fn generate_assembly(&self, value: i64) -> String {
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
