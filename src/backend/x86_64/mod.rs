use crate::backend::Backend;
use crate::frontend::parser::Expr;

pub struct X86_64;

impl Backend for X86_64 {
    fn generate_assembly(&self, expr: &Expr) -> String {
        let mut asm = String::from(".globl main\nmain:\n");

        Self::generate_expr_code(expr, &mut asm);

        asm.push_str("    ret\n");
        asm
    }
}

impl X86_64 {
    fn generate_expr_code(expr: &Expr, asm: &mut String) {
        match expr {
            Expr::Num(n) => {
                asm.push_str(&format!(
                    "    mov ${}, %rax        # Load value into rax\n",
                    n
                ));
            }
            Expr::Add(left, right) => {
                Self::generate_expr_code(left, asm);
                asm.push_str("    push %rax                # Save left operand\n");
                Self::generate_expr_code(right, asm);
                asm.push_str("    pop %rcx                 # Restore left operand into rcx\n");
                asm.push_str("    add %rcx, %rax           # rax = rcx + rax\n");
            }
            Expr::Sub(left, right) => {
                Self::generate_expr_code(left, asm);
                asm.push_str("    push %rax                # Save left operand\n");
                Self::generate_expr_code(right, asm);
                asm.push_str("    mov %rax, %rcx           # Move right operand to rcx\n");
                asm.push_str("    pop %rax                 # Restore left operand\n");
                asm.push_str("    sub %rcx, %rax           # rax = rax - rcx\n");
            }
            Expr::Mul(left, right) => {
                Self::generate_expr_code(left, asm);
                asm.push_str("    push %rax                # Save left operand\n");
                Self::generate_expr_code(right, asm);
                asm.push_str("    pop %rcx                 # Restore left operand into rcx\n");
                asm.push_str("    imul %rcx, %rax          # rax = rcx * rax\n");
            }
            Expr::Div(left, right) => {
                Self::generate_expr_code(left, asm);
                asm.push_str("    push %rax                # Save left operand (dividend)\n");
                Self::generate_expr_code(right, asm);
                asm.push_str(
                    "    mov %rax, %rcx           # Move right operand (divisor) to rcx\n",
                );
                asm.push_str(
                    "    pop %rax                 # Restore left operand (dividend) to rax\n",
                );
                asm.push_str("    cqo                      # Sign-extend RAX into RDX:RAX\n");
                asm.push_str(
                    "    idiv %rcx                # Divide RDX:RAX by RCX, result in RAX\n",
                );
            }
        }
    }
}
