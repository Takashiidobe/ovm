use crate::backend::Backend;
use crate::frontend::parser::{Expr, Program};
use std::fmt::Write;

pub struct X86_64;

impl Backend for X86_64 {
    fn generate_assembly(&self, program: &Program) -> String {
        // Include external printf for print statements
        let mut asm = String::from(
            ".extern printf\n\n.section .rodata\n.print_fmt:\n    .string \"%ld\\n\"\n\n.text\n.globl main\nmain:\n    push %rbp                # Save base pointer\n    mov %rsp, %rbp          # Set up stack frame\n",
        );

        // Generate code for each statement, but only the last statement's value is returned
        for (i, expr) in program.statements.iter().enumerate() {
            if i > 0 {
                // Add a comment to indicate a new statement
                asm.push_str("    # New statement\n");
            }

            // Generate code for this expression
            asm.push_str(&self.generate_expr_code(expr));
        }

        // Return the last expression's value as exit code
        asm.push_str("    xor %rax, %rax          # xor rax before exiting\n");
        asm.push_str("    mov %rbp, %rsp          # Restore stack pointer\n");
        asm.push_str("    pop %rbp                # Restore base pointer\n");
        asm.push_str("    ret\n");
        asm
    }

    fn generate_expr_code(&self, expr: &Expr) -> String {
        let mut asm = String::new();
        Self::generate_expr_code_internal(expr, &mut asm);
        asm
    }
}

impl X86_64 {
    fn generate_expr_code_internal(expr: &Expr, asm: &mut String) {
        match expr {
            Expr::Print(inner_expr) => {
                // First evaluate the expression inside print()
                Self::generate_expr_code_internal(inner_expr, asm);

                // Call printf with the result in %rax
                writeln!(
                    asm,
                    "    mov %rax, %rsi            # Move result to second argument"
                )
                .unwrap();
                writeln!(
                    asm,
                    "    lea .print_fmt(%rip), %rdi  # First argument: format string"
                )
                .unwrap();
                writeln!(
                    asm,
                    "    xor %rax, %rax            # No vector registers used"
                )
                .unwrap();
                writeln!(asm, "    call printf               # Call printf").unwrap();
            }
            Expr::Num(n) => {
                asm.push_str(&format!(
                    "    mov ${}, %rax        # Load value into rax\n",
                    n
                ));
            }
            Expr::Add(left, right) => {
                Self::generate_expr_code_internal(left, asm);
                asm.push_str("    push %rax                # Save left operand\n");
                Self::generate_expr_code_internal(right, asm);
                asm.push_str("    pop %rcx                 # Restore left operand into rcx\n");
                asm.push_str("    add %rcx, %rax           # rax = rcx + rax\n");
            }
            Expr::Sub(left, right) => {
                Self::generate_expr_code_internal(left, asm);
                asm.push_str("    push %rax                # Save left operand\n");
                Self::generate_expr_code_internal(right, asm);
                asm.push_str("    mov %rax, %rcx           # Move right operand to rcx\n");
                asm.push_str("    pop %rax                 # Restore left operand\n");
                asm.push_str("    sub %rcx, %rax           # rax = rax - rcx\n");
            }
            Expr::Mul(left, right) => {
                Self::generate_expr_code_internal(left, asm);
                asm.push_str("    push %rax                # Save left operand\n");
                Self::generate_expr_code_internal(right, asm);
                asm.push_str("    pop %rcx                 # Restore left operand into rcx\n");
                asm.push_str("    imul %rcx, %rax          # rax = rcx * rax\n");
            }
            Expr::Div(left, right) => {
                Self::generate_expr_code_internal(left, asm);
                asm.push_str("    push %rax                # Save left operand (dividend)\n");
                Self::generate_expr_code_internal(right, asm);
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
