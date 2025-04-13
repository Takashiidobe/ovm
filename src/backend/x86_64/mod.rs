use std::collections::HashSet;

use crate::optimizer::{Instr, Op};

use super::Backend;

pub struct Codegen;

impl Backend for Codegen {
    fn generate_assembly(&self, instrs: &[Instr]) -> String {
        let mut asm = Vec::new();

        // Section .data with format string
        asm.push(".section .data".to_string());
        asm.push("fmt: .string \"%ld\\n\"".to_string());

        // Section .bss with temp definitions
        asm.push(".section .bss".to_string());
        for temp in self.collect_temps(instrs) {
            asm.push(format!("{}: .quad 0", temp));
        }

        // Section .text
        asm.push(".section .text".to_string());
        asm.push(".globl main".to_string());
        asm.push("main:".to_string());

        // Emit code
        for instr in instrs {
            match instr {
                Instr::Const(name, val) => {
                    asm.push(format!("movq ${}, {}(%rip)", val, name));
                }
                Instr::BinOp(dest, left, op, right) => {
                    let op_instr = match op {
                        Op::Add => "addq",
                        Op::Sub => "subq",
                        Op::Mul => "imulq",
                        Op::Div => "idivq",
                    };

                    match op {
                        Op::Div => {
                            // idivq needs dividend in RAX and sign-ext in RDX
                            asm.push(format!("movq {}(%rip), %rax", left));
                            asm.push("cqto".to_string()); // sign-extend RAX into RDX:RAX
                            asm.push(format!("idivq {}(%rip)", right));
                            asm.push(format!("movq %rax, {}(%rip)", dest));
                        }
                        _ => {
                            asm.push(format!("movq {left}(%rip), %rax"));
                            asm.push(format!("{} {}(%rip), %rax", op_instr, right));
                            asm.push(format!("movq %rax, {}(%rip)", dest));
                        }
                    }
                }
                Instr::Print(name) => {
                    asm.push(format!("movq {}(%rip), %rsi", name)); // arg 2
                    asm.push("leaq fmt(%rip), %rdi".to_string()); // arg 1
                    asm.push("xor %rax, %rax".to_string()); // clear RAX for varargs
                    asm.push("call printf".to_string());
                }
            }
        }

        // Return 0
        asm.push("movl $0, %eax".to_string());
        asm.push("ret".to_string());

        self.format_asm(&asm)
    }
}

impl Codegen {
    fn format_asm(&self, lines: &[String]) -> String {
        lines
            .iter()
            .map(|line| {
                if line.trim_end().ends_with(':')
                    || line.starts_with(".section")
                    || line.starts_with(".globl")
                {
                    line.to_string()
                } else {
                    format!("  {}", line)
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn collect_temps(&self, instrs: &[Instr]) -> HashSet<String> {
        let mut temps = HashSet::new();
        for instr in instrs {
            match instr {
                Instr::Const(t, _) => {
                    temps.insert(t.clone());
                }
                Instr::BinOp(t, l, _, r) => {
                    temps.insert(t.clone());
                    temps.insert(l.clone());
                    temps.insert(r.clone());
                }
                Instr::Print(t) => {
                    temps.insert(t.clone());
                }
            }
        }
        temps
    }
}
