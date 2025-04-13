use std::collections::HashMap;

use crate::optimizer::{Instr, Op, registers::Location};

use super::Backend;

pub struct Codegen;

impl Backend for Codegen {
    fn generate_assembly(&self, instrs: &[Instr], locations: &HashMap<String, Location>) -> String {
        let mut asm = Vec::new();

        // .data
        asm.push(".section .data".to_string());
        asm.push("fmt: .string \"%ld\\n\"".to_string());

        // .bss for spilled temps
        asm.push(".section .bss".to_string());
        for (temp, loc) in locations {
            if let Location::Spill = loc {
                asm.push(format!("{}: .quad 0", temp));
            }
        }

        // .text
        asm.push(".section .text".to_string());
        asm.push(".globl main".to_string());
        asm.push("main:".to_string());

        // Emit code
        let resolve = |t: &String| -> String {
            match locations.get(t).unwrap_or_else(|| {
                dbg!(&locations);
                panic!("Could not find temporary variable {t}")
            }) {
                Location::Register(reg) => reg.clone(),
                Location::Spill => format!("{}(%rip)", t),
            }
        };

        for instr in instrs {
            match instr {
                Instr::Const(name, val) => {
                    let dst = resolve(name);
                    asm.push(format!("movq ${}, {}", val, dst));
                }
                Instr::BinOp(dest, left, op, right) => {
                    let l = resolve(left);
                    let r = resolve(right);
                    let d = resolve(dest);

                    match op {
                        Op::Div => {
                            asm.push(format!("movq {}, %rax", l));
                            asm.push("cqto".to_string());
                            asm.push(format!("idivq {}", r));
                            asm.push(format!("movq %rax, {}", d));
                        }
                        _ => {
                            asm.push(format!("movq {}, %rax", l));
                            let op_instr = match op {
                                Op::Add => "addq",
                                Op::Sub => "subq",
                                Op::Mul => "imulq",
                                Op::BitOr | Op::Or => "orq",
                                Op::BitAnd | Op::And => "andq",
                                _ => unreachable!(),
                            };
                            asm.push(format!("{} {}, %rax", op_instr, r));
                            asm.push(format!("movq %rax, {}", d));
                        }
                    }
                }
                Instr::Print(name) => {
                    let src = resolve(name);
                    asm.push(format!("movq {}, %rsi", src));
                    asm.push("leaq fmt(%rip), %rdi".to_string());
                    asm.push("xor %rax, %rax".to_string());
                    asm.push("call printf".to_string());
                }
            }
        }

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
}
