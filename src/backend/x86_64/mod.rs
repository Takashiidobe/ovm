use std::collections::{BTreeMap, HashMap};

use crate::optimizer::{CmpOp, Instr, Op, registers::Location};

use super::Backend;

#[derive(Default, Clone)]
pub struct Codegen {
    locations: HashMap<String, Location>,
    last_cmp_op: Option<CmpOp>,
    phi_moves: BTreeMap<String, Vec<(String, String)>>,
}

impl Backend for Codegen {
    fn generate_assembly(
        &mut self,
        instrs: &[Instr],
        locations: &HashMap<String, Location>,
    ) -> String {
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

        dbg!(&instrs);

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
                Instr::Cmp(_, left, cmp_op, right) => {
                    let l = resolve(left);
                    let r = resolve(right);
                    asm.push(format!("cmpq {}, {}", r, l));
                    self.last_cmp_op = Some(cmp_op.clone());
                }
                Instr::BranchIf(cond, then_label, else_label) => {
                    if let Some(cmp_op) = self.last_cmp_op.take() {
                        // Emit jump based on condition flags set by Cmp
                        let jmp = match cmp_op {
                            CmpOp::Eq => "je",
                            CmpOp::Neq => "jne",
                            CmpOp::Lt => "jl",
                            CmpOp::Lte => "jle",
                            CmpOp::Gt => "jg",
                            CmpOp::Gte => "jge",
                        };
                        asm.push(format!("{} {}", jmp, then_label));
                        asm.push(format!("jmp {}", else_label));
                    } else {
                        // Use value of cond as a boolean (0 or nonzero)
                        let reg = resolve(cond);
                        asm.push(format!("movq {}, %rax", reg));
                        asm.push("testq %rax, %rax".to_string());
                        asm.push(format!("jne {}", then_label));
                        asm.push(format!("jmp {}", else_label));
                    }
                }
                Instr::Jump(label) => {
                    if let Some(moves) = self.phi_moves.get(label) {
                        for (src, dest) in moves {
                            asm.push(format!("movq {}, {}", resolve(src), resolve(dest)));
                        }
                    }
                    asm.push(format!("jmp {}", label));
                }
                Instr::Label(label) => {
                    asm.push(format!("{}:", label));
                }
                Instr::Phi(dest, from_then, from_else) => {
                    self.phi_moves
                        .entry("then_0".to_string())
                        .or_default()
                        .push((from_then.clone(), dest.clone()));

                    self.phi_moves
                        .entry("else_1".to_string())
                        .or_default()
                        .push((from_else.clone(), dest.clone()));
                }
                Instr::Assign(dest, src) => {
                    let dest = resolve(dest);
                    let src = resolve(src);

                    asm.push(format!("mov {dest}, {src}"));
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
