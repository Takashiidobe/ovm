use std::collections::{BTreeMap, HashMap};

use crate::optimizer::{CmpOp, Instr, Op, registers::Location};

use super::Backend;

#[derive(Default, Clone)]
pub struct Codegen {
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
                Instr::Cmp(dest, left, cmp_op, right) => {
                    let dest = resolve(dest);
                    let l = resolve(left);
                    let r = resolve(right);
                    asm.push(format!("cmpq {}, {}", r, l));
                    asm.push(
                        match cmp_op {
                            CmpOp::Eq => "sete %al",
                            CmpOp::Neq => "setne %al",
                            CmpOp::Lt => "setl %al",
                            CmpOp::Lte => "setle %al",
                            CmpOp::Gt => "setg %al",
                            CmpOp::Gte => "setge %al",
                        }
                        .to_string(),
                    );
                    asm.push(format!("movzb %al, {}", dest));
                }
                Instr::BranchIf(cond, then_label, else_label) => {
                    let reg = resolve(cond);
                    asm.push(format!("cmpq $0, {reg}"));
                    asm.push(format!("je {}", else_label));
                    asm.push(format!("jmp {}", then_label));
                }
                Instr::Jump(label) => {
                    if let Some(moves) = self.phi_moves.get(label) {
                        for (src, dest) in moves {
                            let src_loc = resolve(src);
                            let dest_loc = resolve(dest);

                            // Check if both source and destination are memory locations
                            let is_src_mem = src_loc.contains("(%rip)");
                            let is_dest_mem = dest_loc.contains("(%rip)");

                            if is_src_mem && is_dest_mem {
                                // Use %rax as a scratch register for memory-to-memory moves
                                asm.push(format!("movq {}, %rax", src_loc));
                                asm.push(format!("movq %rax, {}", dest_loc));
                            } else {
                                // Direct move is possible
                                asm.push(format!("movq {}, {}", src_loc, dest_loc));
                            }
                        }
                    }
                    asm.push(format!("jmp {}", label));
                }
                Instr::Label(label) => {
                    asm.push(format!("{}:", label));
                }
                Instr::Phi(dest, from_then, from_else) => {
                    // Find corresponding branch labels by scanning previous instructions
                    let mut then_label = String::new();
                    let mut else_label = String::new();

                    // Look for the most recent BranchIf to find labels
                    for instr in instrs.iter().rev() {
                        if let Instr::BranchIf(_, t_label, e_label) = instr {
                            then_label = t_label.clone();
                            else_label = e_label.clone();
                            break;
                        }
                    }

                    if !then_label.is_empty() && !else_label.is_empty() {
                        self.phi_moves
                            .entry(then_label)
                            .or_default()
                            .push((from_then.clone(), dest.clone()));

                        self.phi_moves
                            .entry(else_label)
                            .or_default()
                            .push((from_else.clone(), dest.clone()));
                    } else {
                        // Fallback to default behavior
                        eprintln!(
                            "Warning: Could not find branch labels for phi node, using default."
                        );
                        self.phi_moves
                            .entry("then_4".to_string())
                            .or_default()
                            .push((from_then.clone(), dest.clone()));

                        self.phi_moves
                            .entry("else_5".to_string())
                            .or_default()
                            .push((from_else.clone(), dest.clone()));
                    }
                }
                Instr::Assign(dest, src) => {
                    let dest_loc = resolve(dest);
                    let src_loc = resolve(src);

                    // Check if both source and destination are memory locations
                    let is_src_mem = src_loc.contains("(%rip)");
                    let is_dest_mem = dest_loc.contains("(%rip)");

                    if is_src_mem && is_dest_mem {
                        // Can't move directly between memory locations in x86-64
                        // we need to evict a register.
                        // Let's pick %rax arbitrarily, move it to where the
                        asm.push(format!("movq {}, %rax", src_loc));
                        asm.push(format!("movq %rax, {}", dest_loc));
                    } else {
                        // Direct register-to-register or memory-to-register or register-to-memory move
                        asm.push(format!("movq {}, {}", src_loc, dest_loc));
                    }
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
