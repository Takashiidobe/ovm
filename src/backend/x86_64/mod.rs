use std::collections::{BTreeMap, HashMap};

use crate::optimizer::{CmpOp, Instr, Op, registers::Location};

use super::Backend;

#[derive(Default, Clone)]
pub struct Codegen {
    phi_moves: BTreeMap<String, Vec<(String, String)>>,
    asm: Vec<String>,
    current_block: Option<String>,
}

impl Backend for Codegen {
    fn generate_assembly(
        &mut self,
        instrs: &[Instr],
        locations: &HashMap<String, Location>,
    ) -> String {
        // set up phi nodes
        for instr in instrs {
            if let Instr::Phi(dest, incoming) = instr {
                for (pred_label, src_val) in incoming {
                    self.phi_moves
                        .entry(pred_label.clone())
                        .or_default()
                        .push((src_val.clone(), dest.clone()));
                }
            }
        }

        // .data
        self.add(".section .data");
        self.add("fmt: .string \"%ld\\n\"");

        // .bss for spilled temps
        self.add(".section .bss");
        for (temp, loc) in locations {
            if let Location::Spill = loc {
                self.add(format!("{}: .quad 0", temp));
            }
        }

        // .text
        self.add(".section .text");
        self.add(".globl main");
        self.add("main:");

        // Emit code
        let resolve = |t: &String| -> String {
            match locations
                .get(t)
                .unwrap_or_else(|| panic!("Could not find temporary variable {t}"))
            {
                Location::Register(reg) => reg.clone(),
                Location::Spill => format!("{}(%rip)", t),
            }
        };

        dbg!(&instrs);

        for instr in instrs {
            match instr {
                Instr::Const(name, val) => {
                    let dst = resolve(name);
                    self.add(format!("movq ${}, {}", val, dst));
                }
                Instr::BinOp(dest, left, op, right) => {
                    let l = resolve(left);
                    let r = resolve(right);
                    let d = resolve(dest);

                    match op {
                        Op::Div => {
                            self.add(format!("movq {}, %rax", l));
                            self.add("cqto");
                            self.add(format!("idivq {}", r));
                            self.add(format!("movq %rax, {}", d));
                        }
                        _ => {
                            self.add(format!("movq {}, %rax", l));
                            let op_instr = match op {
                                Op::Add => "addq",
                                Op::Sub => "subq",
                                Op::Mul => "imulq",
                                Op::BitOr | Op::Or => "orq",
                                Op::BitAnd | Op::And => "andq",
                                _ => unreachable!(),
                            };
                            self.add(format!("{} {}, %rax", op_instr, r));
                            self.add(format!("movq %rax, {}", d));
                        }
                    }
                }
                Instr::Print(name) => {
                    let src = resolve(name);
                    self.add(format!("movq {src}, %rsi"));
                    self.add("leaq fmt(%rip), %rdi");
                    self.add("xor %rax, %rax");
                    self.add("call printf");
                }
                Instr::Cmp(dest, left, cmp_op, right) => {
                    let dest = resolve(dest);
                    let l = resolve(left);
                    let r = resolve(right);
                    self.add(format!("cmpq {}, {}", r, l));
                    self.add(match cmp_op {
                        CmpOp::Eq => "sete %al",
                        CmpOp::Neq => "setne %al",
                        CmpOp::Lt => "setl %al",
                        CmpOp::Lte => "setle %al",
                        CmpOp::Gt => "setg %al",
                        CmpOp::Gte => "setge %al",
                    });
                    // movzb can only be moved to a register. Arbitrarily use %rax.
                    self.add("movzb %al, %rax");
                    self.add(format!("movq %rax, {dest}"));
                }
                Instr::BranchIf(cond, then_label, else_label) => {
                    let reg = resolve(cond);
                    self.add(format!("cmpq $0, {}", reg));

                    let current = self.current_block.as_ref().expect("no current block");

                    // Emit Phi moves before branching
                    if let Some(moves) = self.phi_moves.get(current) {
                        for (src, dest) in moves.clone() {
                            self.move_memory(&resolve(&src), &resolve(&dest));
                        }
                    }

                    self.add(format!("je {}", else_label));
                    self.add(format!("jmp {}", then_label));
                }
                Instr::Jump(label) => {
                    eprintln!("Emitting jump to {label}");
                    let current = self.current_block.as_ref().expect("no current block");

                    // before jumping to a label, we want to see all the variables in the current
                    // block. If they
                    if let Some(moves) = self.phi_moves.get(current) {
                        eprintln!("Phi moves from {current} to {label}: {:?}", moves);
                        for (src, dest) in moves.clone() {
                            self.move_memory(&resolve(&src), &resolve(&dest));
                        }
                    }

                    self.add(format!("jmp {}", label));
                }
                Instr::Label(label) => {
                    self.current_block = Some(label.clone());
                    self.add(format!("{}:", label));
                }
                // This handles Phi nodes from BranchIf properly, but not from while loops.
                // Before lowering to SSA form, I need a proper CFG.
                Instr::Phi(dest, preds) => {
                    for (pred, val) in preds {
                        eprintln!("Phi: in {pred}, move {val} -> {dest}");
                        self.phi_moves
                            .entry(pred.clone())
                            .or_default()
                            .push((val.clone(), dest.clone()));
                    }
                }
                Instr::Assign(dest, src) => {
                    let dest = resolve(dest);
                    let src = resolve(src);

                    self.move_memory(&src, &dest);
                }
            }
        }

        self.add("movl $0, %eax");
        self.add("ret");

        self.format_asm(&self.asm)
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

    fn move_memory(&mut self, src: &str, dest: &str) {
        // Check if both source and destination are memory locations
        let is_src_mem = src.contains("(%rip)");
        let is_dest_mem = dest.contains("(%rip)");

        if is_src_mem && is_dest_mem {
            // Can't move directly between memory locations in x86-64, so we need to evict a register
            // Let's pick %rax arbitrarily, and use it as an intermediate
            self.add(format!("movq {}, %rax", src));
            self.add(format!("movq %rax, {}", dest));
        } else {
            // Direct move is allowed if either src or dest are a register
            self.add(format!("movq {}, {}", src, dest));
        }
    }

    fn add<S: AsRef<str>>(&mut self, line: S) {
        self.asm.push(line.as_ref().to_string());
    }
}
