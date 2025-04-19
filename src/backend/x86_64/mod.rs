use std::collections::{BTreeMap, HashMap};

use crate::optimizer::{CmpOp, Instr, Op, registers::Location};

use super::Backend;

#[derive(Default, Clone)]
pub struct Codegen {
    phi_prep_moves: BTreeMap<(String, String), Vec<(String, String)>>,
    asm: Vec<String>,
    current_block: Option<String>,
}

impl Backend for Codegen {
    fn generate_assembly(
        &mut self,
        instrs: &[Instr],
        locations: &HashMap<String, Location>,
    ) -> String {
        // --- Pass 1: Map instruction indices to block labels and Phi destinations to block labels ---
        let mut current_block_label = "entry_0".to_string(); // Assume entry_0 if no label seen first
        let mut instr_block_labels = HashMap::new();
        let mut phi_locations = HashMap::new(); // phi_dest_ssa -> block_label

        for (idx, instr) in instrs.iter().enumerate() {
            if let Instr::Label(label) = instr {
                current_block_label = label.clone();
            }
            instr_block_labels.insert(idx, current_block_label.clone());
            if let Instr::Phi(dest, _) = instr {
                phi_locations.insert(dest.clone(), current_block_label.clone());
            }
        }

        // --- Pass 2: Populate phi_prep_moves based on Phi instructions and locations ---
        self.phi_prep_moves.clear(); // Ensure it's empty before populating
        for instr in instrs {
            if let Instr::Phi(dest, preds) = instr {
                if let Some(phi_block_label) = phi_locations.get(dest) {
                    for (pred_label, pred_val_ssa) in preds {
                        eprintln!(
                            "Phi Prep (Pass 2): At end of block '{}', move '{}' to target of '{}' (which is in block '{}')",
                            pred_label, pred_val_ssa, dest, phi_block_label
                        );
                        self.phi_prep_moves
                            .entry((pred_label.clone(), phi_block_label.clone()))
                            .or_default()
                            .push((dest.clone(), pred_val_ssa.clone()));
                    }
                } else {
                    eprintln!(
                        "Warning: Could not find block location for Phi dest '{}'",
                        dest
                    );
                }
            }
        }

        // --- Pass 3: Code Generation ---
        self.current_block = None; // Reset current block before starting generation

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
                                Op::LShift => "shlq",
                                Op::RShift => "shrq",
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
                    let cond_reg = resolve(cond);
                    self.add(format!("cmpq $0, {}", cond_reg));
                    self.add(format!("jne {}", then_label)); // Jump if condition is true (non-zero)

                    // Code for the 'else' path (condition is false/zero)
                    // Emit Phi moves for the edge to the else block *before* jumping/falling through
                    if let Some(prep_moves) = self
                        .phi_prep_moves
                        .get(&(self.current_block.clone().unwrap(), else_label.to_string()))
                    {
                        eprintln!("Phi Moves for edge to {}: {:?}", else_label, prep_moves);
                        for (phi_dest_ssa, pred_val_ssa) in prep_moves.clone() {
                            let dest_loc = resolve(&phi_dest_ssa);
                            let src_loc = resolve(&pred_val_ssa);
                            self.move_memory(&src_loc, &dest_loc);
                        }
                    }
                    self.add(format!("jmp {}", else_label)); // Jump to else block

                    // Code for the 'then' path (condition is true/non-zero)
                    // Need a label for the jump instruction above to target
                    // self.add(format!("{}:", then_label)); // The original branch goes here
                    // Emit Phi moves for the edge to the then block *before* jumping/falling through
                    // NOTE: This part might be unreachable if the jne always jumps past it.
                    // The structure assumes the 'then' block follows immediately.
                    // A robust CFG-aware approach is needed here.
                    if let Some(prep_moves) = self
                        .phi_prep_moves
                        .get(&(self.current_block.clone().unwrap(), then_label.to_string()))
                    {
                        eprintln!("Phi Moves for edge to {}: {:?}", then_label, prep_moves);
                        for (phi_dest_ssa, pred_val_ssa) in prep_moves.clone() {
                            let dest_loc = resolve(&phi_dest_ssa);
                            let src_loc = resolve(&pred_val_ssa);
                            self.move_memory(&src_loc, &dest_loc);
                        }
                    }
                    // Implicit fallthrough to then_label if it's the next instruction
                    // Or an explicit jump if the structure requires it (we assume fallthrough for now)
                }
                Instr::Jump(label) => {
                    // Emit Phi moves for the edge to the target block *before* jumping
                    if let Some(prep_moves) = self
                        .phi_prep_moves
                        .get(&(self.current_block.clone().unwrap(), label.to_string()))
                    {
                        eprintln!("Phi Moves for edge to {}: {:?}", label, prep_moves);
                        for (phi_dest_ssa, pred_val_ssa) in prep_moves.clone() {
                            let dest_loc = resolve(&phi_dest_ssa);
                            let src_loc = resolve(&pred_val_ssa);
                            self.move_memory(&src_loc, &dest_loc);
                        }
                    }

                    self.add(format!("jmp {}", label));
                }
                Instr::Label(label) => {
                    self.current_block = Some(label.clone());
                    self.add(format!("{}:", label));
                }
                // Phi nodes are handled entirely during the setup passes
                Instr::Phi(_, _) => {
                    // No code generation needed here
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
