use std::collections::{BTreeMap, HashMap, VecDeque, HashSet};

use crate::optimizer::{CmpOp, Instr, Op, registers::Location};

use super::Backend;

#[derive(Default, Clone)]
pub struct Codegen {
    phi_prep_moves: BTreeMap<(String, String), Vec<(String, String)>>,
    asm: Vec<String>,
    current_block: Option<String>,
}

// Helper struct for basic block info during preprocessing
#[derive(Debug, Clone)]
struct BlockInfo {
    label: String,
    start_idx: usize,
    end_idx: usize, // Index of the last instruction (usually Jump or BranchIf)
    successors: Vec<String>,
    // We might not need predecessors explicitly if we modify Phis in place
}

impl Backend for Codegen {
    fn generate_assembly(
        &mut self,
        instrs: &[Instr],
        locations: &HashMap<String, Location>,
    ) -> String {
        // --- Pre-processing Pass: Build CFG, Insert Jumps, Map Edges ---
        let (processed_instrs, blocks_map, edge_to_logical_pred) = self.preprocess_ir_and_build_maps(instrs);

        // --- Pass 1 (on processed instrs): Map Phi destinations to block labels ---
        let mut phi_locations = HashMap::new();
        let mut current_block_label_for_phi = "main".to_string();
         if let Some(first_label_instr) = processed_instrs.iter().find(|instr| matches!(instr, Instr::Label(_))) {
              if let Instr::Label(label) = first_label_instr {
                  current_block_label_for_phi = label.clone();
              }
         } else {
             eprintln!("Warning: No labels found in processed IR for phi location mapping, assuming 'main'.");
         }
        for instr in &processed_instrs {
            if let Instr::Label(label) = instr {
                current_block_label_for_phi = label.clone();
            }
            if let Instr::Phi(dest, _) = instr {
                phi_locations.insert(dest.clone(), current_block_label_for_phi.clone());
            }
        }

        // --- Pass 2 (using original instrs for Phi defs): Populate phi_prep_moves ---
        self.phi_prep_moves.clear();
        for (phi_dest_ssa, phi_block_label) in &phi_locations {
             // Find the Phi instruction definition in the *original* list
             let phi_instr = instrs.iter().find(|i| matches!(i, Instr::Phi(d,_) if d == phi_dest_ssa));
             if let Some(Instr::Phi(_, preds)) = phi_instr {
                 for (logical_pred_label, pred_val_ssa) in preds {
                     // Find the actual predecessor corresponding to this logical predecessor
                     let actual_pred_key = (phi_block_label.clone(), logical_pred_label.clone());
                     if let Some(actual_pred_label) = edge_to_logical_pred.iter().find_map(|((ap, tp), lp)| {
                         if tp == phi_block_label && lp == logical_pred_label {
                             Some(ap)
                         } else {
                             None
                         }
                     }) {
                         eprintln!(
                             "Phi Prep (Pass 2): For edge ('{}' -> '{}'), found logical '{}'. Adding move '{}' = '{}'",
                             actual_pred_label, phi_block_label, logical_pred_label, phi_dest_ssa, pred_val_ssa
                         );
                         self.phi_prep_moves
                             .entry((actual_pred_label.clone(), phi_block_label.clone()))
                             .or_default()
                             .push((phi_dest_ssa.clone(), pred_val_ssa.clone()));
                     } else {
                         eprintln!(
                             "Warning: Could not find actual predecessor for logical pred '{}' targeting block '{}' in edge_to_logical_pred map.",
                             logical_pred_label, phi_block_label
                         );
                     }
                 }
             } else {
                 eprintln!("Warning: Could not find definition for Phi instruction dest '{}'", phi_dest_ssa);
             }
        }

        // --- Pass 3 (on processed instrs): Code Generation ---
        self.current_block = None;

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

        dbg!(&processed_instrs); // Debug the processed instructions

        for instr in &processed_instrs {
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
                        Op::Mod => {
                            self.add(format!("movq {}, %rax", l));
                            self.add("cqto");
                            self.add(format!("idivq {}", r));
                            self.add(format!("movq %rdx, {}", d));
                        }
                        _ => {
                            self.add(format!("movq {}, %rax", l));
                            let op_instr = match op {
                                Op::Add => "addq",
                                Op::Sub => "subq",
                                Op::Mul => "imulq",
                                Op::BitOr | Op::Or => "orq",
                                Op::BitAnd | Op::And => "andq",
                                Op::Shl => "shlq",
                                Op::Shr => "shrq",
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

    // --- Function to build CFG, map edges, and insert explicit jumps ---
    fn preprocess_ir_and_build_maps(&self, original_instrs: &[Instr]) -> (Vec<Instr>, BTreeMap<String, BlockInfo>, HashMap<(String, String), String>) {
        // --- Pass 1 & 2: Build initial block info and successors (unchanged) ---
        let mut blocks = BTreeMap::<String, BlockInfo>::new();
        let mut label_to_idx = HashMap::<String, usize>::new();
        let mut current_label = None;
        let mut current_start_idx = 0;

        for (idx, instr) in original_instrs.iter().enumerate() {
            if let Instr::Label(label) = instr {
                if let Some(prev_label) = current_label {
                    blocks.entry(prev_label).and_modify(|b| {
                        b.end_idx = idx - 1;
                    });
                }
                let label_str = label.clone();
                label_to_idx.insert(label_str.clone(), idx);
                blocks.insert(label_str.clone(), BlockInfo {
                    label: label_str.clone(),
                    start_idx: idx,
                    end_idx: original_instrs.len() - 1, // Tentative
                    successors: Vec::new(),
                });
                current_label = Some(label_str);
                current_start_idx = idx;
            }
        }
        if let Some(last_label) = current_label {
            blocks.entry(last_label).and_modify(|b| {
                b.end_idx = original_instrs.len() - 1;
            });
        } else if !original_instrs.is_empty() {
             let default_label = "main".to_string(); // Consistent default
             label_to_idx.insert(default_label.clone(), 0);
             blocks.insert(default_label.clone(), BlockInfo {
                 label: default_label,
                 start_idx: 0,
                 end_idx: original_instrs.len() - 1,
                 successors: Vec::new(),
             });
        }

        let mut fallthrough_jumps_to_insert = HashMap::new(); // block_label -> target_label

        for (label, info) in blocks.iter_mut() {
            if info.end_idx >= original_instrs.len() { continue; } // Skip if end_idx is out of bounds
            if let Some(last_instr) = original_instrs.get(info.end_idx) {
                match last_instr {
                    Instr::Jump(target_label) => {
                        info.successors.push(target_label.clone());
                    }
                    Instr::BranchIf(_, then_label, else_label) => {
                        info.successors.push(then_label.clone());
                        info.successors.push(else_label.clone());
                    }
                    _ => { // Check for fallthrough
                         if let Some(next_instr) = original_instrs.get(info.end_idx + 1) {
                            if let Instr::Label(next_label) = next_instr {
                                 info.successors.push(next_label.clone());
                                 // Mark that an explicit jump needs to be inserted
                                 fallthrough_jumps_to_insert.insert(label.clone(), next_label.clone());
                            }
                        }
                         // Consider edge case: last block doesn't end in jump/branch and isn't followed by label (e.g. ends in ret implicitly)
                         // For now, assuming last block might fall through or have explicit jump.
                    }
                }
            }
        }

        // --- Create Modified Instruction List with Explicit Jumps ---
        let mut modified_instrs = Vec::with_capacity(original_instrs.len() + fallthrough_jumps_to_insert.len());
        let mut original_idx = 0;
        while original_idx < original_instrs.len() {
            let instr = &original_instrs[original_idx];
            modified_instrs.push(instr.clone());

            // Check if this is the end of a block that needs a fallthrough jump inserted
             if let Some(block_label) = blocks.iter().find_map(|(lbl, info)| if info.end_idx == original_idx { Some(lbl) } else { None }) {
                if let Some(target_label) = fallthrough_jumps_to_insert.get(block_label) {
                    eprintln!("Inserting explicit Jump from {} to {} (fallthrough)", block_label, target_label);
                    modified_instrs.push(Instr::Jump(target_label.clone()));
                }
            }
            original_idx += 1;
        }

        // --- Recalculate block info based on modified_instrs --- 
        // (Necessary because indices and end instructions changed)
        let mut final_blocks = BTreeMap::<String, BlockInfo>::new();
        let mut final_label_to_idx = HashMap::<String, usize>::new();
        let mut current_label = None;

        for (idx, instr) in modified_instrs.iter().enumerate() {
            if let Instr::Label(label) = instr {
                if let Some(prev_label) = current_label {
                    final_blocks.entry(prev_label).and_modify(|b| {
                        b.end_idx = idx - 1;
                    });
                }
                let label_str = label.clone();
                final_label_to_idx.insert(label_str.clone(), idx);
                final_blocks.insert(label_str.clone(), BlockInfo {
                    label: label_str.clone(),
                    start_idx: idx,
                    end_idx: modified_instrs.len() - 1, // Tentative
                    successors: Vec::new(),
                });
                current_label = Some(label_str);
            }
        }
        if let Some(last_label) = current_label {
            final_blocks.entry(last_label).and_modify(|b| {
                b.end_idx = modified_instrs.len() - 1;
            });
        } else if !modified_instrs.is_empty() {
             // Handle case where modified list starts without a label (should use default 'main')
             let default_label = "main".to_string();
             if !final_blocks.contains_key(&default_label) {
                 final_label_to_idx.insert(default_label.clone(), 0);
                 final_blocks.insert(default_label.clone(), BlockInfo {
                     label: default_label,
                     start_idx: 0,
                     end_idx: modified_instrs.len() - 1,
                     successors: Vec::new(),
                 });
             }
        }

        // Recalculate successors for final_blocks
        for (label, info) in final_blocks.iter_mut() {
             if info.end_idx >= modified_instrs.len() { continue; }
             // Clear old successors if any were calculated before
             info.successors.clear();
            if let Some(last_instr) = modified_instrs.get(info.end_idx) {
                match last_instr {
                    Instr::Jump(target_label) => {
                        info.successors.push(target_label.clone());
                    }
                    Instr::BranchIf(_, then_label, else_label) => {
                        info.successors.push(then_label.clone());
                        info.successors.push(else_label.clone());
                    }
                    _ => { /* No fallthrough possible anymore */ }
                }
            }
        }

        // --- Pass 3: Build the edge_to_logical_pred map (using original_instrs for Phi defs, final_blocks for CFG) ---
        let mut edge_to_logical_pred = HashMap::<(String, String), String>::new();
        for (phi_block_label, _) in &final_blocks {
            // Find all actual predecessors for this phi_block using final_blocks CFG
            let actual_predecessors_set: HashSet<String> = final_blocks.iter()
                .filter(|(_, info)| info.successors.contains(phi_block_label))
                .map(|(label, _)| label.clone())
                .collect();

            // Find Phi definitions in the original instruction list corresponding to this block
            // Need original block info for this lookup step
            if let Some(original_block_info) = blocks.get(phi_block_label) {
                for idx in original_block_info.start_idx..=original_block_info.end_idx {
                    if idx >= original_instrs.len() { continue; }
                    if let Some(Instr::Phi(_, original_preds_ref)) = original_instrs.get(idx) {
                        let original_preds = original_preds_ref.clone();

                        eprintln!(
                            "Mapping Edges for Phis in {}: Logical Preds: {:?}, Actual Preds: {:?}",
                            phi_block_label,
                            original_preds.iter().map(|(lp, _)| lp).collect::<Vec<_>>(),
                            actual_predecessors_set
                        );
                        let mut used_actual_preds = HashSet::new();
                        let mut remaining_logical: Vec<(String, String)> = Vec::new();
                        // Pass 1: Direct Matches
                        for (logical_pred, _) in &original_preds {
                            if actual_predecessors_set.contains(logical_pred) {
                                if !used_actual_preds.contains(logical_pred) {
                                    eprintln!(
                                        "  Mapping edge ('{}', '{}') -> logical '{}' (direct match)",
                                        logical_pred, phi_block_label, logical_pred
                                    );
                                    edge_to_logical_pred.insert((logical_pred.clone(), phi_block_label.clone()), logical_pred.clone());
                                    used_actual_preds.insert(logical_pred.clone());
                                } else {
                                    remaining_logical.push((logical_pred.clone(), "".to_string()));
                                }
                            } else {
                                remaining_logical.push((logical_pred.clone(), "".to_string()));
                            }
                        }
                        // Pass 2: Fallback for Remaining
                        let mut remaining_actual: Vec<String> = actual_predecessors_set
                            .iter()
                            .filter(|ap| !used_actual_preds.contains(*ap))
                            .cloned()
                            .collect();
                        if !remaining_logical.is_empty() {
                            if remaining_logical.len() == remaining_actual.len() {
                                for (logical_pred, _) in remaining_logical {
                                    if let Some(actual_pred) = remaining_actual.pop() {
                                        eprintln!(
                                            "  Warning: Mapping edge ('{}', '{}') -> logical '{}' (fallback heuristic)",
                                            actual_pred, phi_block_label, logical_pred
                                        );
                                        edge_to_logical_pred.insert((actual_pred, phi_block_label.clone()), logical_pred.clone());
                                    } else {
                                        eprintln!("  Error: Fallback edge mapping failed - ran out of actual preds for logical '{}'.", logical_pred);
                                    }
                                }
                            } else {
                                eprintln!("  Error: Could not map all edges. Count mismatch ({} logical vs {} actual remaining).", remaining_logical.len(), remaining_actual.len());
                            }
                        }
                        // End mapping logic
                    }
                }
            }
        }

        (modified_instrs, final_blocks, edge_to_logical_pred)
    }
}
