use crate::optimizer::{CFG, CmpOp, Instr, Op};
use std::collections::{HashMap, VecDeque};
use super::pass::Pass;

/// Global Value Numbering optimization pass.
/// This pass identifies expressions that compute the same value and replaces them
/// with references to a single representative value.
pub struct GlobalValueNumbering;

// Helper structure to represent expressions canonically for hashing.
// Ensures commutativity for relevant operations.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ExprKey {
    // Constant value
    Const(i64),
    // Op, vn1, vn2 (vn1 <= vn2 for commutative ops)
    BinOp(Op, usize, usize),
    // CmpOp, vn1, vn2
    Cmp(CmpOp, usize, usize),
    // Could add UnaryOp, etc. later if needed.
}

impl ExprKey {
    fn from_binop(op: Op, vn1: usize, vn2: usize) -> Self {
        match op {
            // Commutative operations: order operands by value number.
            Op::Add | Op::Mul | Op::BitAnd | Op::BitOr | Op::And | Op::Or => {
                if vn1 <= vn2 {
                    ExprKey::BinOp(op, vn1, vn2)
                } else {
                    ExprKey::BinOp(op, vn2, vn1)
                }
            }
            // Non-commutative operations: keep original order.
            Op::Sub | Op::Div | Op::Shl | Op::Shr | Op::Mod => ExprKey::BinOp(op, vn1, vn2),
        }
    }
}

impl Pass for GlobalValueNumbering {
    fn optimize(&self, mut cfg: CFG) -> CFG {
        if cfg.blocks.is_empty() {
            return cfg;
        }

        // Initialize GVN Global State
        let mut global_expr_to_vn: HashMap<ExprKey, usize> = HashMap::new();
        let mut global_vn_to_canonical_var: HashMap<usize, String> = HashMap::new();
        let mut next_vn = 0;

        // Track value numbers at block boundaries
        let mut block_entry_states: HashMap<String, HashMap<String, usize>> = HashMap::new();
        let mut block_exit_states: HashMap<String, HashMap<String, usize>> = HashMap::new();

        // Initialize worklist with all blocks
        let mut worklist: VecDeque<String> = cfg.blocks.keys().cloned().collect();
        
        // Process blocks until fixpoint
        while let Some(label) = worklist.pop_front() {
            // Get block data we need before mutable borrow
            let (block_instrs, block_preds, block_succs) = {
                let block = cfg.blocks.get(&label).unwrap();
                (block.instrs.clone(), block.preds.clone(), block.succs.clone())
            };
            
            // Compute entry state by merging predecessor exit states
            let mut entry_state = HashMap::new();
            if !block_preds.is_empty() {
                // Merge states from all predecessors
                for pred in &block_preds {
                    if let Some(pred_state) = block_exit_states.get(pred) {
                        for (var, &vn) in pred_state {
                            entry_state.entry(var.clone())
                                .and_modify(|e| if *e != vn { *e = next_vn; next_vn += 1; })
                                .or_insert(vn);
                        }
                    }
                }
            }

            // Process the block
            let (new_instrs, exit_state) = self.process_basic_block(
                &block_instrs,
                &entry_state,
                &mut global_expr_to_vn,
                &mut global_vn_to_canonical_var,
                &mut next_vn,
            );

            // Check if block's analysis changed
            let state_changed = block_exit_states.get(&label) != Some(&exit_state);
            
            if state_changed {
                // Update states
                block_entry_states.insert(label.clone(), entry_state);
                block_exit_states.insert(label.clone(), exit_state);
                
                // Update block instructions
                cfg.blocks.get_mut(&label).unwrap().instrs = new_instrs;
                
                // Add successors to worklist
                worklist.extend(block_succs);
            }
        }

        cfg
    }

    fn name(&self) -> &'static str {
        "GlobalValueNumbering"
    }
}

impl GlobalValueNumbering {
    // Renamed and updated signature for global processing
    fn process_basic_block(
        &self,
        instrs: &[Instr], // Use slice
        entry_var_to_vn: &HashMap<String, usize>,
        global_expr_to_vn: &mut HashMap<ExprKey, usize>,
        global_vn_to_canonical_var: &mut HashMap<usize, String>,
        next_vn: &mut usize,
    ) -> (Vec<Instr>, HashMap<String, usize>) // Return optimized instrs and exit state
    {
        let mut current_var_to_vn = entry_var_to_vn.clone();
        let mut new_instrs = Vec::with_capacity(instrs.len());

        for instr in instrs {
            if matches!(instr, Instr::Label(_)) {
                new_instrs.push(instr.clone());
                continue;
            }

            match instr {
                Instr::Const(dest, val) => {
                    let key = ExprKey::Const(*val);
                    let vn = match global_expr_to_vn.entry(key) {
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            // Value number already exists for this constant
                            *entry.get()
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            // New constant value, assign a new VN
                            let new_vn = *next_vn;
                            *next_vn += 1;
                            entry.insert(new_vn);
                            // Record the canonical variable for this new VN
                            global_vn_to_canonical_var.insert(new_vn, dest.clone());
                            new_vn
                        }
                    };

                    // Update the current block's variable mapping for the destination
                    current_var_to_vn.insert(dest.clone(), vn);

                    // Always keep the original Const instruction
                    new_instrs.push(instr.clone());
                }
                Instr::BinOp(dest, src1, op, src2) => {
                    // Ensure operands have value numbers before proceeding
                    if let (Some(&vn1), Some(&vn2)) =
                        (current_var_to_vn.get(src1), current_var_to_vn.get(src2))
                    {
                        let key = ExprKey::from_binop(*op, vn1, vn2);
                        if let Some(&existing_vn) = global_expr_to_vn.get(&key) {
                            // Expression result already computed
                            if let Some(canonical_var) =
                                global_vn_to_canonical_var.get(&existing_vn)
                            {
                                if canonical_var != dest {
                                    new_instrs
                                        .push(Instr::Assign(dest.clone(), canonical_var.clone()));
                                } else {
                                    // If dest is the canonical var, re-emit original BinOp
                                    // (Similar reasoning to Const: first encounter in this block processing)
                                    new_instrs.push(instr.clone());
                                }
                                current_var_to_vn.insert(dest.clone(), existing_vn);
                            } else {
                                // Defensive: canonical var missing
                                eprintln!(
                                    "Warning: GVN found VN for BinOp but no canonical var. Re-emitting."
                                );
                                let vn = *next_vn;
                                *next_vn += 1;
                                global_expr_to_vn.insert(key, vn);
                                current_var_to_vn.insert(dest.clone(), vn);
                                global_vn_to_canonical_var.insert(vn, dest.clone());
                                new_instrs.push(instr.clone());
                            }
                        } else {
                            // New expression result
                            let vn = *next_vn;
                            *next_vn += 1;
                            global_expr_to_vn.insert(key, vn);
                            current_var_to_vn.insert(dest.clone(), vn);
                            global_vn_to_canonical_var.insert(vn, dest.clone());
                            new_instrs.push(instr.clone()); // Keep original BinOp
                        }
                    } else {
                        // Operands not found in VN map (should not happen in SSA/well-formed IR)
                        // Fallback: Assign a new VN and keep the original instruction
                        eprintln!("Warning: GVN operands missing VN for BinOp. Assigning new VN.");
                        let vn = *next_vn;
                        *next_vn += 1;
                        // We don't add to global_expr_to_vn as we don't know the operand VNs
                        current_var_to_vn.insert(dest.clone(), vn);
                        global_vn_to_canonical_var.insert(vn, dest.clone());
                        new_instrs.push(instr.clone());
                    }
                }
                Instr::Cmp(dest, src1, op, src2) => {
                    // Ensure operands have value numbers
                    if let (Some(&vn1), Some(&vn2)) =
                        (current_var_to_vn.get(src1), current_var_to_vn.get(src2))
                    {
                        // Note: Comparison result depends on Op, vn1, vn2.
                        // Not making comparison commutative in key for simplicity,
                        // but could be done if needed (e.g., swapping operands and flipping CmpOp).
                        let key = ExprKey::Cmp(*op, vn1, vn2);
                        if let Some(&existing_vn) = global_expr_to_vn.get(&key) {
                            if let Some(canonical_var) =
                                global_vn_to_canonical_var.get(&existing_vn)
                            {
                                if canonical_var != dest {
                                    new_instrs
                                        .push(Instr::Assign(dest.clone(), canonical_var.clone()));
                                } else {
                                    new_instrs.push(instr.clone()); // Dest is canonical
                                }
                                current_var_to_vn.insert(dest.clone(), existing_vn);
                            } else {
                                eprintln!(
                                    "Warning: GVN found VN for Cmp but no canonical var. Re-emitting."
                                );
                                let vn = *next_vn;
                                *next_vn += 1;
                                global_expr_to_vn.insert(key, vn);
                                current_var_to_vn.insert(dest.clone(), vn);
                                global_vn_to_canonical_var.insert(vn, dest.clone());
                                new_instrs.push(instr.clone());
                            }
                        } else {
                            let vn = *next_vn;
                            *next_vn += 1;
                            global_expr_to_vn.insert(key, vn);
                            current_var_to_vn.insert(dest.clone(), vn);
                            global_vn_to_canonical_var.insert(vn, dest.clone());
                            new_instrs.push(instr.clone());
                        }
                    } else {
                        eprintln!("Warning: GVN operands missing VN for Cmp. Assigning new VN.");
                        let vn = *next_vn;
                        *next_vn += 1;
                        current_var_to_vn.insert(dest.clone(), vn);
                        global_vn_to_canonical_var.insert(vn, dest.clone());
                        new_instrs.push(instr.clone());
                    }
                }
                Instr::Assign(dest, src) => {
                    if let Some(&src_vn) = current_var_to_vn.get(src) {
                        current_var_to_vn.insert(dest.clone(), src_vn);
                        // Check if dest == src. If so, the assignment is redundant (could be removed by another pass)
                        if dest != src {
                            new_instrs.push(instr.clone());
                        }
                    } else {
                        eprintln!(
                            "Warning: GVN source missing VN for Assign. Assigning new VN to dest."
                        );
                        let vn = *next_vn;
                        *next_vn += 1;
                        current_var_to_vn.insert(dest.clone(), vn);
                        global_vn_to_canonical_var.insert(vn, dest.clone());
                        new_instrs.push(instr.clone());
                    }
                }
                Instr::Phi(dest, _) => {
                    // Phi node handling is complex in GVN.
                    // A simple approach is to always assign a new VN to Phi results
                    // as their value depends on the control flow path taken.
                    // More advanced GVN might try to equate Phis if their inputs match
                    // under all predecessor paths, but that requires more complex state merging.
                    // For now, let's ensure Phi destinations get a VN but don't participate
                    // in expression key lookups directly based on sources.
                    let vn = *next_vn;
                    *next_vn += 1;
                    current_var_to_vn.insert(dest.clone(), vn);
                    global_vn_to_canonical_var.insert(vn, dest.clone());
                    // Keep the original Phi instruction; its resolution happens later?
                    new_instrs.push(instr.clone());
                }
                _ => {
                    // Preserve instructions like Print, Jump, BranchIf, etc.
                    // Potentially invalidate VN for registers modified by calls, etc. if added later.
                    new_instrs.push(instr.clone());
                }
            }
        }
        (new_instrs, current_var_to_vn)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::passes::test_helpers::*;

    #[test]
    fn test_simple_redundancy() {
        let pass = GlobalValueNumbering;
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("c1", 1),
                cnst("c2", 2),
                binop("t0", "c1", Op::Add, "c2"),
                binop("t1", "c1", Op::Add, "c2"),  // redundant
                print("t1"),
            ], vec![], vec![]),
        ]);

        let expected = create_test_cfg(vec![
            ("entry", vec![
                cnst("c1", 1),
                cnst("c2", 2),
                binop("t0", "c1", Op::Add, "c2"),
                assign("t1", "t0"),  // replaced with assignment
                print("t1"),
            ], vec![], vec![]),
        ]);

        assert_eq!(pass.optimize(cfg), expected);
    }

    #[test]
    fn test_cross_block_redundancy() {
        let pass = GlobalValueNumbering;
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("c1", 1),
                cnst("c2", 2),
                binop("a", "c1", Op::Add, "c2"),
                branch("a", "then", "else"),
            ], vec![], vec!["then", "else"]),
            ("then", vec![
                binop("b", "c1", Op::Add, "c2"),  // redundant with 'a'
                jump("merge"),
            ], vec!["entry"], vec!["merge"]),
            ("else", vec![
                binop("c", "c1", Op::Add, "c2"),  // redundant with 'a'
                jump("merge"),
            ], vec!["entry"], vec!["merge"]),
            ("merge", vec![
                phi("d", vec![("then", "b"), ("else", "c")]),
                print("d"),
            ], vec!["then", "else"], vec![]),
        ]);

        let expected = create_test_cfg(vec![
            ("entry", vec![
                cnst("c1", 1),
                cnst("c2", 2),
                binop("a", "c1", Op::Add, "c2"),
                branch("a", "then", "else"),
            ], vec![], vec!["then", "else"]),
            ("then", vec![
                assign("b", "a"),  // replaced with assignment
                jump("merge"),
            ], vec!["entry"], vec!["merge"]),
            ("else", vec![
                assign("c", "a"),  // replaced with assignment
                jump("merge"),
            ], vec!["entry"], vec!["merge"]),
            ("merge", vec![
                phi("d", vec![("then", "b"), ("else", "c")]),
                print("d"),
            ], vec!["then", "else"], vec![]),
        ]);

        assert_eq!(pass.optimize(cfg), expected);
    }

    #[test]
    fn test_phi_aware_numbering() {
        let pass = GlobalValueNumbering;
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("x", 1),
                jump("loop"),
            ], vec![], vec!["loop"]),
            ("loop", vec![
                phi("v", vec![("entry", "x"), ("loop", "v")]),
                print("v"),
                jump("loop"),
            ], vec!["entry", "loop"], vec!["loop"]),
        ]);

        // Phi should not be eliminated as it represents different values
        assert_eq!(pass.optimize(cfg.clone()), cfg);
    }
}
