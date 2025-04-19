use crate::optimizer::{CmpOp, Instr, Op, passes::Pass};
use std::collections::{HashMap, VecDeque};

/// Global Value Numbering optimization pass.
/// This pass identifies expressions that compute the same value and replaces them
/// with references to a single representative value.
pub struct GlobalValueNumbering;

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
                Instr::Const(dest, _) => {
                    let vn = *next_vn;
                    *next_vn += 1;
                    current_var_to_vn.insert(dest.clone(), vn);
                    global_vn_to_canonical_var.insert(vn, dest.clone());
                    new_instrs.push(instr.clone());
                }
                Instr::BinOp(dest, src1, op, src2) => {
                    if let (Some(&vn1), Some(&vn2)) =
                        (current_var_to_vn.get(src1), current_var_to_vn.get(src2))
                    {
                        let key = ExprKey::from_binop(*op, vn1, vn2);
                        if let Some(&existing_vn) = global_expr_to_vn.get(&key) {
                            let canonical_var = global_vn_to_canonical_var
                                .get(&existing_vn)
                                .expect("Canonical var must exist");
                            current_var_to_vn.insert(dest.clone(), existing_vn);
                            new_instrs.push(Instr::Assign(dest.clone(), canonical_var.clone()));
                        } else {
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
                        current_var_to_vn.insert(dest.clone(), vn);
                        global_vn_to_canonical_var.insert(vn, dest.clone());
                        new_instrs.push(instr.clone());
                    }
                }
                Instr::Cmp(dest, src1, op, src2) => {
                    if let (Some(&vn1), Some(&vn2)) =
                        (current_var_to_vn.get(src1), current_var_to_vn.get(src2))
                    {
                        let key = ExprKey::Cmp(*op, vn1, vn2);
                        if let Some(&existing_vn) = global_expr_to_vn.get(&key) {
                            let canonical_var = global_vn_to_canonical_var
                                .get(&existing_vn)
                                .expect("Canonical var must exist");
                            current_var_to_vn.insert(dest.clone(), existing_vn);
                            new_instrs.push(Instr::Assign(dest.clone(), canonical_var.clone()));
                        } else {
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
                        current_var_to_vn.insert(dest.clone(), vn);
                        global_vn_to_canonical_var.insert(vn, dest.clone());
                        new_instrs.push(instr.clone());
                    }
                }
                Instr::Assign(dest, src) => {
                    if let Some(&src_vn) = current_var_to_vn.get(src) {
                        current_var_to_vn.insert(dest.clone(), src_vn);
                        new_instrs.push(instr.clone());
                    } else {
                        let vn = *next_vn;
                        *next_vn += 1;
                        current_var_to_vn.insert(dest.clone(), vn);
                        global_vn_to_canonical_var.insert(vn, dest.clone());
                        new_instrs.push(instr.clone());
                    }
                }
                Instr::Phi(dest, _) => {
                    if !current_var_to_vn.contains_key(dest) {
                        let vn = *next_vn;
                        *next_vn += 1;
                        current_var_to_vn.insert(dest.clone(), vn);
                        global_vn_to_canonical_var.insert(vn, dest.clone());
                    }
                    new_instrs.push(instr.clone());
                }
                _ => {
                    new_instrs.push(instr.clone());
                }
            }
        }
        (new_instrs, current_var_to_vn)
    }
}

impl Pass for GlobalValueNumbering {
    fn name(&self) -> &'static str {
        "global_value_numbering"
    }

    fn optimize(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        if instrs.is_empty() {
            return instrs;
        }

        // 1. Build CFG
        let mut blocks: HashMap<String, Vec<Instr>> = HashMap::new();
        let mut predecessors: HashMap<String, Vec<String>> = HashMap::new();
        let mut successors: HashMap<String, Vec<String>> = HashMap::new();
        let mut block_order: Vec<String> = Vec::new();

        let mut current_label: Option<String> = None;
        let mut current_instrs: Vec<Instr> = Vec::new();

        if !matches!(instrs.first(), Some(Instr::Label(_))) {
            panic!("IR must start with a Label instruction");
        }

        for instr in &instrs {
            if let Instr::Label(label) = instr {
                if let Some(prev_label) = current_label.take() {
                    if !current_instrs.is_empty() {
                        let last_instr_is_terminator = current_instrs.last().map_or(false, |li| {
                            matches!(li, Instr::Jump(_) | Instr::BranchIf(_, _, _))
                        });
                        if !last_instr_is_terminator {
                            successors
                                .entry(prev_label.clone())
                                .or_default()
                                .push(label.clone());
                            predecessors
                                .entry(label.clone())
                                .or_default()
                                .push(prev_label.clone());
                        }
                        blocks.insert(prev_label, current_instrs);
                    }
                }
                current_label = Some(label.clone());
                block_order.push(label.clone());
                predecessors.entry(label.clone()).or_default();
                successors.entry(label.clone()).or_default();
                current_instrs = vec![instr.clone()];
            } else {
                if let Some(ref label) = current_label {
                    current_instrs.push(instr.clone());
                    let mut terminated = false;
                    match instr {
                        Instr::Jump(target_label) => {
                            successors
                                .entry(label.clone())
                                .or_default()
                                .push(target_label.clone());
                            predecessors
                                .entry(target_label.clone())
                                .or_default()
                                .push(label.clone());
                            terminated = true;
                        }
                        Instr::BranchIf(_, true_label, false_label) => {
                            successors
                                .entry(label.clone())
                                .or_default()
                                .push(true_label.clone());
                            predecessors
                                .entry(true_label.clone())
                                .or_default()
                                .push(label.clone());
                            successors
                                .entry(label.clone())
                                .or_default()
                                .push(false_label.clone());
                            predecessors
                                .entry(false_label.clone())
                                .or_default()
                                .push(label.clone());
                            terminated = true;
                        }
                        _ => {}
                    }
                    if terminated {
                        blocks.insert(label.clone(), current_instrs);
                        current_label = None;
                        current_instrs = Vec::new();
                    }
                } else {
                    panic!("Internal error: Instruction processed before first label.");
                }
            }
        }
        if let Some(label) = current_label {
            if !current_instrs.is_empty() {
                blocks.insert(label, current_instrs);
            }
        }

        // 2. Initialize GVN Global State
        let mut global_expr_to_vn: HashMap<ExprKey, usize> = HashMap::new();
        let mut global_vn_to_canonical_var: HashMap<usize, String> = HashMap::new();
        let mut next_vn = 0;
        let mut block_entry_states: HashMap<String, HashMap<String, usize>> = HashMap::new();
        let mut block_exit_states: HashMap<String, HashMap<String, usize>> = HashMap::new();

        // Initialize states for all blocks before starting iteration
        for label in &block_order {
            block_entry_states.insert(label.clone(), HashMap::new());
            block_exit_states.insert(label.clone(), HashMap::new());
        }

        // 3. Worklist Iteration
        let mut worklist: VecDeque<String> = block_order.iter().cloned().collect();
        let mut optimized_blocks: HashMap<String, Vec<Instr>> = HashMap::new();

        while let Some(label) = worklist.pop_front() {
            let block_instrs = blocks.get(&label).expect("Block must exist in CFG");
            let block_preds = predecessors.get(&label).cloned().unwrap_or_default();

            // --- Compute Entry State via Merge (Phi-aware) ---
            let mut computed_entry_state: HashMap<String, usize> = HashMap::new();

            if !block_preds.is_empty() {
                // Identify variables defined by Phi nodes in this block
                let mut phi_defs = HashMap::new(); // dest -> Vec<(pred_label, source_var)>
                for instr in block_instrs.iter() {
                    if let Instr::Phi(dest, sources) = instr {
                        phi_defs.insert(dest.clone(), sources.clone());
                    }
                }

                // Process Phi nodes first
                for (dest, sources) in &phi_defs {
                    let mut incoming_vn: Option<usize> = None;
                    let mut first_source = true;
                    let mut disagreed = false;

                    for (pred_label, source_var) in sources {
                        if let Some(pred_exit_state) = block_exit_states.get(pred_label) {
                            if let Some(&source_vn) = pred_exit_state.get(source_var) {
                                if first_source {
                                    incoming_vn = Some(source_vn);
                                    first_source = false;
                                } else if incoming_vn != Some(source_vn) {
                                    disagreed = true;
                                    break; // Disagreement found
                                }
                            } else {
                                disagreed = true;
                                break;
                            }
                        } else {
                            disagreed = true;
                            break;
                        }
                    }

                    // Refined logic for assigning Phi VN
                    if disagreed {
                        // Disagreement among incoming VNs.
                        // Check previous iteration's entry state for this block (`label`).
                        if let Some(prev_vn) = block_entry_states
                            .get(&label)
                            .and_then(|prev_state| prev_state.get(dest))
                        {
                            // Reuse previous VN to aid convergence.
                            computed_entry_state.insert(dest.clone(), *prev_vn);
                        } else {
                            // No previous VN / first time disagreement. Assign a fresh VN.
                            let new_vn = next_vn;
                            next_vn += 1;
                            computed_entry_state.insert(dest.clone(), new_vn);
                            global_vn_to_canonical_var.insert(new_vn, dest.clone());
                        }
                    } else if let Some(vn) = incoming_vn {
                        // All known sources agree
                        computed_entry_state.insert(dest.clone(), vn);
                    } else {
                        // No known incoming VNs (e.g., unreachable code feeding phi?)
                        // Assign new VN. This might indicate earlier IR issues.
                        let new_vn = next_vn;
                        next_vn += 1;
                        computed_entry_state.insert(dest.clone(), new_vn);
                        global_vn_to_canonical_var.insert(new_vn, dest.clone());
                    }
                }

                // Merge non-Phi variables
                // Collect all variables defined in any predecessor's exit state
                let mut potential_vars = std::collections::HashSet::new();
                for pred_label in &block_preds {
                    if let Some(state) = block_exit_states.get(pred_label) {
                        potential_vars.extend(state.keys().cloned());
                    }
                }

                for var in potential_vars {
                    // Skip if already handled by a Phi node
                    if phi_defs.contains_key(&var) {
                        continue;
                    }

                    let mut agreed_vn: Option<usize> = None;
                    let mut first_pred = true;
                    let mut missing_or_disagreed = false;

                    for pred_label in &block_preds {
                        if let Some(pred_exit_state) = block_exit_states.get(pred_label) {
                            if let Some(&vn) = pred_exit_state.get(&var) {
                                if first_pred {
                                    agreed_vn = Some(vn);
                                    first_pred = false;
                                } else if agreed_vn != Some(vn) {
                                    missing_or_disagreed = true;
                                    break;
                                }
                            } else {
                                // Variable missing in this predecessor
                                missing_or_disagreed = true;
                                break;
                            }
                        } else {
                            // Should not happen with pre-initialization
                            missing_or_disagreed = true;
                            break;
                        }
                    }

                    if !missing_or_disagreed {
                        if let Some(vn) = agreed_vn {
                            computed_entry_state.insert(var.clone(), vn);
                        }
                    }
                }
            } // else: entry block, entry state remains empty

            // --- End Merge Logic ---

            // Check if entry state changed compared to previous iteration
            let old_entry_state = block_entry_states.get(&label).unwrap(); // Should exist due to init
            if old_entry_state == &computed_entry_state && optimized_blocks.contains_key(&label) {
                // If entry state hasn't changed and we already processed this block, skip.
                continue;
            }

            // Store the computed entry state
            block_entry_states.insert(label.clone(), computed_entry_state.clone());

            // --- Process the block ---
            let (new_block_instrs, exit_state) = self.process_basic_block(
                block_instrs,
                &computed_entry_state, // Pass calculated entry state
                &mut global_expr_to_vn,
                &mut global_vn_to_canonical_var,
                &mut next_vn,
            );
            optimized_blocks.insert(label.clone(), new_block_instrs);

            // --- Check exit state and update worklist ---
            let old_exit_state = block_exit_states.insert(label.clone(), exit_state.clone()); // Update and get old
            if old_exit_state.as_ref() != Some(&exit_state) {
                // Exit state changed (or was None before), add successors to worklist
                if let Some(succs) = successors.get(&label) {
                    for succ_label in succs {
                        if !worklist.contains(succ_label) {
                            worklist.push_back(succ_label.clone());
                        }
                    }
                }
            }
        }

        // 4. Flatten result
        let mut final_instrs = Vec::new();
        for label in &block_order {
            if let Some(opt_instrs) = optimized_blocks.get(label) {
                final_instrs.extend(opt_instrs.clone());
            } else {
                if let Some(orig_instrs) = blocks.get(label) {
                    // Fallback if block wasn't optimized (shouldn't happen with this worklist setup)
                    final_instrs.extend(orig_instrs.clone());
                }
            }
        }
        final_instrs
    }
}

// Helper structure to represent expressions canonically for hashing.
// Ensures commutativity for relevant operations.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum ExprKey {
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
            Op::Sub | Op::Div | Op::LShift | Op::RShift => ExprKey::BinOp(op, vn1, vn2),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::{CmpOp, Instr, Op};

    #[test]
    fn test_simple_redundancy_with_label() {
        let gvn = GlobalValueNumbering;
        let instrs = vec![
            Instr::Label("entry".to_string()), // Added Label
            Instr::Const("c1".to_string(), 1),
            Instr::Const("c2".to_string(), 2),
            Instr::BinOp(
                "t0".to_string(),
                "c1".to_string(),
                Op::Add,
                "c2".to_string(),
            ),
            Instr::BinOp(
                "t1".to_string(),
                "c1".to_string(),
                Op::Add,
                "c2".to_string(),
            ),
            Instr::Print("t1".to_string()),
        ];

        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("c1".to_string(), 1),
            Instr::Const("c2".to_string(), 2),
            Instr::BinOp(
                "t0".to_string(),
                "c1".to_string(),
                Op::Add,
                "c2".to_string(),
            ),
            Instr::Assign("t1".to_string(), "t0".to_string()), // t1 = t0
            Instr::Print("t1".to_string()),
        ];

        let optimized = gvn.optimize(instrs);
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_commutativity() {
        let gvn = GlobalValueNumbering;
        let instrs = vec![
            Instr::Label("entry".to_string()), // Added Label
            Instr::Const("c1".to_string(), 1),
            Instr::Const("c2".to_string(), 2),
            Instr::BinOp(
                "t0".to_string(),
                "c1".to_string(),
                Op::Add,
                "c2".to_string(),
            ), // t0 = 1 + 2
            Instr::BinOp(
                "t1".to_string(),
                "c2".to_string(),
                Op::Add,
                "c1".to_string(),
            ), // t1 = 2 + 1 (redundant due to commutativity)
            Instr::Print("t1".to_string()),
        ];

        let expected = vec![
            Instr::Label("entry".to_string()), // Added Label
            Instr::Const("c1".to_string(), 1),
            Instr::Const("c2".to_string(), 2),
            Instr::BinOp(
                "t0".to_string(),
                "c1".to_string(),
                Op::Add,
                "c2".to_string(),
            ), // t0 = 1 + 2
            Instr::Assign("t1".to_string(), "t0".to_string()), // t1 = t0
            Instr::Print("t1".to_string()),
        ];

        let optimized = gvn.optimize(instrs);
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_non_commutative() {
        let gvn = GlobalValueNumbering;
        let instrs = vec![
            Instr::Label("entry".to_string()), // Added Label
            Instr::Const("c3".to_string(), 3),
            Instr::Const("c1".to_string(), 1),
            Instr::BinOp(
                "t0".to_string(),
                "c3".to_string(),
                Op::Sub,
                "c1".to_string(),
            ), // t0 = 3 - 1
            Instr::BinOp(
                "t1".to_string(),
                "c1".to_string(),
                Op::Sub,
                "c3".to_string(),
            ), // t1 = 1 - 3 (NOT redundant)
            Instr::Print("t0".to_string()),
            Instr::Print("t1".to_string()),
        ];

        // Expected: Only Label added
        let expected = vec![
            Instr::Label("entry".to_string()), // Added Label
            Instr::Const("c3".to_string(), 3),
            Instr::Const("c1".to_string(), 1),
            Instr::BinOp(
                "t0".to_string(),
                "c3".to_string(),
                Op::Sub,
                "c1".to_string(),
            ),
            Instr::BinOp(
                "t1".to_string(),
                "c1".to_string(),
                Op::Sub,
                "c3".to_string(),
            ),
            Instr::Print("t0".to_string()),
            Instr::Print("t1".to_string()),
        ];
        let optimized = gvn.optimize(instrs);
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_redundant_comparison() {
        let gvn = GlobalValueNumbering;
        let instrs = vec![
            Instr::Label("entry".to_string()), // Added Label
            Instr::Const("c10".to_string(), 10),
            Instr::Const("c20".to_string(), 20),
            Instr::Cmp(
                "t0".to_string(),
                "c10".to_string(),
                CmpOp::Lt,
                "c20".to_string(),
            ), // t0 = 10 < 20
            Instr::Cmp(
                "t1".to_string(),
                "c10".to_string(),
                CmpOp::Lt,
                "c20".to_string(),
            ), // t1 = 10 < 20 (redundant)
            Instr::Print("t1".to_string()),
        ];

        let expected = vec![
            Instr::Label("entry".to_string()), // Added Label
            Instr::Const("c10".to_string(), 10),
            Instr::Const("c20".to_string(), 20),
            Instr::Cmp(
                "t0".to_string(),
                "c10".to_string(),
                CmpOp::Lt,
                "c20".to_string(),
            ), // t0 = 10 < 20
            Instr::Assign("t1".to_string(), "t0".to_string()), // t1 = t0
            Instr::Print("t1".to_string()),
        ];

        let optimized = gvn.optimize(instrs);
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_chained_redundancy() {
        let gvn = GlobalValueNumbering;
        let instrs = vec![
            Instr::Label("entry".to_string()), // Added Label
            Instr::Const("c1".to_string(), 1),
            Instr::Const("c2".to_string(), 2),
            Instr::Const("c3".to_string(), 3),
            Instr::BinOp("a".to_string(), "c1".to_string(), Op::Add, "c2".to_string()), // a = 1 + 2
            Instr::BinOp("b".to_string(), "a".to_string(), Op::Mul, "c3".to_string()),  // b = a * 3
            Instr::BinOp("c".to_string(), "c1".to_string(), Op::Add, "c2".to_string()), // c = 1 + 2 (redundant with a)
            Instr::BinOp("d".to_string(), "c".to_string(), Op::Mul, "c3".to_string()), // d = c * 3 (redundant with b)
            Instr::Print("d".to_string()),
        ];

        let expected = vec![
            Instr::Label("entry".to_string()), // Added Label
            Instr::Const("c1".to_string(), 1),
            Instr::Const("c2".to_string(), 2),
            Instr::Const("c3".to_string(), 3),
            Instr::BinOp("a".to_string(), "c1".to_string(), Op::Add, "c2".to_string()), // a = 1 + 2
            Instr::BinOp("b".to_string(), "a".to_string(), Op::Mul, "c3".to_string()),  // b = a * 3
            Instr::Assign("c".to_string(), "a".to_string()),                            // c = a
            Instr::Assign("d".to_string(), "b".to_string()),                            // d = b
            Instr::Print("d".to_string()),
        ];

        let optimized = gvn.optimize(instrs);
        assert_eq!(optimized, expected);
    }

    // Add more tests for CFGs, branches, merges, and Phi nodes later.
    #[test]
    fn test_cross_block_redundancy() {
        let gvn = GlobalValueNumbering;
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("c1".to_string(), 1),
            Instr::Const("c2".to_string(), 2),
            Instr::BinOp("a".to_string(), "c1".to_string(), Op::Add, "c2".to_string()), // a = 1 + 2
            Instr::Const("cond".to_string(), 1), // Assume true condition
            Instr::BranchIf("cond".to_string(), "then".to_string(), "else".to_string()),
            Instr::Label("then".to_string()),
            Instr::BinOp("b".to_string(), "c1".to_string(), Op::Add, "c2".to_string()), // REDUNDANT: b = 1 + 2
            Instr::Jump("merge".to_string()),
            Instr::Label("else".to_string()),
            Instr::BinOp("c".to_string(), "c1".to_string(), Op::Add, "c2".to_string()), // REDUNDANT: c = 1 + 2
            Instr::Jump("merge".to_string()),
            Instr::Label("merge".to_string()),
            Instr::Phi(
                "d".to_string(),
                vec![
                    ("then".to_string(), "b".to_string()),
                    ("else".to_string(), "c".to_string()),
                ],
            ),
            Instr::Print("d".to_string()),
        ];

        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("c1".to_string(), 1),
            Instr::Const("c2".to_string(), 2),
            Instr::BinOp("a".to_string(), "c1".to_string(), Op::Add, "c2".to_string()), // Original calculation
            Instr::Const("cond".to_string(), 1),
            Instr::BranchIf("cond".to_string(), "then".to_string(), "else".to_string()),
            Instr::Label("then".to_string()),
            Instr::Assign("b".to_string(), "a".to_string()), // Replaced with assignment
            Instr::Jump("merge".to_string()),
            Instr::Label("else".to_string()),
            Instr::Assign("c".to_string(), "a".to_string()), // Replaced with assignment
            Instr::Jump("merge".to_string()),
            Instr::Label("merge".to_string()),
            Instr::Phi(
                "d".to_string(),
                vec![
                    ("then".to_string(), "b".to_string()),
                    ("else".to_string(), "c".to_string()),
                ],
            ), // Phi remains
            Instr::Print("d".to_string()),
        ];

        let optimized = gvn.optimize(instrs);
        assert_eq!(optimized, expected);
    }
}
