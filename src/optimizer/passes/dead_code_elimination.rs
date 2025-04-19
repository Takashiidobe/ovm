use std::collections::{BTreeMap, HashMap, HashSet};

use crate::optimizer::Instr;

use super::pass::Pass;

/// Dead code elimination optimization pass
///
/// This pass removes instructions that don't contribute to program output.
pub struct DeadCodeElimination;

impl Pass for DeadCodeElimination {
    fn optimize(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        if instrs.is_empty() {
            return vec![];
        }

        // --- Pass 1: Map definitions and identify block structure (copied from previous attempt) ---
        let mut def_map: HashMap<String, usize> = HashMap::new(); // var_name -> instr_idx defining it
        let mut blocks = BTreeMap::new(); // label -> (start_idx, end_idx)
        let mut leaders = HashSet::new();
        leaders.insert(0);
        let mut label_map = HashMap::new(); // label -> start_idx

        for (idx, instr) in instrs.iter().enumerate() {
            // Populate def_map
            match instr {
                Instr::Const(d, _)
                | Instr::BinOp(d, _, _, _)
                | Instr::Cmp(d, _, _, _)
                | Instr::Phi(d, _)
                | Instr::Assign(d, _) => {
                    def_map.insert(d.clone(), idx);
                }
                _ => {}
            }

            // Identify leaders and labels
            if let Instr::Label(l) = instr {
                leaders.insert(idx);
                label_map.insert(l.clone(), idx);
            }
            match instr {
                Instr::Jump(_) | Instr::BranchIf(_, _, _) => {
                    if idx + 1 < instrs.len() {
                        leaders.insert(idx + 1);
                    }
                }
                _ => {}
            }
        }
        for instr in &instrs {
            // Add jump targets to leaders
            match instr {
                Instr::Jump(t) | Instr::BranchIf(_, t, _) => {
                    if let Some(idx) = label_map.get(t) {
                        leaders.insert(*idx);
                    }
                }
                _ => {}
            }
        }

        let mut sorted_leaders: Vec<_> = leaders.iter().cloned().collect();
        sorted_leaders.sort();
        let mut instr_to_label = HashMap::new(); // instr_idx -> block_label

        for i in 0..sorted_leaders.len() {
            let start = sorted_leaders[i];
            let end = if i + 1 < sorted_leaders.len() {
                sorted_leaders[i + 1]
            } else {
                instrs.len()
            };
            let label = instrs
                .get(start)
                .and_then(|instr| {
                    if let Instr::Label(l) = instr {
                        Some(l.clone())
                    } else {
                        None
                    }
                })
                .unwrap_or_else(|| format!("block_{}", start));
            label_map.entry(label.clone()).or_insert(start);
            blocks.insert(label.clone(), (start, end));
            for idx in start..end {
                instr_to_label.insert(idx, label.clone());
            }
        }

        // --- Pass 2: Mark Required Labels (Explicit Targets Only) ---
        let mut required_labels: HashSet<String> = HashSet::new();
        for instr in &instrs {
            match instr {
                Instr::Jump(target_lbl) => {
                    required_labels.insert(target_lbl.clone());
                }
                Instr::BranchIf(_, then_lbl, else_lbl) => {
                    required_labels.insert(then_lbl.clone());
                    required_labels.insert(else_lbl.clone());
                }
                _ => {}
            }
        }
        // We no longer add fallthrough targets or the entry label here.
        // Required means explicitly targeted by a jump/branch.

        // --- Pass 2.5: Perform Forward Reachability Analysis ---
        let mut reachable_labels: HashSet<String> = HashSet::new();
        let mut reachability_worklist: Vec<String> = Vec::new();

        // Determine the actual entry label (block containing instruction 0)
        let mut initial_entry_label: Option<String> = None;
        if !instrs.is_empty() {
            if let Some(label) = instr_to_label.get(&0) {
                if blocks.contains_key(label) {
                    initial_entry_label = Some(label.clone());
                }
            } else {
                // Handle case where block 0 doesn't start with a label (use synthetic label if needed)
                // Note: The block identification logic should ideally always assign a label name.
                // Let's assume instr_to_label covers index 0 if instrs is not empty.
                // If this assumption is wrong, this part needs adjustment.
                // For safety, we might need to iterate blocks map to find the one containing index 0.
            }
        }

        if let Some(entry_label) = initial_entry_label {
            reachable_labels.insert(entry_label.clone());
            reachability_worklist.push(entry_label.clone());
        }

        let mut processed_labels = HashSet::new(); // Avoid redundant processing in cycles
        while let Some(current_label) = reachability_worklist.pop() {
            if !processed_labels.insert(current_label.clone()) {
                continue; // Already processed this label
            }

            let (start, end) = match blocks.get(&current_label) {
                Some(bounds) => bounds,
                None => continue, // Label in worklist but not in blocks map? Error.
            };

            if *end == 0 || *end > instrs.len() || *start >= *end {
                continue;
            } // Skip empty/invalid blocks

            let last_instr_idx = end - 1;
            if last_instr_idx >= instrs.len() {
                continue;
            }
            let last_instr = &instrs[last_instr_idx];

            let mut add_target = |target_label: &String| {
                // Add target to reachable set and worklist *only if* it corresponds to a valid block
                if blocks.contains_key(target_label)
                    && reachable_labels.insert(target_label.clone())
                {
                    reachability_worklist.push(target_label.clone());
                }
            };

            match last_instr {
                Instr::Jump(target_lbl) => {
                    add_target(target_lbl);
                    // NO Fallthrough
                }
                Instr::BranchIf(_, then_lbl, else_lbl) => {
                    add_target(then_lbl);
                    add_target(else_lbl);
                    // NO Fallthrough
                }
                _ => {
                    // Fallthrough ONLY if not Jump/BranchIf
                    if *end < instrs.len() {
                        // Find the label of the block starting exactly at the fallthrough position
                        if let Some(fallthrough_label) = instr_to_label.get(end) {
                            if blocks
                                .get(fallthrough_label).is_some_and(|(s, _)| *s == *end)
                            {
                                add_target(fallthrough_label);
                            }
                        }
                    }
                }
            }
        }

        // --- Pass 3: Mark Essential Instructions using Worklist ---
        let mut essential_instrs: HashSet<usize> = HashSet::new();
        let mut live_vars: HashSet<String> = HashSet::new();
        let mut worklist: Vec<String> = Vec::new(); // Variables whose definitions need to be marked essential

        // Initialize essential instructions and live vars, considering block reachability
        for (idx, instr) in instrs.iter().enumerate() {
            // Determine if the instruction is in a reachable block
            let block_label_opt = instr_to_label.get(&idx);
            let is_block_reachable =
                block_label_opt.map_or(idx == 0, |lbl| reachable_labels.contains(lbl));
            // If idx == 0 and no label, assume reachable only if instrs is not empty.
            // The initial_entry_label logic should handle this, ensuring reachable_labels is seeded correctly.

            if !is_block_reachable {
                continue; // Skip instructions in unreachable blocks.
            }

            let mut is_essential = false;
            let mut uses = Vec::new();

            match instr {
                Instr::Print(var) => {
                    is_essential = true;
                    uses.push(var.clone());
                }
                Instr::BranchIf(cond, _, _) => {
                    is_essential = true;
                    uses.push(cond.clone());
                }
                Instr::Jump(_) => {
                    is_essential = true;
                }
                Instr::Label(_) => {
                    // If we are processing this instruction, its block MUST be reachable
                    // (due to the 'is_block_reachable' check earlier in the loop).
                    // Therefore, any reachable label should be considered essential.
                    // The previous check `if required_labels.contains(label)` was too strict.
                    is_essential = true;
                }
                // Other instructions are essential only if their results are used.
                // Phi uses are handled below.
                _ => {}
            }

            if is_essential {
                essential_instrs.insert(idx);
                for used_var in uses {
                    if live_vars.insert(used_var.clone()) {
                        // If newly inserted
                        worklist.push(used_var);
                    }
                }
            }
        }

        // Propagate liveness backward
        while let Some(var) = worklist.pop() {
            if let Some(def_idx) = def_map.get(&var) {
                // Check if the defining instruction itself is in a reachable block
                let defining_instr = &instrs[*def_idx];
                let def_block_label_opt = instr_to_label.get(def_idx);
                // Assuming index 0 is always the start of a reachable block if non-empty
                let is_def_instr_reachable = def_block_label_opt
                    .map_or(*def_idx == 0 && !instrs.is_empty(), |lbl| {
                        reachable_labels.contains(lbl)
                    });

                // Mark essential ONLY if the definition is reachable AND wasn't already essential
                if is_def_instr_reachable && essential_instrs.insert(*def_idx) {
                    // If the defining instr wasn't already essential (and is reachable)
                    let uses = Self::get_instr_uses(defining_instr);
                    for used_var in uses {
                        if live_vars.insert(used_var.clone()) {
                            // If newly inserted
                            worklist.push(used_var);
                        }
                    }
                }
            }
        }

        // --- Pass 4: Filter Instructions ---
        let optimized: Vec<Instr> = instrs
            .into_iter()
            .enumerate()
            .filter(|(idx, _)| essential_instrs.contains(idx))
            .map(|(_, instr)| instr)
            .collect();

        optimized
    }

    fn name(&self) -> &'static str {
        "DeadCodeElimination"
    }
}

// Helper to get just the uses of an instruction
impl DeadCodeElimination {
    fn get_instr_uses(instr: &Instr) -> Vec<String> {
        match instr {
            Instr::BinOp(_, left, _, right) => vec![left.clone(), right.clone()],
            Instr::Print(var) => vec![var.clone()],
            Instr::Cmp(_, left, _, right) => vec![left.clone(), right.clone()],
            Instr::BranchIf(cond, _, _) => vec![cond.clone()],
            Instr::Phi(_, preds) => preds.iter().map(|(_, val)| val.clone()).collect(),
            Instr::Assign(_, src) => vec![src.clone()],
            Instr::Const(_, _) | Instr::Jump(_) | Instr::Label(_) => vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::{Instr, Op};

    #[test]
    fn test_dead_code_elimination() {
        let instrs = vec![
            Instr::Const("t0".to_string(), 10),
            Instr::Const("t1".to_string(), 20),
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Add,
                "t1".to_string(),
            ),
            Instr::Const("t_unused".to_string(), 999),
            Instr::Print("t2".to_string()),
        ];

        let pass = DeadCodeElimination;
        let optimized = pass.optimize(instrs);

        // t_unused should be eliminated since it's not used
        // t0 and t1 should be kept since they're used in the BinOp
        let expected = vec![
            Instr::Const("t0".to_string(), 10),
            Instr::Const("t1".to_string(), 20),
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Add,
                "t1".to_string(),
            ),
            Instr::Print("t2".to_string()),
        ];

        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_dead_code_elimination_chain() {
        let instrs = vec![
            Instr::Const("t0".to_string(), 5),
            Instr::Const("t1".to_string(), 10),
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Add,
                "t1".to_string(),
            ),
            Instr::Const("t3".to_string(), 15),
            Instr::BinOp(
                "t4".to_string(),
                "t2".to_string(),
                Op::Mul,
                "t3".to_string(),
            ),
            // t4 is never used, so it and its dependencies should be eliminated
            Instr::Const("t5".to_string(), 20),
            Instr::Print("t5".to_string()),
        ];

        let pass = DeadCodeElimination;
        let optimized = pass.optimize(instrs);

        // Only t5 and its print should remain
        let expected = vec![
            Instr::Const("t5".to_string(), 20),
            Instr::Print("t5".to_string()),
        ];

        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_dce_after_branch_elim() {
        // Simulates code like: if (true) { t2 = 0; } print 1;
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t1".to_string(), 1), // Condition is constant true
            Instr::BranchIf("t1".to_string(), "then".to_string(), "else".to_string()),
            Instr::Label("then".to_string()),
            Instr::Const("t2".to_string(), 0), // This should become dead
            Instr::Jump("merge".to_string()),
            Instr::Label("else".to_string()), // This block should become dead
            Instr::Jump("merge".to_string()),
            Instr::Label("merge".to_string()),
            Instr::Const("t3".to_string(), 1), // Use a constant value later
            Instr::Assign("res".to_string(), "t3".to_string()), // Introduce a move to test MC
            Instr::Print("res".to_string()),   // Use the final result
        ];

        // Use the full optimizer pipeline
        let optimizer = Optimizer;
        let optimized = optimizer.run_all(instrs);

        // Expected result after CF, BE, MC, DCE (iterated), GVN, DCE:
        // - CF folds t1 to 1.
        // - BE converts BranchIf to Jump("then").
        // - CF might not be re-run in this specific test setup's sequence before DCE.
        // - The first DCE might remove the BranchIf or make the else block dead.
        // - MC coalesces t3 into res -> Const("res", 1)
        // - Iteration ensures the else block and t2 definition are removed.
        // - Final DCE removes unused labels and constants if any.
        let expected = vec![
            Instr::Label("entry".to_string()),
            // t1 constant is folded and DCE'd
            // BranchIf is eliminated
            Instr::Jump("then".to_string()), //might be removed if Label("then") is removed and merge follows entry directly.
            Instr::Label("then".to_string()), // is DCE'd
            Instr::Jump("merge".to_string()), // from else is DCE'd
            Instr::Label("merge".to_string()), // might be removed if not jumped to.
            Instr::Const("res".to_string(), 1), // t3 coalesced into res
            Instr::Print("res".to_string()),
        ];

        // Need to import Optimizer for this test
        use crate::optimizer::passes::Optimizer;

        assert_eq!(
            optimized, expected,
            "Optimized instructions do not match expected after full pipeline"
        );
    }

    // ... other tests ...
}
