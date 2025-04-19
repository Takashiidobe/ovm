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

        // --- Pass 2: Mark Required Labels (Targets + Fallthrough) ---
        let mut required_labels: HashSet<String> = HashSet::new();
        for (_label, (_start, end)) in &blocks {
            if *end == 0 || *end > instrs.len() {
                continue;
            } // Skip empty/invalid blocks
            let last_instr_idx = end - 1;
            if last_instr_idx >= instrs.len() {
                continue;
            }
            let last_instr = &instrs[last_instr_idx];

            match last_instr {
                Instr::BranchIf(_, then_lbl, else_lbl) => {
                    required_labels.insert(then_lbl.clone());
                    required_labels.insert(else_lbl.clone());
                }
                Instr::Jump(target_lbl) => {
                    required_labels.insert(target_lbl.clone());
                }
                _ => {
                    // Fallthrough case
                    if *end < instrs.len() {
                        if let Some(fallthrough_label) = instr_to_label.get(end) {
                            if blocks
                                .get(fallthrough_label)
                                .map_or(false, |(s, _)| *s == *end)
                            {
                                required_labels.insert(fallthrough_label.clone());
                            }
                        }
                    }
                }
            }
        }
        if let Some(entry_label) = instr_to_label.get(&0) {
            // Ensure entry label is kept
            required_labels.insert(entry_label.clone());
        }

        // --- Pass 3: Mark Essential Instructions using Worklist ---
        let mut essential_instrs: HashSet<usize> = HashSet::new();
        let mut live_vars: HashSet<String> = HashSet::new();
        let mut worklist: Vec<String> = Vec::new(); // Variables whose definitions need to be marked essential

        // Initialize essential instructions and live vars
        for (idx, instr) in instrs.iter().enumerate() {
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
                Instr::Label(label) => {
                    if required_labels.contains(label) {
                        is_essential = true;
                    }
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
                if essential_instrs.insert(*def_idx) {
                    // If the defining instr wasn't already essential
                    let defining_instr = &instrs[*def_idx];
                    let uses = Self::get_instr_uses(defining_instr); // Assign Vec<String> directly
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
}
