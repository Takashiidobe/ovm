use super::pass::Pass;
use crate::optimizer::{CFG, Instr};
use std::collections::{HashMap, HashSet};

/// Dead code elimination optimization pass
/// This pass removes instructions that don't contribute to program output.
pub struct DeadCodeElimination;

impl DeadCodeElimination {
    fn get_instr_uses(instr: &Instr) -> Vec<String> {
        match instr {
            Instr::BinOp(_, left, _, right) => vec![left.clone(), right.clone()],
            Instr::Print(var) => vec![var.clone()],
            Instr::Cmp(_, left, _, right) => vec![left.clone(), right.clone()],
            Instr::BranchIf(cond, _, _) => vec![cond.clone()],
            Instr::Phi(_, preds) => preds.iter().map(|(_, val)| val.clone()).collect(),
            Instr::Assign(_, src) => vec![src.clone()],
            // --- Additions for functions ---
            Instr::Call {
                target: _,
                args,
                result: _,
            } => args.clone(), // Uses are the arguments
            Instr::Ret { value } => value.as_ref().cloned().map_or(vec![], |v| vec![v]), // Use is the return value (if any)
            Instr::FuncParam { name, .. } => vec![name.clone()], // FuncParam defines its name
            // --- End Additions ---
            Instr::Const(_, _) | Instr::Jump(_) | Instr::Label(_) => vec![], // No variable uses
        }
    }

    fn get_instr_defs(instr: &Instr) -> Vec<String> {
        match instr {
            Instr::Const(d, _)
            | Instr::BinOp(d, _, _, _)
            | Instr::Cmp(d, _, _, _)
            | Instr::Phi(d, _)
            | Instr::Assign(d, _) => vec![d.clone()],
            Instr::Call {
                target: _,
                args: _,
                result,
            } => result.iter().cloned().collect(), // Definition is the result (if any)
            Instr::FuncParam { name, .. } => vec![name.clone()], // FuncParam defines its name
            Instr::Print(_)
            | Instr::BranchIf(_, _, _)
            | Instr::Jump(_)
            | Instr::Label(_)
            | Instr::Ret { .. } => vec![], // No definitions
        }
    }

    /// Returns if a block is essential (must be kept)
    fn is_block_essential(label: &str, cfg: &CFG) -> bool {
        // Entry block is always essential
        if Some(label.to_string()) == cfg.current_block {
            return true;
        }

        // Check if any other block branches to this one
        cfg.blocks
            .values()
            .any(|block| block.succs.contains(&label.to_string()))
    }
}

impl Pass for DeadCodeElimination {
    fn optimize(&self, mut cfg: CFG) -> CFG {
        // First pass: collect essential instructions and live variables
        let mut essential_instrs: HashMap<String, HashSet<usize>> = HashMap::new();
        let mut live_vars: HashSet<String> = HashSet::new();
        let mut worklist: Vec<(String, String)> = Vec::new(); // (block_label, var_name)
        let mut var_defs: HashMap<String, (String, usize)> = HashMap::new(); // var -> (block, idx)

        // Initialize by finding all variable definitions and essential instructions
        for (label, block) in &cfg.blocks {
            let block_essential = Self::is_block_essential(label, &cfg);
            let mut block_essentials = HashSet::new();

            for (idx, instr) in block.instrs.iter().enumerate() {
                // Record variable definitions
                for def in Self::get_instr_defs(instr) {
                    var_defs.insert(def, (label.clone(), idx));
                }

                // Determine if instruction is essential
                let is_essential = block_essential
                    && match instr {
                        Instr::Print(_)
                        | Instr::BranchIf(_, _, _)
                        | Instr::Jump(_)
                        | Instr::Call { .. }
                        | Instr::Ret { .. } => true,
                        _ => false,
                    };

                if is_essential {
                    block_essentials.insert(idx);
                    // Add uses of essential instructions to worklist
                    for used_var in Self::get_instr_uses(instr) {
                        if live_vars.insert(used_var.clone()) {
                            worklist.push((label.clone(), used_var));
                        }
                    }
                }
            }

            if !block_essentials.is_empty() {
                essential_instrs.insert(label.clone(), block_essentials);
            }
        }

        // Propagate liveness backward
        while let Some((block_label, var)) = worklist.pop() {
            if let Some((def_block, def_idx)) = var_defs.get(&var) {
                let block_essentials = essential_instrs.entry(def_block.clone()).or_default();

                if block_essentials.insert(*def_idx) {
                    // Definition became essential, add its uses to worklist
                    if let Some(def_instr) = cfg
                        .blocks
                        .get(def_block)
                        .and_then(|b| b.instrs.get(*def_idx))
                    {
                        for used_var in Self::get_instr_uses(def_instr) {
                            for used_var in Self::get_instr_uses(def_instr) {
                                if live_vars.insert(used_var.clone()) {
                                    worklist.push((def_block.clone(), used_var));
                                }
                            }
                        }
                    }
                }
            }
        }

        // Filter out dead instructions
        for (label, block) in cfg.blocks.iter_mut() {
            if let Some(essential_indices) = essential_instrs.get(label) {
                block.instrs = block
                    .instrs
                    .iter()
                    .enumerate()
                    .filter(|(i, _)| essential_indices.contains(i))
                    .map(|(_, instr)| instr.clone())
                    .collect();
            } else {
                block.instrs.clear(); // No essential instructions in this block
            }
        }

        cfg
    }

    fn name(&self) -> &'static str {
        "DeadCodeElimination"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::Op;
    use crate::optimizer::passes::test_helpers::*;

    #[test]
    fn test_dead_code_elimination() {
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("t0", 10),
                cnst("t1", 20),
                binop("t2", "t0", Op::Add, "t1"),
                cnst("t_unused", 999), // dead
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        let pass = DeadCodeElimination;
        let optimized = pass.optimize(cfg);

        let expected = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("t0", 10),
                cnst("t1", 20),
                binop("t2", "t0", Op::Add, "t1"),
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_dead_code_elimination_chain() {
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("t0", 5),                    // dead
                cnst("t1", 10),                   // dead
                binop("t2", "t0", Op::Add, "t1"), // dead
                cnst("t5", 20),                   // used
                print("t5"),
            ],
            vec![],
            vec![],
        )]);

        let pass = DeadCodeElimination;
        let optimized = pass.optimize(cfg);

        let expected = create_test_cfg(vec![(
            "entry",
            vec![cnst("t5", 20), print("t5")],
            vec![],
            vec![],
        )]);

        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_dead_block_elimination() {
        let cfg = create_test_cfg(vec![
            (
                "entry",
                vec![cnst("cond", 0), branch("cond", "dead", "live")],
                vec![],
                vec!["dead", "live"],
            ),
            (
                "dead",
                vec![cnst("unused", 1), print("unused")],
                vec!["entry"],
                vec!["exit"],
            ),
            (
                "live",
                vec![cnst("used", 2), print("used")],
                vec!["entry"],
                vec!["exit"],
            ),
            ("exit", vec![ret()], vec!["dead", "live"], vec![]),
        ]);

        let pass = DeadCodeElimination;
        let optimized = pass.optimize(cfg);

        assert!(optimized.blocks.contains_key("live"));
        assert!(optimized.blocks.contains_key("exit"));
        assert_eq!(
            optimized.blocks.get("dead").map(|b| b.instrs.len()),
            Some(0)
        );
    }

    #[test]
    fn test_keep_essential_calls() {
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("arg", 42),
                call("foo", vec!["arg"], Some("result")), // call is essential
                cnst("unused", 99),                       // dead
            ],
            vec![],
            vec![],
        )]);

        let pass = DeadCodeElimination;
        let optimized = pass.optimize(cfg);

        let expected = create_test_cfg(vec![(
            "entry",
            vec![cnst("arg", 42), call("foo", vec!["arg"], Some("result"))],
            vec![],
            vec![],
        )]);

        assert_eq!(optimized, expected);
    }
}
