// filepath: /home/takashi/ovm/src/optimizer/passes/constant_propagation.rs
use std::collections::HashMap;

use crate::optimizer::{CFG, Instr}; // Import CFG

use super::pass::Pass;

/// Constant propagation optimization pass (Intra-block)
///
/// This pass replaces uses of variables that have constant values with the constants themselves,
/// operating independently within each basic block.
pub struct ConstantPropagation;

impl Pass for ConstantPropagation {
    // Updated signature to work with CFG
    fn optimize(&self, cfg: CFG) -> CFG {
        let mut new_cfg = cfg.clone(); // Clone the CFG to modify it

        for block in new_cfg.blocks.values_mut() {
            let mut constants: HashMap<String, i64> = HashMap::new();
            let mut new_instrs = Vec::with_capacity(block.instrs.len());

            for instr in &block.instrs { // Iterate over original instructions
                // Clone instruction to potentially modify operands
                let mut current_instr = instr.clone();

                // Propagate constants for operands before processing the instruction itself
                match &mut current_instr {
                    Instr::BinOp(_, left, _, right) => {
                        if let Some(&val) = constants.get(left) {
                            *left = val.to_string();
                        }
                        if let Some(&val) = constants.get(right) {
                            *right = val.to_string();
                        }
                    }
                    Instr::Cmp(_, left, _, right) => {
                        if let Some(&val) = constants.get(left) {
                            *left = val.to_string();
                        }
                        if let Some(&val) = constants.get(right) {
                            *right = val.to_string();
                        }
                    }
                    Instr::Assign(_dest, src) => {
                        if let Some(&val) = constants.get(src) {
                            *src = val.to_string();
                        }
                    }
                    Instr::Phi(_, preds) => {
                        // Note: For intra-block, Phi propagation is limited.
                        // A full dataflow analysis would be needed for inter-block.
                        for (_, pred_val) in preds.iter_mut() {
                            if let Some(&val) = constants.get(pred_val) {
                                *pred_val = val.to_string();
                            }
                        }
                    }
                    Instr::Print(name) => {
                        if let Some(&val) = constants.get(name) {
                            *name = val.to_string();
                        }
                    }
                    Instr::BranchIf(cond, _, _) => {
                        if let Some(&val) = constants.get(cond) {
                            *cond = val.to_string();
                        }
                    }
                    Instr::FuncParam { .. } => {
                        // No operands to propagate into
                    }
                    Instr::Ret { value } => {
                        if let Some(v) = value {
                            if let Some(&val) = constants.get(v) {
                                *v = val.to_string();
                            }
                        }
                    }
                    Instr::Call {
                        target: _,
                        args,
                        result: _,
                    } => {
                        for arg in args.iter_mut() {
                            if let Some(&val) = constants.get(arg) {
                                *arg = val.to_string();
                            }
                        }
                    }
                    Instr::Jump(_) | Instr::Label(_) | Instr::Const(_, _) => {}
                }

                // Update constants map based on the result of the instruction
                match &current_instr {
                    Instr::Const(name, val) => {
                        constants.insert(name.clone(), *val);
                    }
                    Instr::Assign(dest, src) => {
                        if let Ok(val) = src.parse::<i64>() {
                            constants.insert(dest.clone(), val);
                        } else if let Some(&val) = constants.get(src) {
                            constants.insert(dest.clone(), val);
                        } else {
                            constants.remove(dest);
                        }
                    }
                    Instr::Call {
                        target: _,
                        args: _,
                        result,
                    } => {
                        if let Some(res_var) = result {
                            constants.remove(res_var);
                        }
                    }
                    Instr::BinOp(dest, ..) | Instr::Cmp(dest, ..) | Instr::Phi(dest, ..) => {
                        constants.remove(dest);
                    }
                    Instr::FuncParam { name, .. } => {
                        constants.remove(name);
                    }
                    Instr::Print(_)
                    | Instr::BranchIf(_, _, _)
                    | Instr::Jump(_)
                    | Instr::Label(_)
                    | Instr::Ret { .. } => {}
                }

                new_instrs.push(current_instr);
            }
            // Replace the block's instructions with the optimized ones
            block.instrs = new_instrs;
        }

        new_cfg
    }

    fn name(&self) -> &'static str {
        "ConstantPropagation"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::passes::test_helpers::*; // Import helpers
    use crate::optimizer::{Op, CmpOp}; // Keep Op, CmpOp

    #[test]
    fn test_basic_propagation() {
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("c", 10),
                assign("x", "c"), // x = c
                binop("y", "x", Op::Add, "c"), // y = x + c
                print("y"),
            ],
            vec![], // predecessors
            vec![], // successors
        )]);

        let pass = ConstantPropagation;
        let optimized_cfg = pass.optimize(cfg);

        let expected_cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("c", 10),
                assign("x", "10"), // Propagated
                binop("y", "10", Op::Add, "10"), // Propagated
                print("y"), // y is not constant within the block
            ],
            vec![],
            vec![],
        )]);

        assert_eq!(optimized_cfg, expected_cfg);
    }

    #[test]
    fn test_propagation_chain() {
         let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("a", 5),
                assign("b", "a"), // b = a
                assign("c", "b"), // c = b
                print("c"),
            ],
            vec![],
            vec![],
        )]);

        let pass = ConstantPropagation;
        let optimized_cfg = pass.optimize(cfg);

        let expected_cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("a", 5),
                assign("b", "5"), // Propagated
                assign("c", "5"), // Propagated
                print("5"),       // Propagated
            ],
            vec![],
            vec![],
        )]);
        assert_eq!(optimized_cfg, expected_cfg);
    }

    #[test]
    fn test_redefinition_kills_propagation() {
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("x", 1),
                assign("y", "x"), // y = x -> y = 1
                cnst("z", 100),
                assign("x", "z"), // x = z -> x = 100
                binop("w", "y", Op::Add, "x"), // w = y + x
                print("w"),
            ],
            vec![],
            vec![],
        )]);

        let pass = ConstantPropagation;
        let optimized_cfg = pass.optimize(cfg);

        let expected_cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("x", 1),
                assign("y", "1"), // Propagated original x=1
                cnst("z", 100),
                assign("x", "100"), // Propagated z=100
                binop("w", "1", Op::Add, "100"), // Propagated y=1 and new x=100
                print("w"), // w is not constant yet
            ],
            vec![],
            vec![],
        )]);

        assert_eq!(optimized_cfg, expected_cfg);
    }

    #[test]
    fn test_branch_condition_propagation() {
        // CFG: entry -> L1 -> L3, entry -> L2 -> L3
        let cfg = create_test_cfg(vec![
            (
                "entry",
                vec![
                    cnst("cond", 0), // Represents false
                    branch("cond", "L1", "L2"),
                ],
                vec![],
                vec!["L1", "L2"],
            ),
            (
                "L1",
                vec![print("never"), jump("L3")],
                vec!["entry"],
                vec!["L3"],
            ),
            (
                "L2",
                vec![print("always"), jump("L3")],
                vec!["entry"],
                vec!["L3"],
            ),
            ("L3", vec![ret()], vec!["L1", "L2"], vec![]),
        ]);

        let pass = ConstantPropagation;
        let optimized_cfg = pass.optimize(cfg);

        // Expected: Condition in entry block is propagated
        let expected_cfg = create_test_cfg(vec![
            (
                "entry",
                vec![
                    cnst("cond", 0),
                    branch("0", "L1", "L2"), // Propagated
                ],
                vec![],
                vec!["L1", "L2"],
            ),
            ( // L1 is unchanged by this pass
                "L1",
                vec![print("never"), jump("L3")],
                vec!["entry"],
                vec!["L3"],
            ),
            ( // L2 is unchanged by this pass
                "L2",
                vec![print("always"), jump("L3")],
                vec!["entry"],
                vec!["L3"],
            ),
            ( // L3 is unchanged by this pass
                "L3",
                vec![ret()],
                vec!["L1", "L2"],
                vec![],
            ),
        ]);

        assert_eq!(optimized_cfg, expected_cfg);
    }

    #[test]
    fn test_call_arg_propagation() {
         let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("arg1", 42),
                cnst("arg2", 99),
                call("foo", vec!["arg1", "arg2"], Some("res")),
                print("res"),
            ],
            vec![],
            vec![],
        )]);

        let pass = ConstantPropagation;
        let optimized_cfg = pass.optimize(cfg);

        let expected_cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("arg1", 42),
                cnst("arg2", 99),
                call("foo", vec!["42", "99"], Some("res")), // Args propagated
                print("res"), // Result not constant
            ],
            vec![],
            vec![],
        )]);
        assert_eq!(optimized_cfg, expected_cfg);
    }

     #[test]
    fn test_ret_val_propagation() {
         let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("ret_val", 100),
                ret_val("ret_val"), // Use new helper
            ],
            vec![],
            vec![],
        )]);

        let pass = ConstantPropagation;
        let optimized_cfg = pass.optimize(cfg);

        let expected_cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("ret_val", 100),
                ret_val("100"), // Propagated
            ],
            vec![],
            vec![],
        )]);
        assert_eq!(optimized_cfg, expected_cfg);
    }

    #[test]
    fn test_call_kills_result_constant() {
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("res", 1), // res = 1
                call("foo", vec![], Some("res")), // Call redefines res
                assign("other", "res"), // other = res
            ],
            vec![],
            vec![],
        )]);

        let pass = ConstantPropagation;
        let optimized_cfg = pass.optimize(cfg);

        let expected_cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("res", 1),
                call("foo", vec![], Some("res")),
                assign("other", "res"), // 'res' not propagated
            ],
            vec![],
            vec![],
        )]);
        assert_eq!(optimized_cfg, expected_cfg);
    }

    // Add a test case spanning multiple blocks to show intra-block nature
    #[test]
    fn test_intra_block_limit() {
        let cfg = create_test_cfg(vec![
            (
                "entry",
                vec![cnst("a", 10), jump("next")],
                vec![],
                vec!["next"],
            ),
            (
                "next",
                vec![assign("b", "a"), print("b")], // 'a' is not known constant *in this block*
                vec!["entry"],
                vec![],
            ),
        ]);

        let pass = ConstantPropagation;
        let optimized_cfg = pass.optimize(cfg);

        // Expected: 'a' is not propagated into 'next' block
        let expected_cfg = create_test_cfg(vec![
             (
                "entry",
                vec![cnst("a", 10), jump("next")], // Unchanged
                vec![],
                vec!["next"],
            ),
            (
                "next",
                vec![assign("b", "a"), print("b")], // Unchanged, 'a' not propagated
                vec!["entry"],
                vec![],
            ),
        ]);

        assert_eq!(optimized_cfg, expected_cfg);
    }
}