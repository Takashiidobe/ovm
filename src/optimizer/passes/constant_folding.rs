use std::collections::HashMap;

use crate::optimizer::{CmpOp, Instr, Op, CFG}; // Added CFG, BasicBlock

use super::pass::Pass;

/// Constant folding optimization pass
///
/// This pass replaces operations on constants with their computed results within each basic block.
pub struct ConstantFolding;

impl Pass for ConstantFolding {
    fn optimize(&self, mut cfg: CFG) -> CFG { // Changed signature to use CFG
        for bb in cfg.blocks.values_mut() { // Iterate through mutable basic blocks
            let mut constants: HashMap<String, i64> = HashMap::new();
            let mut new_instrs = Vec::new();

            for instr in &bb.instrs { // Iterate over instructions in the block
                match instr {
                    Instr::Const(name, val) => {
                        constants.insert(name.clone(), *val);
                        new_instrs.push(instr.clone()); // Keep the original const instruction
                    }
                    Instr::BinOp(dest, left, op, right) => {
                        let lval = constants.get(left);
                        let rval = constants.get(right);
                        match (lval, rval) {
                            (Some(&lv), Some(&rv)) => {
                                // Handle potential division by zero or other runtime errors if necessary
                                let result = match op {
                                    Op::Add => lv.wrapping_add(rv), // Use wrapping ops for safety
                                    Op::Sub => lv.wrapping_sub(rv),
                                    Op::Mul => lv.wrapping_mul(rv),
                                    Op::Div => if rv != 0 { lv / rv } else { 0 }, // Avoid panic
                                    Op::BitAnd => lv & rv,
                                    Op::BitOr => lv | rv,
                                    Op::And => (lv != 0 && rv != 0) as i64, // Use != 0 for boolean logic
                                    Op::Or => (lv != 0 || rv != 0) as i64,
                                    Op::Shl => lv.checked_shl(rv as u32).unwrap_or(0), // Handle potential overflow
                                    Op::Shr => lv.checked_shr(rv as u32).unwrap_or(0),
                                    Op::Mod => if rv != 0 { lv % rv } else { 0 }, // Avoid panic
                                };
                                constants.insert(dest.clone(), result);
                                new_instrs.push(Instr::Const(dest.clone(), result));
                            }
                            _ => {
                                constants.remove(dest); // Invalidate destination if operands aren't constant
                                new_instrs.push(instr.clone());
                            }
                        }
                    }
                    Instr::Cmp(dest, left, cmp_op, right) => {
                        let lval = constants.get(left);
                        let rval = constants.get(right);
                        match (lval, rval) {
                            (Some(&lv), Some(&rv)) => {
                                let result = match cmp_op {
                                    CmpOp::Eq => lv == rv,
                                    CmpOp::Neq => lv != rv,
                                    CmpOp::Lt => lv < rv,
                                    CmpOp::Lte => lv <= rv,
                                    CmpOp::Gt => lv > rv,
                                    CmpOp::Gte => lv >= rv,
                                };
                                let result_val = result as i64;
                                constants.insert(dest.clone(), result_val);
                                new_instrs.push(Instr::Const(dest.clone(), result_val));
                            }
                            _ => {
                                constants.remove(dest); // Invalidate destination
                                new_instrs.push(instr.clone());
                            }
                        }
                    }
                    Instr::Phi(dest, preds) => {
                        // Basic Phi handling: If all incoming values *known within this block*
                        // are the same constant, fold it. This is limited as it doesn't
                        // track constants across block boundaries effectively.
                        let mut first_const: Option<i64> = None;
                        let mut all_same = true;
                        let mut all_const = true;

                        for (_, pred_val_name) in preds {
                            match constants.get(pred_val_name) {
                                Some(&val) => {
                                    if first_const.is_none() {
                                        first_const = Some(val);
                                    } else if first_const != Some(val) {
                                        all_same = false;
                                        break; // No need to check further if different constants found
                                    }
                                }
                                None => {
                                    all_const = false;
                                    break; // Found a non-constant predecessor value
                                }
                            }
                        }

                        if all_const && all_same && first_const.is_some() {
                            let const_val = first_const.unwrap();
                            constants.insert(dest.clone(), const_val);
                            new_instrs.push(Instr::Const(dest.clone(), const_val));
                        } else {
                            // Cannot fold the Phi node based on current block's constants
                            constants.remove(dest); // Invalidate destination
                            new_instrs.push(instr.clone());
                        }
                    }
                    // Instructions that define a variable but aren't foldable yet
                    Instr::FuncParam { name, .. } | Instr::Call { target: name, ..} => {
                         constants.remove(name); // Invalidate the destination variable
                         new_instrs.push(instr.clone());
                    }
                    // Instructions that don't define a variable or are terminals
                    _ => {
                        new_instrs.push(instr.clone());
                    }
                }
            }
            // Update the block's instructions
            bb.instrs = new_instrs;
        }

        cfg // Return the modified CFG
    }

    fn name(&self) -> &'static str {
        "ConstantFolding"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::Op;
    use crate::optimizer::passes::test_helpers::*; // Import helpers

    #[test]
    fn test_constant_folding_cfg() {
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("t0", 2),
                cnst("t1", 3),
                binop("t2", "t0", Op::Add, "t1"),
                print("t2"),
            ], vec![], vec![]) // No predecessors or successors for a single block test
        ]);

        let pass = ConstantFolding;
        let optimized_cfg = pass.optimize(cfg);

        let expected_instrs = vec![
            cnst("t0", 2),
            cnst("t1", 3),
            cnst("t2", 5), // The BinOp is replaced by Const
            print("t2"),
        ];

        let optimized_block = optimized_cfg.blocks.get("entry").unwrap();
        assert_eq!(optimized_block.instrs, expected_instrs);
    }

     #[test]
    fn test_constant_folding_phi_simple() {
        // Test case where a Phi node can be folded because both inputs
        // are the same constant defined within the *same* block.
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("c1", 10),
                cnst("c2", 10),
                phi("phi_res", vec![("pred1", "c1"), ("pred2", "c2")]),
                print("phi_res"),
            ], vec!["pred1", "pred2"], vec![]) // Define predecessors for the block
        ]);

        // Note: We need dummy predecessor blocks in the CFG for Phi nodes to be meaningful,
        // but for this specific test focusing on *intra-block* folding,
        // just declaring the predecessors in the block metadata is sufficient
        // to satisfy the structure expected by the pass. A more complex test
        // would involve multiple blocks.

        let pass = ConstantFolding;
        let optimized_cfg = pass.optimize(cfg);

        let expected_instrs = vec![
            cnst("c1", 10),
            cnst("c2", 10),
            cnst("phi_res", 10), // Phi folded
            print("phi_res"),
        ];

        let optimized_block = optimized_cfg.blocks.get("entry").unwrap();
        assert_eq!(optimized_block.instrs, expected_instrs);
    }

    #[test]
    fn test_constant_folding_invalidate() {
        // Test case where a non-constant operation invalidates a variable
         let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("t0", 5),
                func_param("t1", 0), // t1 is not constant
                binop("t2", "t0", Op::Add, "t1"), // This cannot be folded
                binop("t3", "t2", Op::Mul, "t0"), // t2 is not constant, so this cannot be folded either
                print("t3"),
            ], vec![], vec![])
        ]);


        let pass = ConstantFolding;
        let optimized_cfg = pass.optimize(cfg);

        // Expected: Only the Const instruction remains, others are kept as is
        // because their inputs weren't both constant at the time of processing.
        let expected_instrs = vec![
            cnst("t0", 5),
            func_param("t1", 0),
            binop("t2", "t0", Op::Add, "t1"),
            binop("t3", "t2", Op::Mul, "t0"),
            print("t3"),
        ];


        let optimized_block = optimized_cfg.blocks.get("entry").unwrap();
        assert_eq!(optimized_block.instrs, expected_instrs);
    }
}