use std::collections::HashMap;

use crate::optimizer::passes::pass::Pass;
use crate::optimizer::{Instr, Op, CFG, BasicBlock}; // Added CFG, BasicBlock
use indexmap::IndexMap; // Assuming CFG uses IndexMap

/// Algebraic Simplification Pass (CFG Version)
///
/// Simplifies binary operations based on algebraic identities involving 0 and 1
/// within each basic block.
/// Examples:
/// - x + 0 => x
/// - x * 1 => x
/// - x * 0 => 0
/// - x - x => 0
pub struct AlgebraicSimplification;

impl Pass for AlgebraicSimplification {
    // Updated signature to work with CFG
    fn optimize(&self, cfg: CFG) -> CFG {
        let mut optimized_blocks = IndexMap::new();

        for (label, block) in cfg.blocks {
            let mut optimized_instrs = Vec::with_capacity(block.instrs.len());
            // Track constants locally within this block
            let mut constants: HashMap<String, i64> = HashMap::new();

            for instr in block.instrs { // Iterate over original instructions
                match instr {
                    Instr::Const(ref name, val) => {
                        constants.insert(name.clone(), val);
                        optimized_instrs.push(instr.clone());
                    }
                    Instr::BinOp(ref dest, ref left, op, ref right) => {
                        let left_val = constants.get(left).copied();
                        let right_val = constants.get(right).copied();
                        let mut simplified_instr: Option<Instr> = None;

                        match op {
                            Op::Add => {
                                if right_val == Some(0) { // x + 0 => x
                                    simplified_instr = Some(Instr::Assign(dest.clone(), left.clone()));
                                } else if left_val == Some(0) { // 0 + x => x
                                    simplified_instr = Some(Instr::Assign(dest.clone(), right.clone()));
                                }
                            }
                            Op::Sub => {
                                if right_val == Some(0) { // x - 0 => x
                                    simplified_instr = Some(Instr::Assign(dest.clone(), left.clone()));
                                } else if left == right { // x - x => 0
                                    simplified_instr = Some(Instr::Const(dest.clone(), 0));
                                }
                            }
                            Op::Mul => {
                                if right_val == Some(1) { // x * 1 => x
                                    simplified_instr = Some(Instr::Assign(dest.clone(), left.clone()));
                                } else if left_val == Some(1) { // 1 * x => x
                                    simplified_instr = Some(Instr::Assign(dest.clone(), right.clone()));
                                } else if right_val == Some(0) || left_val == Some(0) { // x * 0 or 0 * x => 0
                                    simplified_instr = Some(Instr::Const(dest.clone(), 0));
                                }
                            }
                            Op::Div => {
                                if right_val == Some(1) { // x / 1 => x
                                    simplified_instr = Some(Instr::Assign(dest.clone(), left.clone()));
                                }
                            }
                            Op::Shl | Op::Shr => {
                                if right_val == Some(0) { // x << 0 => x, x >> 0 => x
                                    simplified_instr = Some(Instr::Assign(dest.clone(), left.clone()));
                                }
                            }
                            _ => {} // No other simplifications implemented
                        }

                        if let Some(simplified) = simplified_instr {
                            // Update constants map based on the *new* instruction
                            if let Instr::Const(_, val) = &simplified {
                                constants.insert(dest.clone(), *val);
                            } else if let Instr::Assign(_, src) = &simplified {
                                if let Some(&val) = constants.get(src) {
                                    constants.insert(dest.clone(), val);
                                } else {
                                    constants.remove(dest); // Source wasn't constant
                                }
                            } else {
                                constants.remove(dest); // Should not happen for current simplifications
                            }
                            optimized_instrs.push(simplified);
                        } else {
                            // If not simplified, keep original, invalidate dest constant
                            constants.remove(dest);
                            optimized_instrs.push(instr.clone());
                        }
                    }
                    Instr::Assign(ref dest, ref src) => {
                        // Propagate constants through assignments
                        if let Some(&val) = constants.get(src) {
                            constants.insert(dest.clone(), val);
                        } else {
                            constants.remove(dest);
                        }
                        optimized_instrs.push(instr.clone());
                    }
                    // Instructions that define a register invalidate it in the constants map
                    Instr::Cmp(ref name, ..) |
                    Instr::Phi(ref name, ..) |
                    Instr::Call { result: Some(ref name), .. } |
                    Instr::FuncParam { ref name, .. } => {
                        constants.remove(name); // Use 'name' for FuncParam
                        optimized_instrs.push(instr.clone());
                    }
                    // Other instructions are kept as is
                    _ => {
                        optimized_instrs.push(instr.clone());
                    }
                }
            }

            // Create a new block with the optimized instructions
            let optimized_block = BasicBlock {
                label: block.label, // Keep original label
                instrs: optimized_instrs,
                preds: block.preds, // Keep original predecessors
                succs: block.succs, // Keep original successors
            };
            optimized_blocks.insert(label, optimized_block);
        }

        // Return a new CFG with the optimized blocks
        CFG {
            blocks: optimized_blocks,
            current_block: None, // Reset transient state
        }
    }

    fn name(&self) -> &'static str {
        "AlgebraicSimplification"
    }
}

// Note: get_defined_register helper is no longer needed as we handle
// definition invalidation directly in the main match.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::{CFG, BasicBlock, Instr, Op};
    use indexmap::IndexMap;
    use std::collections::HashSet;

    // Helper to create a simple CFG for testing (same as in identity_elimination)
    fn create_test_cfg(blocks_data: Vec<(&str, Vec<Instr>, Vec<&str>, Vec<&str>)>) -> CFG {
        let mut blocks = IndexMap::new();
        for (label, instrs, preds, succs) in blocks_data {
            blocks.insert(
                label.to_string(),
                BasicBlock {
                    label: label.to_string(),
                    instrs,
                    preds: preds.into_iter().map(|s| s.to_string()).collect::<HashSet<_>>().into_iter().collect(),
                    succs: succs.into_iter().map(|s| s.to_string()).collect::<HashSet<_>>().into_iter().collect(),
                },
            );
        }
        CFG { blocks, current_block: None }
    }

    // Helper instruction creators
    fn assign(dest: &str, src: &str) -> Instr { Instr::Assign(dest.to_string(), src.to_string()) }
    fn cnst(dest: &str, val: i64) -> Instr { Instr::Const(dest.to_string(), val) }
    fn binop(dest: &str, l: &str, op: Op, r: &str) -> Instr { Instr::BinOp(dest.to_string(), l.to_string(), op, r.to_string()) }
    fn jump(target: &str) -> Instr { Instr::Jump(target.to_string()) }
    fn ret() -> Instr { Instr::Ret{value: None} }

    #[test]
    fn test_add_zero_cfg() {
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("c0", 0),
                assign("t1", "x"), // Assume x defined before entry
                binop("t2", "t1", Op::Add, "c0"), // t2 = t1 + 0
                jump("exit"),
            ], vec![], vec!["exit"]),
            ("exit", vec![ret()], vec!["entry"], vec![]),
        ]);
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(cfg);
        let expected = create_test_cfg(vec![
            ("entry", vec![
                cnst("c0", 0),
                assign("t1", "x"),
                assign("t2", "t1"), // Simplified
                jump("exit"),
            ], vec![], vec!["exit"]),
            ("exit", vec![ret()], vec!["entry"], vec![]),
        ]);
        assert_eq!(optimized.blocks["entry"].instrs, expected.blocks["entry"].instrs);
    }

    #[test]
    fn test_sub_self_cfg() {
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                assign("t1", "x"),
                binop("t2", "t1", Op::Sub, "t1"), // t2 = t1 - t1
                jump("exit"),
            ], vec![], vec!["exit"]),
            ("exit", vec![ret()], vec!["entry"], vec![]),
        ]);
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(cfg);
        let expected = create_test_cfg(vec![
            ("entry", vec![
                assign("t1", "x"),
                cnst("t2", 0), // Simplified
                jump("exit"),
            ], vec![], vec!["exit"]),
            ("exit", vec![ret()], vec!["entry"], vec![]),
        ]);
        assert_eq!(optimized.blocks["entry"].instrs, expected.blocks["entry"].instrs);
    }

     #[test]
    fn test_mul_one_cfg() {
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("c1", 1),
                assign("t1", "x"),
                binop("t2", "t1", Op::Mul, "c1"), // t2 = t1 * 1
                jump("exit"),
            ], vec![], vec!["exit"]),
            ("exit", vec![ret()], vec!["entry"], vec![]),
        ]);
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(cfg);
        let expected = create_test_cfg(vec![
            ("entry", vec![
                cnst("c1", 1),
                assign("t1", "x"),
                assign("t2", "t1"), // Simplified
                jump("exit"),
            ], vec![], vec!["exit"]),
            ("exit", vec![ret()], vec!["entry"], vec![]),
        ]);
        assert_eq!(optimized.blocks["entry"].instrs, expected.blocks["entry"].instrs);
    }

    #[test]
    fn test_mul_zero_cfg() {
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("c0", 0),
                assign("t1", "x"),
                binop("t2", "t1", Op::Mul, "c0"), // t2 = t1 * 0
                jump("exit"),
            ], vec![], vec!["exit"]),
            ("exit", vec![ret()], vec!["entry"], vec![]),
        ]);
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(cfg);
        let expected = create_test_cfg(vec![
            ("entry", vec![
                cnst("c0", 0),
                assign("t1", "x"),
                cnst("t2", 0), // Simplified
                jump("exit"),
            ], vec![], vec!["exit"]),
            ("exit", vec![ret()], vec!["entry"], vec![]),
        ]);
        assert_eq!(optimized.blocks["entry"].instrs, expected.blocks["entry"].instrs);
    }

    #[test]
    fn test_shift_zero_cfg() {
         let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("c0", 0),
                assign("t1", "x"),
                binop("t2", "t1", Op::Shl, "c0"), // t2 = t1 << 0
                binop("t3", "t1", Op::Shr, "c0"), // t3 = t1 >> 0
                jump("exit"),
            ], vec![], vec!["exit"]),
            ("exit", vec![ret()], vec!["entry"], vec![]),
        ]);
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(cfg);
        let expected = create_test_cfg(vec![
            ("entry", vec![
                cnst("c0", 0),
                assign("t1", "x"),
                assign("t2", "t1"), // Simplified
                assign("t3", "t1"), // Simplified
                jump("exit"),
            ], vec![], vec!["exit"]),
            ("exit", vec![ret()], vec!["entry"], vec![]),
        ]);
        assert_eq!(optimized.blocks["entry"].instrs, expected.blocks["entry"].instrs);
    }

    #[test]
    fn test_multiple_blocks_cfg_algebraic() {
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("c0", 0),
                cnst("c1", 1),
                assign("x", "c1"), // x = 1
                binop("y", "x", Op::Add, "c0"), // y = x + 0 => y = x
                jump("next"),
            ], vec![], vec!["next"]),
            ("next", vec![
                assign("z", "y"), // z = y (which is x, which is 1)
                binop("a", "z", Op::Mul, "c1"), // a = z * 1 => a = z
                binop("b", "a", Op::Sub, "a"), // b = a - a => b = 0
                jump("exit"),
            ], vec!["entry"], vec!["exit"]),
            ("exit", vec![ret()], vec!["next"], vec![]),
        ]);
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(cfg);
        let expected = create_test_cfg(vec![
             ("entry", vec![
                cnst("c0", 0),
                cnst("c1", 1),
                assign("x", "c1"),
                assign("y", "x"), // Simplified
                jump("next"),
            ], vec![], vec!["next"]),
            ("next", vec![
                assign("z", "y"),
                assign("a", "z"), // Simplified
                cnst("b", 0), // Simplified
                jump("exit"),
            ], vec!["entry"], vec!["exit"]),
            ("exit", vec![ret()], vec!["next"], vec![]),
        ]);

        assert_eq!(optimized.blocks["entry"].instrs, expected.blocks["entry"].instrs, "Block 'entry' mismatch");
        assert_eq!(optimized.blocks["next"].instrs, expected.blocks["next"].instrs, "Block 'next' mismatch");
        assert_eq!(optimized.blocks["exit"].instrs, expected.blocks["exit"].instrs, "Block 'exit' mismatch");
    }
}