use std::collections::HashMap;

use crate::optimizer::{Instr, Op};
use crate::optimizer::passes::pass::Pass;

/// Algebraic Simplification Pass
///
/// Simplifies binary operations based on algebraic identities involving 0 and 1.
/// Examples:
/// - x + 0 => x
/// - x * 1 => x
/// - x * 0 => 0
/// - x - x => 0
pub struct AlgebraicSimplification;


impl Pass for AlgebraicSimplification {
    fn optimize(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        let mut optimized_instrs = Vec::with_capacity(instrs.len());
        let mut constants: HashMap<String, i64> = HashMap::new();

        for instr in instrs {
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
                           // Note: 0 - x cannot be simplified further here easily
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
                            // Note: 0 / x => 0 requires x != 0, hard to guarantee here.
                            // Note: x / x => 1 requires x != 0.
                        }
                        Op::Shl | Op::Shr => { // x << 0 => x, x >> 0 => x
                             if right_val == Some(0) {
                                simplified_instr = Some(Instr::Assign(dest.clone(), left.clone()));
                             }
                        }
                        // No algebraic simplifications for BitAnd, BitOr, And, Or implemented
                        _ => {}
                    }

                    if let Some(simplified) = simplified_instr {
                        // If simplified, update constants map based on the *new* instruction
                        if let Instr::Const(_, val) = &simplified {
                            constants.insert(dest.clone(), *val);
                        } else if let Instr::Assign(_, src) = &simplified {
                             if let Some(&val) = constants.get(src) {
                                constants.insert(dest.clone(), val);
                            } else {
                                constants.remove(dest);
                            }
                        } else {
                             constants.remove(dest);
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
                _ => {
                    // For other instructions, invalidate potential definitions
                    if let Some(def_reg) = get_defined_register(&instr) {
                        constants.remove(&def_reg);
                    }
                    optimized_instrs.push(instr.clone());
                }
            }
        }

        optimized_instrs
    }

    fn name(&self) -> &'static str {
        "AlgebraicSimplification"
    }
}

// Helper (can be shared or duplicated from other passes)
fn get_defined_register(instr: &Instr) -> Option<String> {
    match instr {
        Instr::Const(d, _) |
        Instr::BinOp(d, _, _, _) |
        Instr::Cmp(d, _, _, _) |
        Instr::Phi(d, _) |
        Instr::Assign(d, _) => Some(d.clone()),
        _ => None,
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::{Instr, Op};

    #[test]
    fn test_add_zero() {
        let instrs = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("t1".to_string(), "x".to_string()), // Assume x is defined
            Instr::BinOp("t2".to_string(), "t1".to_string(), Op::Add, "c0".to_string()), // t2 = t1 + 0
        ];
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::Assign("t2".to_string(), "t1".to_string()), // Simplified to t2 = t1
        ];
        assert_eq!(optimized, expected);
    }

     #[test]
    fn test_zero_add() {
        let instrs = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("t1".to_string(), "x".to_string()), // Assume x is defined
            Instr::BinOp("t2".to_string(), "c0".to_string(), Op::Add, "t1".to_string()), // t2 = 0 + t1
        ];
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::Assign("t2".to_string(), "t1".to_string()), // Simplified to t2 = t1
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_sub_zero() {
        let instrs = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::BinOp("t2".to_string(), "t1".to_string(), Op::Sub, "c0".to_string()), // t2 = t1 - 0
        ];
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::Assign("t2".to_string(), "t1".to_string()), // Simplified to t2 = t1
        ];
        assert_eq!(optimized, expected);
    }

     #[test]
    fn test_sub_self() {
        let instrs = vec![
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::BinOp("t2".to_string(), "t1".to_string(), Op::Sub, "t1".to_string()), // t2 = t1 - t1
        ];
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::Const("t2".to_string(), 0), // Simplified to t2 = 0
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_mul_one() {
        let instrs = vec![
            Instr::Const("c1".to_string(), 1),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::BinOp("t2".to_string(), "t1".to_string(), Op::Mul, "c1".to_string()), // t2 = t1 * 1
        ];
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Const("c1".to_string(), 1),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::Assign("t2".to_string(), "t1".to_string()), // Simplified to t2 = t1
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_one_mul() {
         let instrs = vec![
            Instr::Const("c1".to_string(), 1),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::BinOp("t2".to_string(), "c1".to_string(), Op::Mul, "t1".to_string()), // t2 = 1 * t1
        ];
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Const("c1".to_string(), 1),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::Assign("t2".to_string(), "t1".to_string()), // Simplified to t2 = t1
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_mul_zero() {
        let instrs = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::BinOp("t2".to_string(), "t1".to_string(), Op::Mul, "c0".to_string()), // t2 = t1 * 0
        ];
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::Const("t2".to_string(), 0), // Simplified to t2 = 0
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_zero_mul() {
        let instrs = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::BinOp("t2".to_string(), "c0".to_string(), Op::Mul, "t1".to_string()), // t2 = 0 * t1
        ];
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::Const("t2".to_string(), 0), // Simplified to t2 = 0
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_div_one() {
        let instrs = vec![
            Instr::Const("c1".to_string(), 1),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::BinOp("t2".to_string(), "t1".to_string(), Op::Div, "c1".to_string()), // t2 = t1 / 1
        ];
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Const("c1".to_string(), 1),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::Assign("t2".to_string(), "t1".to_string()), // Simplified to t2 = t1
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_shift_zero() {
        let instrs = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::BinOp("t2".to_string(), "t1".to_string(), Op::Shl, "c0".to_string()), // t2 = t1 << 0
            Instr::BinOp("t3".to_string(), "t1".to_string(), Op::Shr, "c0".to_string()), // t3 = t1 >> 0
        ];
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::Assign("t2".to_string(), "t1".to_string()), // Simplified to t2 = t1
            Instr::Assign("t3".to_string(), "t1".to_string()), // Simplified to t3 = t1
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_no_simplification() {
        let instrs = vec![
            Instr::Const("c2".to_string(), 2),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::BinOp("t2".to_string(), "t1".to_string(), Op::Add, "c2".to_string()),
        ];
        let pass = AlgebraicSimplification;
        let original_instrs = instrs.clone();
        let optimized = pass.optimize(instrs);
        assert_eq!(optimized, original_instrs);
    }

    #[test]
    fn test_simplification_constant_propagation() {
         // Test that simplification works when constant comes via Assign
         let instrs = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("zero".to_string(), "c0".to_string()),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::BinOp("t2".to_string(), "t1".to_string(), Op::Add, "zero".to_string()), // t2 = t1 + zero (0)
        ];
        let pass = AlgebraicSimplification;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Const("c0".to_string(), 0),
            Instr::Assign("zero".to_string(), "c0".to_string()),
            Instr::Assign("t1".to_string(), "x".to_string()),
            Instr::Assign("t2".to_string(), "t1".to_string()), // Simplified to t2 = t1
        ];
        assert_eq!(optimized, expected);
    }

} 