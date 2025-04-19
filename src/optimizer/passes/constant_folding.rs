use std::collections::HashMap;

use crate::optimizer::{CmpOp, Instr, Op};

use super::pass::Pass;

/// Constant folding optimization pass
///
/// This pass replaces operations on constants with their computed results.
pub struct ConstantFolding;

impl Pass for ConstantFolding {
    fn optimize(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        let mut constants: HashMap<String, i64> = HashMap::new();
        let mut new_instrs = Vec::new();

        for instr in instrs {
            match instr {
                Instr::Const(name, val) => {
                    constants.insert(name.clone(), val);
                    new_instrs.push(Instr::Const(name, val));
                }
                Instr::BinOp(dest, left, op, right) => {
                    let lval = constants.get(&left);
                    let rval = constants.get(&right);
                    match (lval, rval) {
                        (Some(&lv), Some(&rv)) => {
                            let result = match op {
                                Op::Add => lv + rv,
                                Op::Sub => lv - rv,
                                Op::Mul => lv * rv,
                                Op::Div => lv / rv,
                                Op::BitAnd => lv & rv,
                                Op::BitOr => lv | rv,
                                Op::And => (lv > 0 && rv > 0) as i64,
                                Op::Or => (lv > 0 || rv > 0) as i64,
                                Op::LShift => lv << rv,
                                Op::RShift => lv >> rv,
                            };
                            constants.insert(dest.clone(), result);
                            new_instrs.push(Instr::Const(dest, result));
                        }
                        _ => {
                            constants.remove(&dest);
                            new_instrs.push(Instr::BinOp(dest, left, op, right));
                        }
                    }
                }
                Instr::Cmp(dest, left, cmp_op, right) => {
                    let lval = constants.get(&left);
                    let rval = constants.get(&right);
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
                            constants.insert(dest.clone(), result as i64);
                            new_instrs.push(Instr::Const(dest, result as i64));
                        }
                        _ => {
                            constants.remove(&dest);
                            new_instrs.push(Instr::Cmp(dest, left, cmp_op, right));
                        }
                    }
                }
                Instr::Phi(dest, preds) => {
                    // Collect constant values for each predecessor (if available)
                    let pred_constants: Vec<Option<i64>> = preds
                        .iter()
                        .map(|(_, pred_val)| constants.get(pred_val).copied())
                        .collect();

                    if pred_constants.iter().all(|val| val.is_some()) {
                        // If all predecessor values are constants:
                        let first_const = pred_constants[0].unwrap();

                        if pred_constants
                            .iter()
                            .all(|&val| val.unwrap() == first_const)
                        {
                            // All constants identical; fold into a single constant
                            constants.insert(dest.clone(), first_const);
                            new_instrs.push(Instr::Const(dest.clone(), first_const));
                        } else {
                            // Different constants; can't fold, keep the phi
                            constants.remove(&dest);
                            new_instrs.push(Instr::Phi(dest.clone(), preds.clone()));
                        }
                    } else {
                        // At least one predecessor is non-constant; can't fold
                        constants.remove(&dest);
                        new_instrs.push(Instr::Phi(dest.clone(), preds.clone()));
                    }
                }
                Instr::Print(name) => {
                    new_instrs.push(Instr::Print(name));
                }
                other => new_instrs.push(other),
            }
        }

        new_instrs
    }

    fn name(&self) -> &'static str {
        "ConstantFolding"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::{Instr, Op};

    #[test]
    fn test_constant_folding() {
        let instrs = vec![
            Instr::Const("t0".to_string(), 2),
            Instr::Const("t1".to_string(), 3),
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Add,
                "t1".to_string(),
            ),
            Instr::Print("t2".to_string()),
        ];

        let pass = ConstantFolding;
        let optimized = pass.optimize(instrs);

        let expected = vec![
            Instr::Const("t0".to_string(), 2),
            Instr::Const("t1".to_string(), 3),
            Instr::Const("t2".to_string(), 5),
            Instr::Print("t2".to_string()),
        ];

        assert_eq!(optimized, expected);
    }
}
