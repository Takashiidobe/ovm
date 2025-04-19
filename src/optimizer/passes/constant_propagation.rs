use std::collections::HashMap;

use crate::optimizer::Instr; // Import Op and CmpOp for tests

use super::pass::Pass;

/// Constant propagation optimization pass
///
/// This pass replaces uses of variables that have constant values with the constants themselves.
pub struct ConstantPropagation;

impl Pass for ConstantPropagation {
    fn optimize(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        let mut constants: HashMap<String, i64> = HashMap::new();
        let mut new_instrs = Vec::with_capacity(instrs.len());

        for instr in instrs {
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
                    // Check source operand
                    if let Some(&val) = constants.get(src) {
                        *src = val.to_string();
                    }
                }
                Instr::Phi(_, preds) => {
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
                // Jump, Label don't have variable operands to propagate into
                // Const defines a constant, no operands to propagate into
                Instr::Jump(_) | Instr::Label(_) | Instr::Const(_, _) => {}
            }

            // Update constants map based on the result of the instruction
            match &current_instr {
                Instr::Const(name, val) => {
                    constants.insert(name.clone(), *val);
                }
                Instr::Assign(dest, src) => {
                    // Check if source is a literal number (string) after propagation
                    if let Ok(val) = src.parse::<i64>() {
                        constants.insert(dest.clone(), val);
                        // We could potentially change this Assign to a Const instruction here,
                        // but maybe better left for constant folding or copy propagation later.
                        // current_instr = Instr::Const(dest.clone(), val);
                    } else if let Some(&val) = constants.get(src) {
                        // If source is a variable still holding a constant
                        constants.insert(dest.clone(), val);
                    } else {
                        // Result is not constant
                        constants.remove(dest);
                    }
                }
                // Instructions that define a destination variable whose value isn't known constant here
                Instr::BinOp(dest, ..) | Instr::Cmp(dest, ..) | Instr::Phi(dest, ..) => {
                    // The destination's value depends on an operation, not directly a constant.
                    // Constant folding might make it constant later.
                    constants.remove(dest);
                }
                // Instructions that don't define a variable we track constants for
                Instr::Print(_) | Instr::BranchIf(_, _, _) | Instr::Jump(_) | Instr::Label(_) => {
                    // No constant assignment to track.
                    // Note: Calls are missing from Instr enum, if added, they might invalidate constants.
                }
            }

            new_instrs.push(current_instr);
        }

        new_instrs
    }

    fn name(&self) -> &'static str {
        "ConstantPropagation"
    }
}

// Basic tests (will need refinement based on actual IR structure)
#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::{Instr, Op}; // Ensure Op, CmpOp are available

    #[test]
    fn test_basic_propagation() {
        let instrs = vec![
            Instr::Const("c".to_string(), 10),
            Instr::Assign("x".to_string(), "c".to_string()), // x = c (should become x = 10 in src)
            Instr::BinOp(
                "y".to_string(),
                "x".to_string(), // Should become 10
                Op::Add,
                "c".to_string(), // Should become 10
            ), // y = x + c (should become y = 10 + 10)
            Instr::Print("y".to_string()),                   // print y
        ];

        let pass = ConstantPropagation;
        let optimized = pass.optimize(instrs);

        let expected = vec![
            Instr::Const("c".to_string(), 10),
            // Assign source 'c' becomes "10"
            Instr::Assign("x".to_string(), "10".to_string()),
            Instr::BinOp(
                "y".to_string(),
                // BinOp operands 'x' and 'c' become "10"
                "10".to_string(),
                Op::Add,
                "10".to_string(),
            ),
            // Print operand 'y' is not known constant by this pass
            Instr::Print("y".to_string()),
        ];

        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_propagation_chain() {
        let instrs = vec![
            Instr::Const("a".to_string(), 5),
            Instr::Assign("b".to_string(), "a".to_string()), // b = a -> b = 5
            Instr::Assign("c".to_string(), "b".to_string()), // c = b -> c = 5
            Instr::Print("c".to_string()),                   // print c
        ];

        let pass = ConstantPropagation;
        let optimized = pass.optimize(instrs);

        let expected = vec![
            Instr::Const("a".to_string(), 5),
            Instr::Assign("b".to_string(), "5".to_string()), // Propagated 'a'
            Instr::Assign("c".to_string(), "5".to_string()), // Propagated 'b' which got 5
            Instr::Print("5".to_string()),                   // 'c' is propagated here too
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_redefinition_kills_propagation() {
        let instrs = vec![
            Instr::Const("x".to_string(), 1),
            Instr::Assign("y".to_string(), "x".to_string()), // y = x -> y = 1
            Instr::Const("z".to_string(), 100),              // Define z
            Instr::Assign("x".to_string(), "z".to_string()), // x = z -> x = 100 (x is now 100, previous constant is dead in map)
            Instr::BinOp("w".to_string(), "y".to_string(), Op::Add, "x".to_string()), // w = y + x -> w = 1 + 100
            Instr::Print("w".to_string()),
        ];

        let pass = ConstantPropagation;
        let optimized = pass.optimize(instrs);

        let expected = vec![
            Instr::Const("x".to_string(), 1),
            Instr::Assign("y".to_string(), "1".to_string()), // Propagated original x=1
            Instr::Const("z".to_string(), 100),
            Instr::Assign("x".to_string(), "100".to_string()), // Propagated z=100
            Instr::BinOp("w".to_string(), "1".to_string(), Op::Add, "100".to_string()), // Propagated y=1 and new x=100
            Instr::Print("w".to_string()), // w is not constant yet
        ];

        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_branch_condition_propagation() {
        let instrs = vec![
            Instr::Const("cond".to_string(), 0), // Represents false
            Instr::BranchIf("cond".to_string(), "L1".to_string(), "L2".to_string()),
            Instr::Label("L1".to_string()),
            Instr::Print("never".to_string()),
            Instr::Jump("L3".to_string()),
            Instr::Label("L2".to_string()),
            Instr::Print("always".to_string()),
            Instr::Label("L3".to_string()),
        ];

        let pass = ConstantPropagation;
        let optimized = pass.optimize(instrs);

        let expected = vec![
            Instr::Const("cond".to_string(), 0),
            // Condition "cond" is replaced with "0"
            Instr::BranchIf("0".to_string(), "L1".to_string(), "L2".to_string()),
            Instr::Label("L1".to_string()),
            Instr::Print("never".to_string()), // Print operand not affected
            Instr::Jump("L3".to_string()),
            Instr::Label("L2".to_string()),
            Instr::Print("always".to_string()), // Print operand not affected
            Instr::Label("L3".to_string()),
        ];

        assert_eq!(optimized, expected);
    }

    // Add tests for Phi nodes if needed
}
