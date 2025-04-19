use std::collections::HashMap;

use crate::optimizer::Instr;
use crate::optimizer::passes::pass::Pass;

/// Copy Propagation Optimization Pass
///
/// Replaces uses of variables that are copies of other variables
/// with the original variable.
/// Example: If `y = x`, subsequent uses of `y` become uses of `x`.
pub struct CopyPropagation;

impl CopyPropagation {
    /// Recursively finds the original source variable in a chain of copies.
    /// e.g., if copies = { "y": "x", "z": "y" }, find_source("z", copies) returns "x".
    fn find_source<'a>(var: &'a String, copies: &'a HashMap<String, String>) -> &'a String {
        let mut current = var;
        while let Some(source) = copies.get(current) {
            // Added basic cycle detection: if source points back to var, stop.
            if source == var {
                eprintln!("Warning: Detected copy cycle involving {}", var);
                return current; // Return the variable itself in case of a cycle
            }
            current = source;
        }
        current
    }
}

impl Pass for CopyPropagation {
    fn optimize(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        let mut optimized_instrs = Vec::with_capacity(instrs.len());
        // Map from a variable to the variable it's a direct copy of.
        let mut copies: HashMap<String, String> = HashMap::new();

        for instr in instrs {
            let mut new_instr = instr.clone();

            // --- 1. Replace uses of variables with their sources ---
            match &mut new_instr {
                Instr::BinOp(_, left, _, right) => {
                    *left = Self::find_source(left, &copies).clone();
                    *right = Self::find_source(right, &copies).clone();
                }
                Instr::Cmp(_, left, _, right) => {
                    *left = Self::find_source(left, &copies).clone();
                    *right = Self::find_source(right, &copies).clone();
                }
                Instr::Assign(_, src) => {
                    // Important: Propagate the source *before* recording the copy.
                    *src = Self::find_source(src, &copies).clone();
                    // Handled below in step 2
                }
                Instr::Print(name) => {
                    *name = Self::find_source(name, &copies).clone();
                }
                Instr::BranchIf(cond, _, _) => {
                    *cond = Self::find_source(cond, &copies).clone();
                }
                Instr::Phi(_, preds) => {
                    for (_, pred_val_ssa) in preds.iter_mut() {
                        *pred_val_ssa = Self::find_source(pred_val_ssa, &copies).clone();
                    }
                }
                // Instr::Const, Instr::Label, Instr::Jump don't use variables in a way that needs replacement.
                _ => {}
            }

            // --- 2. Update copy map and potentially invalidate old copies ---
            // If the instruction defines a variable, it might invalidate existing copies
            // that point *to* this variable, and it might start a new copy chain.
            match &new_instr {
                Instr::Assign(dest, src) => {
                    // If src is the same as dest after propagation, this is a redundant assign (x = x)
                    // We will keep it for now; DCE can remove it.

                    // Record that `dest` is now a copy of `src` (which is already the ultimate source).
                    // If `dest` itself was copied previously, this overwrites that.
                    copies.insert(dest.clone(), src.clone());
                }
                Instr::Const(dest, _)
                | Instr::BinOp(dest, ..)
                | Instr::Cmp(dest, ..)
                | Instr::Phi(dest, _) => {
                    // This instruction defines `dest`. Any previous copy information
                    // *about* `dest` (i.e., `dest = some_other_var`) is now invalid.
                    // Remove `dest` from the `copies` map if it exists as a key.
                    copies.remove(dest);

                    // Also, crucial: If any *other* variable was a copy *of* `dest`,
                    // that copy relationship is now broken because `dest` has a new value.
                    // We need to remove all map entries where `dest` is the *value*.
                    copies.retain(|_, source| source != dest);
                }
                Instr::FuncParam { name, .. } => {
                    copies.remove(name);
                    // If any other variable was a copy *of* 'name', invalidate that.
                    copies.retain(|_, source| source != name);
                }
                Instr::Call {
                    result: Some(dest), ..
                } => {
                    copies.remove(dest);
                    copies.retain(|_, source| source != dest);
                }
                _ => {}
            }

            optimized_instrs.push(new_instr);
        }

        optimized_instrs
    }

    fn name(&self) -> &'static str {
        "CopyPropagation"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::{Instr, Op};

    #[test]
    fn test_simple_copy() {
        let pass = CopyPropagation;
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("x".to_string(), 10),
            Instr::Assign("y".to_string(), "x".to_string()), // y = x
            Instr::Print("y".to_string()),                   // Should become Print(x)
        ];
        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("x".to_string(), 10),
            Instr::Assign("y".to_string(), "x".to_string()), // Assign stays for now
            Instr::Print("x".to_string()),                   // Use propagated
        ];
        assert_eq!(pass.optimize(instrs), expected);
    }

    #[test]
    fn test_copy_chain() {
        let pass = CopyPropagation;
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("x".to_string(), 10),
            Instr::Assign("y".to_string(), "x".to_string()), // y = x
            Instr::Assign("z".to_string(), "y".to_string()), // z = y
            Instr::Print("z".to_string()),                   // Should become Print(x)
        ];
        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("x".to_string(), 10),
            Instr::Assign("y".to_string(), "x".to_string()),
            Instr::Assign("z".to_string(), "x".to_string()), // Propagated source for Assign
            Instr::Print("x".to_string()),                   // Use fully propagated
        ];
        assert_eq!(pass.optimize(instrs), expected);
    }

    #[test]
    fn test_copy_in_binop() {
        let pass = CopyPropagation;
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("x".to_string(), 10),
            Instr::Const("one".to_string(), 1),
            Instr::Assign("y".to_string(), "x".to_string()), // y = x
            Instr::Assign("two".to_string(), "one".to_string()), // two = one
            Instr::BinOp("z".to_string(), "y".to_string(), Op::Add, "two".to_string()), // z = y + two => z = x + one
            Instr::Print("z".to_string()),
        ];
        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("x".to_string(), 10),
            Instr::Const("one".to_string(), 1),
            Instr::Assign("y".to_string(), "x".to_string()),
            Instr::Assign("two".to_string(), "one".to_string()),
            Instr::BinOp("z".to_string(), "x".to_string(), Op::Add, "one".to_string()), // Operands propagated
            Instr::Print("z".to_string()),
        ];
        assert_eq!(pass.optimize(instrs), expected);
    }

    #[test]
    fn test_copy_killed_by_redefinition() {
        let pass = CopyPropagation;
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("x".to_string(), 10),
            Instr::Assign("y".to_string(), "x".to_string()), // y = x
            Instr::Const("y".to_string(), 20),               // y redefined, copy killed
            Instr::Print("y".to_string()), // Should print the new y (20), not x (10)
        ];
        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("x".to_string(), 10),
            Instr::Assign("y".to_string(), "x".to_string()), // This assign happens
            Instr::Const("y".to_string(), 20),               // y is redefined
            Instr::Print("y".to_string()),                   // Print uses the *redefined* y
        ];
        assert_eq!(pass.optimize(instrs), expected);
    }

    #[test]
    fn test_copy_of_copy_killed() {
        let pass = CopyPropagation;
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("a".to_string(), 1),
            Instr::Assign("b".to_string(), "a".to_string()), // b = a
            Instr::Assign("c".to_string(), "b".to_string()), // c = b (= a)
            Instr::Const("b".to_string(), 2),                // b redefined, kills c=b link
            Instr::Print("c".to_string()),                   // Should still print a (1)
            Instr::Print("b".to_string()),                   // Should print 2
        ];
        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("a".to_string(), 1),
            Instr::Assign("b".to_string(), "a".to_string()),
            Instr::Assign("c".to_string(), "a".to_string()), // Source propagated
            Instr::Const("b".to_string(), 2),                // b redefined
            Instr::Print("a".to_string()),                   // c propagated to a
            Instr::Print("b".to_string()),                   // new b
        ];
        assert_eq!(pass.optimize(instrs), expected);
    }
}
