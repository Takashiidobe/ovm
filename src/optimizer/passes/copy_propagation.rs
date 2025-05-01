use std::collections::HashMap;

use crate::optimizer::passes::pass::Pass;
use crate::optimizer::{CFG, Instr};

/// Copy Propagation Optimization Pass
///
/// Replaces uses of variables that are copies of other variables
/// with the original variable.
/// Example: If `y = x`, subsequent uses of `y` become uses of `x`.
pub struct CopyPropagation;

impl CopyPropagation {
    /// Recursively finds the original source variable in a chain of copies.
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
    fn optimize(&self, cfg: CFG) -> CFG {
        let mut new_cfg = cfg.clone();

        // Process each basic block independently
        for block in new_cfg.blocks.values_mut() {
            let mut optimized_instrs = Vec::with_capacity(block.instrs.len());
            let mut copies: HashMap<String, String> = HashMap::new();

            for instr in &block.instrs {
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

            block.instrs = optimized_instrs;
        }

        new_cfg
    }

    fn name(&self) -> &'static str {
        "CopyPropagation"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::passes::test_helpers::*;

    #[test]
    fn test_simple_copy() {
        let pass = CopyPropagation;
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("x", 10),
                assign("y", "x"), // y = x
                print("y"),       // Should become Print(x)
            ],
            vec![],
            vec![],
        )]);

        let expected = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("x", 10),
                assign("y", "x"),
                print("x"), // Use propagated
            ],
            vec![],
            vec![],
        )]);

        assert_eq!(pass.optimize(cfg), expected);
    }

    #[test]
    fn test_copy_chain() {
        let pass = CopyPropagation;
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("x", 10),
                assign("y", "x"), // y = x
                assign("z", "y"), // z = y
                print("z"),       // Should become Print(x)
            ],
            vec![],
            vec![],
        )]);

        let expected = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("x", 10),
                assign("y", "x"),
                assign("z", "x"), // Propagated source
                print("x"),       // Use fully propagated
            ],
            vec![],
            vec![],
        )]);

        assert_eq!(pass.optimize(cfg), expected);
    }

    #[test]
    fn test_cross_block_limitation() {
        let pass = CopyPropagation;
        let cfg = create_test_cfg(vec![
            (
                "entry",
                vec![
                    cnst("x", 10),
                    assign("y", "x"), // y = x
                    jump("next"),
                ],
                vec![],
                vec!["next"],
            ),
            (
                "next",
                vec![
                    print("y"), // Should NOT become Print(x) - different block
                ],
                vec!["entry"],
                vec![],
            ),
        ]);

        let expected = create_test_cfg(vec![
            (
                "entry",
                vec![cnst("x", 10), assign("y", "x"), jump("next")],
                vec![],
                vec!["next"],
            ),
            (
                "next",
                vec![
                    print("y"), // Stays as y - copies don't propagate across blocks
                ],
                vec!["entry"],
                vec![],
            ),
        ]);

        assert_eq!(pass.optimize(cfg), expected);
    }

    #[test]
    fn test_copy_killed_by_redefinition() {
        let pass = CopyPropagation;
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("x", 10),
                assign("y", "x"), // y = x
                cnst("y", 20),    // y redefined
                print("y"),       // Should print y (not x)
            ],
            vec![],
            vec![],
        )]);

        let expected = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("x", 10),
                assign("y", "x"),
                cnst("y", 20),
                print("y"), // Uses redefined y
            ],
            vec![],
            vec![],
        )]);

        assert_eq!(pass.optimize(cfg), expected);
    }
}
