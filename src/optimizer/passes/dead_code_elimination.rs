use std::collections::HashSet;

use crate::optimizer::Instr;

use super::pass::Pass;

/// Dead code elimination optimization pass
///
/// This pass removes instructions that don't contribute to program output.
pub struct DeadCodeElimination;

impl Pass for DeadCodeElimination {
    fn optimize(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        let mut used: HashSet<String> = HashSet::new();
        let mut required_labels: HashSet<String> = HashSet::new();
        let mut reversed = instrs.clone();
        reversed.reverse();

        let mut optimized = Vec::new();

        // Pre-pass to record labels that can't be deleted (otherwise code will try to jump to a
        // target that doesn't exist).
        for instr in &instrs {
            match instr {
                Instr::BranchIf(_, then_lbl, else_lbl) => {
                    required_labels.insert(then_lbl.clone());
                    required_labels.insert(else_lbl.clone());
                }
                Instr::Jump(label) => {
                    required_labels.insert(label.clone());
                }
                _ => {}
            }
        }

        for instr in &reversed {
            match instr {
                Instr::Print(var) => {
                    used.insert(var.clone());
                    optimized.push(instr.clone());
                }
                Instr::BinOp(dest, left, _, right) => {
                    if used.contains(dest) {
                        used.insert(left.clone());
                        used.insert(right.clone());
                        optimized.push(instr.clone());
                    }
                }
                Instr::Cmp(dest, left, _, right) => {
                    if used.contains(dest) {
                        used.insert(left.clone());
                        used.insert(right.clone());
                        optimized.push(instr.clone());
                    }
                }
                Instr::BranchIf(cond, then_label, else_label) => {
                    used.insert(cond.clone());
                    required_labels.insert(then_label.clone());
                    required_labels.insert(else_label.clone());
                    optimized.push(instr.clone());
                }
                Instr::Jump(label) => {
                    required_labels.insert(label.clone());
                    optimized.push(instr.clone());
                }
                Instr::Label(label) => {
                    if required_labels.contains(label) {
                        optimized.push(instr.clone());
                    }
                }
                Instr::Phi(dest, left, right) => {
                    if used.contains(dest) {
                        used.insert(left.clone());
                        used.insert(right.clone());
                        optimized.push(instr.clone());
                    }
                }
                Instr::Const(dest, _) => {
                    if used.contains(dest) {
                        optimized.push(instr.clone());
                    }
                }
                Instr::Assign(dest, src) => {
                    if used.contains(dest) {
                        used.insert(src.clone());
                        optimized.push(instr.clone());
                    }
                }
            }
        }

        optimized.reverse();
        optimized
    }

    fn name(&self) -> &'static str {
        "DeadCodeElimination"
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
