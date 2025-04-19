use std::collections::HashMap;

use crate::optimizer::Instr;

use super::pass::Pass;

/// Conditional Branch Elimination Pass
///
/// This pass eliminates conditional branches (`BranchIf`) where the condition
/// is known to be a constant value at compile time. It replaces such branches
/// with unconditional jumps (`Jump`) to the appropriate target label.
pub struct BranchElimination;

impl Pass for BranchElimination {
    fn optimize(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        let mut optimized_instrs = Vec::with_capacity(instrs.len());
        let mut constants: HashMap<String, i64> = HashMap::new();

        for instr in instrs {
            match instr {
                Instr::Const(ref dest, val) => {
                    constants.insert(dest.to_string(), val);
                    optimized_instrs.push(instr.clone());
                }
                Instr::BranchIf(ref cond_var, ref then_lbl, ref else_lbl) => {
                    if let Some(&val) = constants.get(cond_var) {
                        match val != 0 {
                            true => optimized_instrs.push(Instr::Jump(then_lbl.clone())),
                            false => optimized_instrs.push(Instr::Jump(else_lbl.clone())),
                        }
                    } else {
                        optimized_instrs.push(instr.clone());
                    }
                }
                Instr::FuncParam { .. } => {
                    // Doesn't affect branching or define a constant
                    optimized_instrs.push(instr.clone());
                }
                _ => {
                    optimized_instrs.push(instr.clone());
                }
            }
        }

        optimized_instrs
    }

    fn name(&self) -> &'static str {
        "BranchElimination"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::Instr;

    #[test]
    fn test_branch_elimination_true() {
        let instrs = vec![
            Instr::Const("cond".to_string(), 1), // True condition
            Instr::BranchIf(
                "cond".to_string(),
                "then_label".to_string(),
                "else_label".to_string(),
            ),
            Instr::Label("then_label".to_string()),
            Instr::Print("then".to_string()), // Should be kept
            Instr::Label("else_label".to_string()),
            Instr::Print("else".to_string()), // Should be removed by subsequent DCE
        ];

        let pass = BranchElimination;
        let optimized = pass.optimize(instrs);

        let expected = vec![
            Instr::Const("cond".to_string(), 1),
            Instr::Jump("then_label".to_string()), // Replaced BranchIf
            Instr::Label("then_label".to_string()),
            Instr::Print("then".to_string()),
            Instr::Label("else_label".to_string()),
            Instr::Print("else".to_string()),
        ];

        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_branch_elimination_false() {
        let instrs = vec![
            Instr::Const("cond".to_string(), 0), // False condition
            Instr::BranchIf(
                "cond".to_string(),
                "then_label".to_string(),
                "else_label".to_string(),
            ),
            Instr::Label("then_label".to_string()),
            Instr::Print("then".to_string()), // Should be removed by subsequent DCE
            Instr::Label("else_label".to_string()),
            Instr::Print("else".to_string()), // Should be kept
        ];

        let pass = BranchElimination;
        let optimized = pass.optimize(instrs.clone());

        let expected = vec![
            Instr::Const("cond".to_string(), 0),
            Instr::Jump("else_label".to_string()), // Replaced BranchIf
            Instr::Label("then_label".to_string()),
            Instr::Print("then".to_string()),
            Instr::Label("else_label".to_string()),
            Instr::Print("else".to_string()),
        ];

        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_branch_no_elimination() {
        // Case where the condition is not a known constant
        let instrs = vec![
            // Assume "var_cond" comes from input or complex calculation
            Instr::Assign("var_cond".to_string(), "input".to_string()),
            Instr::BranchIf(
                "var_cond".to_string(),
                "then_label".to_string(),
                "else_label".to_string(),
            ),
            Instr::Label("then_label".to_string()),
            Instr::Print("then".to_string()),
            Instr::Label("else_label".to_string()),
            Instr::Print("else".to_string()),
        ];

        let pass = BranchElimination;
        let optimized = pass.optimize(instrs.clone());

        // Expect no change
        assert_eq!(optimized, instrs);
    }
}
