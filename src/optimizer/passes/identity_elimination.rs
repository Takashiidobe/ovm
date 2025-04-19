use crate::optimizer::Instr;
use crate::optimizer::passes::pass::Pass;

/// Identity Function Elimination Pass
///
/// Removes instructions of the form `Assign(x, x)`.
pub struct IdentityElimination;

impl Pass for IdentityElimination {
    fn optimize(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        instrs
            .into_iter()
            .filter(|instr| {
                // Keep instruction if it's NOT an Assign(x, x) identity
                !matches!(instr, Instr::Assign(dest, src) if dest == src)
            })
            .collect()
    }

    fn name(&self) -> &'static str {
        "IdentityElimination"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::Instr;
    use crate::optimizer::Op; // Import Op if needed for other test instructions

    #[test]
    fn test_removes_identity_assign() {
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 5),
            Instr::Assign("t1".to_string(), "t0".to_string()), // Keep: t1 = t0
            Instr::Assign("t1".to_string(), "t1".to_string()), // Remove: t1 = t1
            Instr::Print("t1".to_string()),
        ];
        let pass = IdentityElimination;
        let optimized = pass.optimize(instrs);

        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 5),
            Instr::Assign("t1".to_string(), "t0".to_string()),
            // Assign("t1", "t1") should be removed
            Instr::Print("t1".to_string()),
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_no_identity_assign() {
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 10),
            Instr::Assign("t1".to_string(), "t0".to_string()),
            Instr::BinOp("t2".to_string(), "t1".to_string(), Op::Add, "t0".to_string()),
            Instr::Print("t2".to_string()),
        ];
        let pass = IdentityElimination;
        let original_instrs = instrs.clone(); // Clone before moving
        let optimized = pass.optimize(instrs);

        assert_eq!(optimized, original_instrs, "Should not change if no identity assigns exist");
    }

     #[test]
    fn test_multiple_identity_assigns() {
        let instrs = vec![
            Instr::Assign("a".to_string(), "a".to_string()), // Remove
            Instr::Const("b".to_string(), 1),
            Instr::Assign("c".to_string(), "b".to_string()),
            Instr::Assign("c".to_string(), "c".to_string()), // Remove
            Instr::Assign("b".to_string(), "b".to_string()), // Remove
            Instr::Print("c".to_string()),
        ];
        let pass = IdentityElimination;
        let optimized = pass.optimize(instrs);

        let expected = vec![
            // Assign("a", "a") removed
            Instr::Const("b".to_string(), 1),
            Instr::Assign("c".to_string(), "b".to_string()),
            // Assign("c", "c") removed
            // Assign("b", "b") removed
            Instr::Print("c".to_string()),
        ];
        assert_eq!(optimized, expected);
    }

     #[test]
    fn test_empty_input() {
        let instrs: Vec<Instr> = vec![];
        let pass = IdentityElimination;
        let optimized = pass.optimize(instrs);
        let expected: Vec<Instr> = vec![];
        assert_eq!(optimized, expected);
    }
} 