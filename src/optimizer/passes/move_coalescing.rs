use crate::optimizer::Instr;
use crate::optimizer::passes::pass::Pass; // Import Pass trait

/// Move Coalescing Optimization Pass
///
/// This pass looks for patterns like:
///   Const(t1, 10)
///   Assign(x, t1)
/// and replaces them with:
///   Const(x, 10)
///
/// It also looks for patterns like:
///  Assign(t1, a)
///  Assign(x, t1)
/// and replaces them with:
///  Assign(x, a)
pub struct MoveCoalescing;

impl Pass for MoveCoalescing {
    fn optimize(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        let mut optimized_instrs = Vec::with_capacity(instrs.len());
        let mut i = 0;

        while i < instrs.len() {
            if i + 1 < instrs.len() {
                match (&instrs[i], &instrs[i + 1]) {
                    // Pattern 1: Const(temp, val) followed by Assign(dest, temp)
                    (Instr::Const(temp_reg, val), Instr::Assign(dest_reg, src_reg))
                        if temp_reg == src_reg =>
                    {
                        // Coalesce: Replace with Const(dest, val)
                        optimized_instrs.push(Instr::Const(dest_reg.clone(), *val));
                        i += 2; // Skip both original instructions
                    }
                    // Pattern 2: Assign(temp, src1) followed by Assign(dest, temp)
                    (Instr::Assign(temp_reg, src1_reg), Instr::Assign(dest_reg, src2_reg))
                        if temp_reg == src2_reg =>
                    {
                        // Coalesce: Replace with Assign(dest, src1)
                        optimized_instrs.push(Instr::Assign(dest_reg.clone(), src1_reg.clone()));
                        i += 2; // Skip both original instructions
                    }
                    _ => {
                        // No pattern match, copy the current instruction
                        optimized_instrs.push(instrs[i].clone());
                        i += 1;
                    }
                }
            } else {
                // Last instruction, just copy it
                optimized_instrs.push(instrs[i].clone());
                i += 1;
            }
        }

        optimized_instrs
    }

    fn name(&self) -> &'static str {
        "MoveCoalescing"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::Instr;

    #[test]
    fn test_coalesce_const_assign() {
        let instrs = vec![
            Instr::Label("start".to_string()),
            Instr::Const("t1".to_string(), 10),
            Instr::Assign("x".to_string(), "t1".to_string()),
            Instr::Print("x".to_string()),
        ];
        let pass = MoveCoalescing;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Label("start".to_string()),
            Instr::Const("x".to_string(), 10),
            Instr::Print("x".to_string()),
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_coalesce_assign_assign() {
        let instrs = vec![
            Instr::Label("start".to_string()),
            Instr::Assign("t1".to_string(), "a".to_string()), // Assume 'a' is defined elsewhere
            Instr::Assign("x".to_string(), "t1".to_string()),
            Instr::Print("x".to_string()),
        ];
        let pass = MoveCoalescing;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Label("start".to_string()),
            Instr::Assign("x".to_string(), "a".to_string()),
            Instr::Print("x".to_string()),
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_no_coalesce_different_temp() {
        let instrs = vec![
            Instr::Const("t1".to_string(), 10),
            Instr::Assign("x".to_string(), "t2".to_string()), // Different temp used
            Instr::Print("x".to_string()),
        ];
        let pass = MoveCoalescing;
        let original_instrs = instrs.clone();
        let optimized = pass.optimize(instrs);
        assert_eq!(
            optimized, original_instrs,
            "Should not coalesce when temps differ"
        );
    }

    #[test]
    fn test_no_coalesce_intervening_instr() {
        let instrs = vec![
            Instr::Const("t1".to_string(), 10),
            Instr::Label("intervene".to_string()), // Intervening instruction
            Instr::Assign("x".to_string(), "t1".to_string()),
            Instr::Print("x".to_string()),
        ];
        let pass = MoveCoalescing;
        let original_instrs = instrs.clone();
        let optimized = pass.optimize(instrs);
        assert_eq!(
            optimized, original_instrs,
            "Should not coalesce across other instructions"
        );
    }

    #[test]
    fn test_coalesce_at_end() {
        let instrs = vec![
            Instr::Print("something_else".to_string()),
            Instr::Const("t1".to_string(), 10),
            Instr::Assign("x".to_string(), "t1".to_string()),
        ];
        let pass = MoveCoalescing;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Print("something_else".to_string()),
            Instr::Const("x".to_string(), 10),
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_coalesce_assign_assign_at_end() {
        let instrs = vec![
            Instr::Print("something_else".to_string()),
            Instr::Assign("t1".to_string(), "a".to_string()),
            Instr::Assign("x".to_string(), "t1".to_string()),
        ];
        let pass = MoveCoalescing;
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Print("something_else".to_string()),
            Instr::Assign("x".to_string(), "a".to_string()),
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_empty_input() {
        let instrs: Vec<Instr> = vec![];
        let pass = MoveCoalescing;
        let optimized = pass.optimize(instrs);
        let expected: Vec<Instr> = vec![];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_single_instruction() {
        let instrs = vec![Instr::Const("t1".to_string(), 10)];
        let pass = MoveCoalescing;
        let optimized = pass.optimize(instrs.clone());
        assert_eq!(optimized, instrs);
    }
}
