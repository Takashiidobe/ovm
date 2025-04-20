use crate::optimizer::{CFG, Instr};
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
    fn optimize(&self, mut cfg: CFG) -> CFG {
        // Process each block independently
        for block in cfg.blocks.values_mut() {
            let mut optimized_instrs = Vec::with_capacity(block.instrs.len());
            let mut i = 0;

            while i < block.instrs.len() {
                if i + 1 < block.instrs.len() {
                    match (&block.instrs[i], &block.instrs[i + 1]) {
                        // Pattern 1: Const(temp, val) followed by Assign(dest, temp)
                        (Instr::Const(temp_reg, val), Instr::Assign(dest_reg, src_reg))
                            if temp_reg == src_reg =>
                        {
                            optimized_instrs.push(Instr::Const(dest_reg.clone(), *val));
                            i += 2;
                        }
                        // Pattern 2: Assign(temp, src1) followed by Assign(dest, temp)
                        (Instr::Assign(temp_reg, src1_reg), Instr::Assign(dest_reg, src2_reg))
                            if temp_reg == src2_reg =>
                        {
                            optimized_instrs.push(Instr::Assign(dest_reg.clone(), src1_reg.clone()));
                            i += 2;
                        }
                        // Pattern 3: Phi(temp, preds) followed by Assign(dest, temp)
                        (Instr::Phi(temp_reg, pred_vals), Instr::Assign(dest_reg, src_reg))
                            if temp_reg == src_reg =>
                        {
                            // Coalesce by making the Phi write directly to the final destination
                            optimized_instrs.push(Instr::Phi(dest_reg.clone(), pred_vals.clone()));
                            i += 2;
                        }
                        _ => {
                            optimized_instrs.push(block.instrs[i].clone());
                            i += 1;
                        }
                    }
                } else {
                    optimized_instrs.push(block.instrs[i].clone());
                    i += 1;
                }
            }
            block.instrs = optimized_instrs;
        }
        cfg
    }

    fn name(&self) -> &'static str {
        "MoveCoalescing"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::passes::test_helpers::*;

    #[test]
    fn test_coalesce_const_assign() {
        let pass = MoveCoalescing;
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("t1", 10),
                assign("x", "t1"),
                print("x"),
            ], vec![], vec![]),
        ]);

        let expected = create_test_cfg(vec![
            ("entry", vec![
                cnst("x", 10),
                print("x"),
            ], vec![], vec![]),
        ]);

        assert_eq!(pass.optimize(cfg), expected);
    }

    #[test]
    fn test_coalesce_phi_assign() {
        let pass = MoveCoalescing;
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("c", 1),
                branch("c", "then", "else"),
            ], vec![], vec!["then", "else"]),
            ("then", vec![
                cnst("a", 10),
                jump("merge"),
            ], vec!["entry"], vec!["merge"]),
            ("else", vec![
                cnst("b", 20),
                jump("merge"),
            ], vec!["entry"], vec!["merge"]),
            ("merge", vec![
                phi("temp", vec![("then", "a"), ("else", "b")]),
                assign("x", "temp"),  // This assign should be coalesced
                print("x"),
            ], vec!["then", "else"], vec![]),
        ]);

        let expected = create_test_cfg(vec![
            ("entry", vec![
                cnst("c", 1),
                branch("c", "then", "else"),
            ], vec![], vec!["then", "else"]),
            ("then", vec![
                cnst("a", 10),
                jump("merge"),
            ], vec!["entry"], vec!["merge"]),
            ("else", vec![
                cnst("b", 20),
                jump("merge"),
            ], vec!["entry"], vec!["merge"]),
            ("merge", vec![
                phi("x", vec![("then", "a"), ("else", "b")]),  // Phi writes directly to x
                print("x"),
            ], vec!["then", "else"], vec![]),
        ]);

        assert_eq!(pass.optimize(cfg), expected);
    }

    // ... add more tests for cross-block patterns ...
}
