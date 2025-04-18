pub mod algebraic_simplification;
pub mod branch_elimination;
pub mod constant_folding;
pub mod constant_propagation;
pub mod copy_propagation;
pub mod dead_code_elimination;
pub mod global_value_numbering;
pub mod identity_elimination;
pub mod move_coalescing;
pub mod pass;
pub mod strength_reduction;

use crate::optimizer::Instr;

pub use algebraic_simplification::AlgebraicSimplification;
pub use branch_elimination::BranchElimination;
pub use constant_folding::ConstantFolding;
pub use constant_propagation::ConstantPropagation;
pub use copy_propagation::CopyPropagation;
pub use dead_code_elimination::DeadCodeElimination;
pub use global_value_numbering::GlobalValueNumbering;
pub use identity_elimination::IdentityElimination;
pub use move_coalescing::MoveCoalescing;
pub use pass::Pass;
pub use strength_reduction::StrengthReduction;

/// Available optimization passes
#[derive(Debug, Clone, Copy)]
pub enum PassType {
    ConstantFolding,
    CopyPropagation,
    DeadCodeElimination,
    GlobalValueNumbering,
    MoveCoalescing,
    BranchElimination,
    StrengthReduction,
    IdentityElimination,
    AlgebraicSimplification,
}

/// The main optimizer that runs optimization passes
pub struct Optimizer;

impl Optimizer {
    /// Run all available optimization passes in order, iterating core passes until fixed point
    pub fn run_all(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        let cf = ConstantFolding;
        let be = BranchElimination;
        let dce = DeadCodeElimination;
        let mc = MoveCoalescing;
        let sr = StrengthReduction;
        let ie = IdentityElimination;
        let algs = AlgebraicSimplification;
        let gvn = GlobalValueNumbering;
        let cp = CopyPropagation;

        let mut current_instrs = instrs;

        loop {
            let before_instrs = current_instrs.clone(); // Clone to compare later

            // Run the core iterative passes in a logical order
            current_instrs = cf.optimize(current_instrs);
            current_instrs = mc.optimize(current_instrs);
            current_instrs = ie.optimize(current_instrs);
            current_instrs = algs.optimize(current_instrs);
            current_instrs = sr.optimize(current_instrs);
            current_instrs = be.optimize(current_instrs);
            current_instrs = dce.optimize(current_instrs);

            // Check if the instructions changed
            if current_instrs == before_instrs {
                break; // Fixed point reached
            }
        }

        // Run GVN, Copy Propagation, and a final DCE pass after the loop stabilizes
        current_instrs = gvn.optimize(current_instrs);
        current_instrs = cp.optimize(current_instrs);
        current_instrs = dce.optimize(current_instrs);

        current_instrs
    }

    /// Run specific optimization passes in the given order
    pub fn run(&self, instrs: Vec<Instr>, passes: Vec<PassType>) -> Vec<Instr> {
        let mut result = instrs;

        for pass_type in passes {
            match pass_type {
                PassType::ConstantFolding => {
                    let pass = ConstantFolding;
                    result = pass.optimize(result);
                }
                PassType::CopyPropagation => {
                    let pass = CopyPropagation;
                    result = pass.optimize(result);
                }
                PassType::DeadCodeElimination => {
                    let pass = DeadCodeElimination;
                    result = pass.optimize(result);
                }
                PassType::GlobalValueNumbering => {
                    let pass = GlobalValueNumbering;
                    result = pass.optimize(result);
                }
                PassType::MoveCoalescing => {
                    let pass = MoveCoalescing;
                    result = pass.optimize(result);
                }
                PassType::BranchElimination => {
                    let pass = BranchElimination;
                    result = pass.optimize(result);
                }
                PassType::StrengthReduction => {
                    let pass = StrengthReduction;
                    result = pass.optimize(result);
                }
                PassType::IdentityElimination => {
                    let pass = IdentityElimination;
                    result = pass.optimize(result);
                }
                PassType::AlgebraicSimplification => {
                    let pass = AlgebraicSimplification;
                    result = pass.optimize(result);
                }
            }
        }

        result
    }

    pub fn run_none(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        instrs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::{CmpOp, Instr, Op};

    #[test]
    fn test_optimizer() {
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 2),
            Instr::Const("t1".to_string(), 3),
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Add,
                "t1".to_string(),
            ),
            Instr::Const("t_unused".to_string(), 999),
            Instr::Print("t2".to_string()),
        ];

        let optimizer = Optimizer;
        let optimized = optimizer.run_all(instrs);

        // after the constant folding pass, t0 and t1 are considered dead, since they aren't used
        // again.
        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t2".to_string(), 5),
            Instr::Print("t2".to_string()),
        ];

        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_optimizer_relational_operators() {
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 10),
            Instr::Const("t1".to_string(), 20),
            Instr::Cmp(
                "t2".to_string(),
                "t0".to_string(),
                CmpOp::Lt,
                "t1".to_string(),
            ), // 10 < 20 => true (1)
            Instr::Const("t_unused".to_string(), 999),
            Instr::Print("t2".to_string()),
        ];

        let optimizer = Optimizer;
        let optimized = optimizer.run_all(instrs);

        // After constant folding: t2 = 1
        // Dead code elimination removes t0, t1, and t_unused
        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t2".to_string(), 1),
            Instr::Print("t2".to_string()),
        ];

        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_optimizer_full_chain_folding() {
        let instrs = vec![
            Instr::Label("entry".to_string()),
            // Arithmetic chain: 4 + 5 = 9
            Instr::Const("t0".to_string(), 4),
            Instr::Const("t1".to_string(), 5),
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Add,
                "t1".to_string(),
            ),
            // 9 * 2 = 18
            Instr::Const("t3".to_string(), 2),
            Instr::BinOp(
                "t4".to_string(),
                "t2".to_string(),
                Op::Mul,
                "t3".to_string(),
            ),
            // 18 == 18 => true (1)
            Instr::Const("t5".to_string(), 18),
            Instr::Cmp(
                "t6".to_string(),
                "t4".to_string(),
                CmpOp::Eq,
                "t5".to_string(),
            ),
            // Dead code
            Instr::Const("t_unused".to_string(), 999),
            // Final print
            Instr::Print("t6".to_string()),
        ];

        let optimizer = Optimizer;
        let optimized = optimizer.run_all(instrs);

        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t6".to_string(), 1), // final folded boolean result
            Instr::Print("t6".to_string()),
        ];

        assert_eq!(optimized, expected);
    }
}
