pub mod algebraic_simplification;
pub mod constant_folding;
pub mod constant_propagation;
pub mod copy_propagation;
pub mod dead_code_elimination;
pub mod global_value_numbering;
pub mod identity_elimination;
pub mod move_coalescing;
pub mod pass;
pub mod strength_reduction;
mod test_helpers;

use crate::optimizer::CFG;

pub use algebraic_simplification::AlgebraicSimplification;
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
    StrengthReduction,
    IdentityElimination,
    AlgebraicSimplification,
}

/// The main optimizer that runs optimization passes
pub struct Optimizer;

impl Optimizer {
    /// Run all available optimization passes in order, iterating core passes until fixed point
    pub fn run_all(&self, instrs: CFG) -> CFG {
        let cf = ConstantFolding;
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
    pub fn run(&self, instrs: CFG, passes: Vec<PassType>) -> CFG {
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

    pub fn run_none(&self, instrs: CFG) -> CFG {
        instrs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::{CmpOp, Op};
    use test_helpers::*;

    #[test]
    fn test_optimizer() {
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("t0", 2),
                cnst("t1", 3),
                binop("t2", "t0", Op::Add, "t1"),
                cnst("t_unused", 999),
                print("t2"),
            ], vec![], vec![]),
        ]);

        let optimizer = Optimizer;
        let optimized = optimizer.run_all(cfg);

        let expected = create_test_cfg(vec![
            ("entry", vec![
                cnst("t2", 5),
                print("t2"),
            ], vec![], vec![]),
        ]);

        assert_eq!(optimized.blocks, expected.blocks);
    }

    #[test]
    fn test_optimizer_relational_operators() {
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("t0", 10),
                cnst("t1", 20),
                cmp("t2", "t0", CmpOp::Lt, "t1"),
                cnst("t_unused", 999),
                print("t2"),
            ], vec![], vec![]),
        ]);

        let optimizer = Optimizer;
        let optimized = optimizer.run_all(cfg);

        let expected = create_test_cfg(vec![
            ("entry", vec![
                cnst("t2", 1),
                print("t2"),
            ], vec![], vec![]),
        ]);

        assert_eq!(optimized.blocks, expected.blocks);
    }

    #[test]
    fn test_optimizer_full_chain_folding() {
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("t0", 4),
                cnst("t1", 5),
                binop("t2", "t0", Op::Add, "t1"),
                cnst("t3", 2),
                binop("t4", "t2", Op::Mul, "t3"),
                cnst("t5", 18),
                cmp("t6", "t4", CmpOp::Eq, "t5"),
                cnst("t_unused", 999),
                print("t6"),
            ], vec![], vec![]),
        ]);

        let optimizer = Optimizer;
        let optimized = optimizer.run_all(cfg);

        let expected = create_test_cfg(vec![
            ("entry", vec![
                cnst("t6", 1),
                print("t6"),
            ], vec![], vec![]),
        ]);

        assert_eq!(optimized.blocks, expected.blocks);
    }
}
