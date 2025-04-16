pub mod pass;
pub mod constant_folding;
pub mod dead_code_elimination;

use crate::optimizer::{Instr, Op, CmpOp};

pub use constant_folding::ConstantFolding;
pub use dead_code_elimination::DeadCodeElimination;
pub use pass::Pass;

/// Available optimization passes
#[derive(Debug, Clone, Copy)]
pub enum PassType {
    ConstantFolding,
    DeadCodeElimination,
}

/// The main optimizer that runs optimization passes
pub struct Optimizer;

impl Optimizer {
    /// Run all available optimization passes in order
    pub fn run_all(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        let cf = ConstantFolding;
        let dce = DeadCodeElimination;
        
        // First run constant folding, then dead code elimination
        let folded = cf.optimize(instrs);
        dce.optimize(folded)
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
                PassType::DeadCodeElimination => {
                    let pass = DeadCodeElimination;
                    result = pass.optimize(result);
                }
            }
        }
        
        result
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::{Instr, Op, CmpOp};

    #[test]
    fn test_optimizer() {
        let instrs = vec![
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
            Instr::Const("t2".to_string(), 5),
            Instr::Print("t2".to_string()),
        ];

        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_optimizer_relational_operators() {
        let instrs = vec![
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
            Instr::Const("t2".to_string(), 1),
            Instr::Print("t2".to_string()),
        ];

        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_optimizer_full_chain_folding() {
        let instrs = vec![
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
            Instr::Const("t6".to_string(), 1), // final folded boolean result
            Instr::Print("t6".to_string()),
        ];

        assert_eq!(optimized, expected);
    }
}