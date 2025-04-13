use crate::frontend::parser::Expr;

#[derive(Debug, PartialEq, Clone)]
pub enum Instr {
    Const(String, i64),
    BinOp(String, String, Op, String),
    Print(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Default, Debug, PartialEq)]
pub struct SSA {
    temp_counter: usize,
}

impl SSA {
    pub fn new_temp(&mut self) -> String {
        let name = format!("t{}", self.temp_counter);
        self.temp_counter += 1;
        name
    }

    pub fn program_to_ir(&mut self, exprs: &[Expr]) -> Vec<Instr> {
        let mut instrs = Vec::new();
        for expr in exprs {
            // We process each expression fully for side effects like `Print`.
            self.expr_to_ir(expr, &mut instrs);
        }
        instrs
    }

    fn expr_to_ir(&mut self, expr: &Expr, instrs: &mut Vec<Instr>) -> String {
        match expr {
            Expr::Num(n) => {
                let temp = self.new_temp();
                instrs.push(Instr::Const(temp.clone(), *n));
                temp
            }
            Expr::Add(lhs, rhs) => {
                let l = self.expr_to_ir(lhs, instrs);
                let r = self.expr_to_ir(rhs, instrs);
                let temp = self.new_temp();
                instrs.push(Instr::BinOp(temp.clone(), l, Op::Add, r));
                temp
            }
            Expr::Sub(lhs, rhs) => {
                let l = self.expr_to_ir(lhs, instrs);
                let r = self.expr_to_ir(rhs, instrs);
                let temp = self.new_temp();
                instrs.push(Instr::BinOp(temp.clone(), l, Op::Sub, r));
                temp
            }
            Expr::Mul(lhs, rhs) => {
                let l = self.expr_to_ir(lhs, instrs);
                let r = self.expr_to_ir(rhs, instrs);
                let temp = self.new_temp();
                instrs.push(Instr::BinOp(temp.clone(), l, Op::Mul, r));
                temp
            }
            Expr::Div(lhs, rhs) => {
                let l = self.expr_to_ir(lhs, instrs);
                let r = self.expr_to_ir(rhs, instrs);
                let temp = self.new_temp();
                instrs.push(Instr::BinOp(temp.clone(), l, Op::Div, r));
                temp
            }
            Expr::Print(inner) => {
                let value_temp = self.expr_to_ir(inner, instrs);
                instrs.push(Instr::Print(value_temp.clone()));
                value_temp // Returning for completeness
            }
        }
    }
}

pub struct Optimizer;

pub enum Pass {
    ConstantFolding,
    DeadCodeElimination,
}

impl Optimizer {
    pub fn run_all(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        let folded = self.constant_folding(instrs);

        self.dead_code_elimination(folded)
    }

    pub fn run(&self, instrs: Vec<Instr>, passes: Vec<Pass>) -> Vec<Instr> {
        let mut res = instrs.clone();
        for pass in passes {
            match pass {
                Pass::ConstantFolding => res = self.constant_folding(res),
                Pass::DeadCodeElimination => res = self.dead_code_elimination(res),
            }
        }

        res
    }

    pub fn constant_folding(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        use std::collections::HashMap;

        let mut constants: HashMap<String, i64> = HashMap::new();
        let mut new_instrs = Vec::new();

        for instr in instrs {
            match instr {
                Instr::Const(name, val) => {
                    constants.insert(name.clone(), val);
                    new_instrs.push(Instr::Const(name, val));
                }
                Instr::BinOp(dest, left, op, right) => {
                    let lval = constants.get(&left);
                    let rval = constants.get(&right);
                    match (lval, rval) {
                        (Some(&lv), Some(&rv)) => {
                            let result = match op {
                                Op::Add => lv + rv,
                                Op::Sub => lv - rv,
                                Op::Mul => lv * rv,
                                Op::Div => lv / rv,
                            };
                            constants.insert(dest.clone(), result);
                            new_instrs.push(Instr::Const(dest, result));
                        }
                        _ => {
                            constants.remove(&dest); // might be overwritten
                            new_instrs.push(Instr::BinOp(dest, left, op, right));
                        }
                    }
                }
                Instr::Print(var) => {
                    new_instrs.push(Instr::Print(var));
                }
            }
        }

        new_instrs
    }

    pub fn dead_code_elimination(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        use std::collections::HashSet;

        let mut used: HashSet<String> = HashSet::new();
        let mut reversed: Vec<Instr> = instrs.clone();
        reversed.reverse();
        let mut optimized = Vec::new();

        for instr in reversed {
            match &instr {
                Instr::Print(var) => {
                    used.insert(var.clone());
                    optimized.push(instr);
                }
                Instr::BinOp(dest, left, _, right) => {
                    if used.contains(dest) {
                        used.insert(left.clone());
                        used.insert(right.clone());
                        optimized.push(instr);
                    }
                }
                Instr::Const(dest, _) => {
                    if used.contains(dest) {
                        optimized.push(instr);
                    }
                }
            }
        }

        optimized.reverse();
        optimized
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ssa_translation_arithmetic_and_print() {
        let exprs = vec![Expr::Print(Box::new(Expr::Add(
            Box::new(Expr::Num(2)),
            Box::new(Expr::Mul(Box::new(Expr::Num(3)), Box::new(Expr::Num(4)))),
        )))];

        let mut ssa = SSA::default();
        let instrs = ssa.program_to_ir(&exprs);

        let expected = vec![
            Instr::Const("t0".to_string(), 2),
            Instr::Const("t1".to_string(), 3),
            Instr::Const("t2".to_string(), 4),
            Instr::BinOp(
                "t3".to_string(),
                "t1".to_string(),
                Op::Mul,
                "t2".to_string(),
            ),
            Instr::BinOp(
                "t4".to_string(),
                "t0".to_string(),
                Op::Add,
                "t3".to_string(),
            ),
            Instr::Print("t4".to_string()),
        ];

        assert_eq!(instrs, expected);
    }

    #[test]
    fn test_constant_folding() {
        let instrs = vec![
            Instr::Const("t0".to_string(), 2),
            Instr::Const("t1".to_string(), 3),
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Add,
                "t1".to_string(),
            ),
            Instr::Print("t2".to_string()),
        ];

        let optimizer = Optimizer;
        let optimized = optimizer.run(instrs, vec![Pass::ConstantFolding]);

        let expected = vec![
            Instr::Const("t0".to_string(), 2),
            Instr::Const("t1".to_string(), 3),
            Instr::Const("t2".to_string(), 5),
            Instr::Print("t2".to_string()),
        ];

        assert_eq!(optimized, expected);
    }

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
}
