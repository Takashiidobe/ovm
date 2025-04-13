pub mod registers;

use crate::frontend::{
    expr::Expr,
    stmt::Stmt,
    token::{Object, TokenType},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Instr {
    Const(String, i64),
    BinOp(String, String, Op, String),
    Print(String),
}

#[non_exhaustive]
#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
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

    pub fn program_to_ir(&mut self, stmts: &[Stmt]) -> Vec<Instr> {
        let mut instrs = Vec::new();
        for stmt in stmts {
            self.stmt_to_ir(stmt, &mut instrs);
        }
        instrs
    }

    fn stmt_to_ir(&mut self, stmt: &Stmt, instrs: &mut Vec<Instr>) -> String {
        match stmt {
            Stmt::Expression { expr } => self.expr_to_ir(expr, instrs),
            Stmt::Print { expr } => {
                let to_print = self.expr_to_ir(expr, instrs);
                instrs.push(Instr::Print(to_print.clone()));
                to_print
            }
            _ => todo!(),
        }
    }

    fn expr_to_ir(&mut self, expr: &Expr, instrs: &mut Vec<Instr>) -> String {
        match expr {
            Expr::Literal { value } => match value {
                // TODO: support turning the other types into literals
                Object::Integer(n) => {
                    let temp = self.new_temp();
                    instrs.push(Instr::Const(temp.clone(), *n));
                    temp
                }
                _ => todo!(),
            },
            Expr::Binary {
                left,
                right,
                operator,
            } => {
                let l = self.expr_to_ir(left, instrs);
                let r = self.expr_to_ir(right, instrs);
                let temp = self.new_temp();
                let op = match operator.r#type {
                    TokenType::Plus => Op::Add,
                    TokenType::Minus => Op::Sub,
                    TokenType::Star => Op::Mul,
                    TokenType::Slash => Op::Div,
                    TokenType::EqualEqual => Op::Eq,
                    TokenType::BangEqual => Op::Neq,
                    TokenType::LessEqual => Op::Lte,
                    TokenType::GreaterEqual => Op::Gte,
                    TokenType::Less => Op::Lt,
                    TokenType::Greater => Op::Gt,
                    _ => todo!(),
                };
                instrs.push(Instr::BinOp(temp.clone(), l, op, r));
                temp
            }
            Expr::Grouping { expr } => self.expr_to_ir(expr, instrs),
            e => panic!("{e:?}"),
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
                                Op::Eq => (lv == rv) as i64,
                                Op::Neq => (lv != rv) as i64,
                                Op::Lt => (lv < rv) as i64,
                                Op::Lte => (lv <= rv) as i64,
                                Op::Gt => (lv > rv) as i64,
                                Op::Gte => (lv >= rv) as i64,
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

    #[test]
    fn test_optimizer_relational_operators() {
        let instrs = vec![
            Instr::Const("t0".to_string(), 10),
            Instr::Const("t1".to_string(), 20),
            Instr::BinOp("t2".to_string(), "t0".to_string(), Op::Lt, "t1".to_string()), // 10 < 20 => true (1)
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
            Instr::BinOp("t6".to_string(), "t4".to_string(), Op::Eq, "t5".to_string()),
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
