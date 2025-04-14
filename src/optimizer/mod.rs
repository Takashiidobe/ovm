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
    Cmp(String, String, CmpOp, String),
    BranchIf(String, String, String),
    Jump(String),
    Label(String),
    Phi(String, String, String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CmpOp {
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

#[non_exhaustive]
#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    BitAnd,
    BitOr,
    And,
    Or,
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

    pub fn new_label(&mut self, base: &str) -> String {
        let name = format!("{}_{}", base, self.temp_counter);
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
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_t = self.expr_to_ir(condition, instrs);
                let then_label = self.new_label("then");
                let else_label = self.new_label("else");
                let merge_label = self.new_label("merge");

                instrs.push(Instr::BranchIf(
                    cond_t.clone(),
                    then_label.clone(),
                    else_label.clone(),
                ));

                // condition
                instrs.push(Instr::Label(then_label.clone()));
                let then_result = self.stmt_to_ir(then_branch, instrs);
                instrs.push(Instr::Jump(merge_label.clone()));

                // else, if it exists
                let else_result = if let Some(else_stmt) = *else_branch.clone() {
                    instrs.push(Instr::Label(else_label.clone()));
                    let res = self.stmt_to_ir(&else_stmt, instrs);
                    instrs.push(Instr::Jump(merge_label.clone()));
                    Some(res)
                } else {
                    // No else: skip to merge
                    instrs.push(Instr::Label(else_label.clone()));
                    instrs.push(Instr::Jump(merge_label.clone()));
                    None
                };

                // merge label
                instrs.push(Instr::Label(merge_label.clone()));

                // If both branches yield a value, merge them with a phi
                match else_result {
                    Some(e) => {
                        let t = self.new_temp();
                        instrs.push(Instr::Phi(t.clone(), then_result, e));
                        t
                    }
                    None => then_result, // Result is only from then-branch
                }
            }
            Stmt::Block { statements } => {
                let mut ret = String::default();
                for stmt in statements {
                    ret = self.stmt_to_ir(stmt, instrs)
                }
                ret
            }
            stmt => panic!("{stmt:?}"),
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
                match operator.r#type {
                    TokenType::Plus => instrs.push(Instr::BinOp(temp.clone(), l, Op::Add, r)),
                    TokenType::Minus => instrs.push(Instr::BinOp(temp.clone(), l, Op::Sub, r)),
                    TokenType::Star => instrs.push(Instr::BinOp(temp.clone(), l, Op::Mul, r)),
                    TokenType::Slash => instrs.push(Instr::BinOp(temp.clone(), l, Op::Div, r)),
                    TokenType::EqualEqual => instrs.push(Instr::Cmp(temp.clone(), l, CmpOp::Eq, r)),
                    TokenType::BangEqual => instrs.push(Instr::Cmp(temp.clone(), l, CmpOp::Eq, r)),
                    TokenType::LessEqual => instrs.push(Instr::Cmp(temp.clone(), l, CmpOp::Lte, r)),
                    TokenType::GreaterEqual => {
                        instrs.push(Instr::Cmp(temp.clone(), l, CmpOp::Gte, r))
                    }
                    TokenType::Less => instrs.push(Instr::Cmp(temp.clone(), l, CmpOp::Lt, r)),
                    TokenType::Greater => instrs.push(Instr::Cmp(temp.clone(), l, CmpOp::Gt, r)),
                    _ => unreachable!(),
                };

                temp
            }
            Expr::Grouping { expr } => self.expr_to_ir(expr, instrs),
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let l = self.expr_to_ir(left, instrs);
                let r = self.expr_to_ir(right, instrs);
                let temp = self.new_temp();
                match operator.r#type {
                    TokenType::EqualEqual => instrs.push(Instr::Cmp(temp.clone(), l, CmpOp::Eq, r)),
                    TokenType::BangEqual => instrs.push(Instr::Cmp(temp.clone(), l, CmpOp::Eq, r)),
                    TokenType::LessEqual => instrs.push(Instr::Cmp(temp.clone(), l, CmpOp::Lte, r)),
                    TokenType::GreaterEqual => {
                        instrs.push(Instr::Cmp(temp.clone(), l, CmpOp::Gte, r))
                    }
                    TokenType::Less => instrs.push(Instr::Cmp(temp.clone(), l, CmpOp::Lt, r)),
                    TokenType::Greater => instrs.push(Instr::Cmp(temp.clone(), l, CmpOp::Gt, r)),
                    _ => unreachable!(),
                };
                temp
            }
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
                                Op::BitAnd => lv & rv,
                                Op::BitOr => lv | rv,
                                Op::And => (lv > 0 && rv > 0) as i64,
                                Op::Or => (lv > 0 || rv > 0) as i64,
                            };
                            constants.insert(dest.clone(), result);
                            new_instrs.push(Instr::Const(dest, result));
                        }
                        _ => {
                            constants.remove(&dest);
                            new_instrs.push(Instr::BinOp(dest, left, op, right));
                        }
                    }
                }
                Instr::Cmp(dest, left, cmp_op, right) => {
                    let lval = constants.get(&left);
                    let rval = constants.get(&right);
                    match (lval, rval) {
                        (Some(&lv), Some(&rv)) => {
                            let result = match cmp_op {
                                CmpOp::Eq => lv == rv,
                                CmpOp::Neq => lv != rv,
                                CmpOp::Lt => lv < rv,
                                CmpOp::Lte => lv <= rv,
                                CmpOp::Gt => lv > rv,
                                CmpOp::Gte => lv >= rv,
                            };
                            constants.insert(dest.clone(), result as i64);
                            new_instrs.push(Instr::Const(dest, result as i64));
                        }
                        _ => {
                            constants.remove(&dest);
                            new_instrs.push(Instr::Cmp(dest, left, cmp_op, right));
                        }
                    }
                }
                Instr::Phi(dest, left, right) => {
                    let lval = constants.get(&left);
                    let rval = constants.get(&right);
                    match (lval, rval) {
                        (Some(&lv), Some(&rv)) if lv == rv => {
                            constants.insert(dest.clone(), lv);
                            new_instrs.push(Instr::Const(dest, lv));
                        }
                        _ => {
                            constants.remove(&dest);
                            new_instrs.push(Instr::Phi(dest, left, right));
                        }
                    }
                }
                Instr::Print(name) => {
                    new_instrs.push(Instr::Print(name));
                }
                other => new_instrs.push(other),
            }
        }

        new_instrs
    }

    pub fn dead_code_elimination(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        use std::collections::HashSet;

        let mut used: HashSet<String> = HashSet::new();
        let mut required_labels: HashSet<String> = HashSet::new();
        let mut reversed = instrs.clone();
        reversed.reverse();

        let mut optimized = Vec::new();

        // Pre-pass to record labels that can't be deleted (otherwise code will try to jump to a
        // target that doesn't exist.
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
