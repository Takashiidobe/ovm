pub mod registers;

use std::collections::{HashMap, HashSet};

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
    Assign(String, String),
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
    variable_versions: HashMap<String, usize>,
    scopes: Vec<HashMap<String, String>>,
}

impl SSA {
    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn define_variable(&mut self, name: &str, ssa_name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), ssa_name);
        } else {
            let mut scope = HashMap::new();
            scope.insert(name.to_string(), ssa_name);
            self.scopes.push(scope);
        }
    }

    fn resolve_variable(&self, name: &str) -> Option<&String> {
        for scope in self.scopes.iter().rev() {
            if let Some(ssa_name) = scope.get(name) {
                return Some(ssa_name);
            }
        }
        None
    }

    fn with_scope<F, R>(&mut self, mut f: F) -> R
    where
        F: FnMut(&mut Self) -> R,
    {
        self.enter_scope();
        let result = f(self);
        self.exit_scope();
        result
    }

    fn new_temp(&mut self) -> String {
        let name = format!("t{}", self.temp_counter);
        self.temp_counter += 1;
        name
    }

    fn new_label(&mut self, base: &str) -> String {
        let name = format!("{}_{}", base, self.temp_counter);
        self.temp_counter += 1;
        name
    }

    fn next_versioned_name(&mut self, var: &str) -> String {
        let version = self.variable_versions.entry(var.to_string()).or_insert(0);
        *version += 1;
        format!("{}_{}", var, *version)
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

                let (then_result, then_scope) = {
                    instrs.push(Instr::Label(then_label.clone()));
                    let mut result = String::new();
                    let scope = self.with_scope(|ssa| {
                        result = ssa.stmt_to_ir(then_branch, instrs);
                        ssa.scopes.last().cloned().unwrap_or_default()
                    });
                    instrs.push(Instr::Jump(merge_label.clone()));
                    (result, scope)
                };

                let (else_result, else_scope) = {
                    instrs.push(Instr::Label(else_label.clone()));
                    let mut result = String::new();
                    let scope = self.with_scope(|ssa| {
                        if let Some(else_stmt) = *else_branch.clone() {
                            result = ssa.stmt_to_ir(&else_stmt, instrs);
                        } else {
                            result = ssa.new_temp();
                            instrs.push(Instr::Const(result.clone(), 0));
                        }
                        ssa.scopes.last().cloned().unwrap_or_default()
                    });
                    instrs.push(Instr::Jump(merge_label.clone()));
                    (result, scope)
                };

                instrs.push(Instr::Label(merge_label.clone()));
                self.enter_scope(); // merge scope must be explicitly re-entered

                // Value result of the whole if-expression
                let if_result = self.new_temp();
                instrs.push(Instr::Phi(
                    if_result.clone(),
                    then_result.clone(),
                    else_result.clone(),
                ));

                let outer_scope = self
                    .scopes
                    .get(self.scopes.len().saturating_sub(2))
                    .cloned()
                    .unwrap_or_default();

                let all_vars: HashSet<_> = then_scope
                    .keys()
                    .chain(else_scope.keys())
                    .chain(outer_scope.keys())
                    .collect();

                let mut processed_vars = HashSet::new();

                for var in all_vars {
                    if processed_vars.contains(var) {
                        continue;
                    }
                    processed_vars.insert(var.clone());

                    let then_val = then_scope.get(var);
                    let else_val = else_scope.get(var);

                    match (then_val, else_val) {
                        // Case 1: Variable modified in both branches with different values
                        (Some(t1), Some(t2)) if t1 != t2 => {
                            let phi = self.new_temp();
                            instrs.push(Instr::Phi(phi.clone(), t1.clone(), t2.clone()));
                            self.define_variable(var, phi);
                        }
                        // Case 2: Variable modified in only one branch
                        (Some(t), None) => {
                            // We need a phi node that uses the outer scope value for the
                            // branch where it wasn't modified
                            let outer_val = outer_scope.get(var).cloned().unwrap_or_else(|| {
                                // If not in outer scope, create a dummy value
                                let dummy = self.new_temp();
                                instrs.push(Instr::Const(dummy.clone(), 0));
                                dummy
                            });

                            let phi = self.new_temp();
                            instrs.push(Instr::Phi(phi.clone(), t.clone(), outer_val));
                            self.define_variable(var, phi);
                        }
                        (None, Some(t)) => {
                            // Similar to above but for else branch
                            let outer_val = outer_scope.get(var).cloned().unwrap_or_else(|| {
                                let dummy = self.new_temp();
                                instrs.push(Instr::Const(dummy.clone(), 0));
                                dummy
                            });

                            let phi = self.new_temp();
                            instrs.push(Instr::Phi(phi.clone(), outer_val, t.clone()));
                            self.define_variable(var, phi);
                        }
                        // Case 3: Variable not modified in either branch, use outer scope
                        (None, None) => {
                            if let Some(t) = outer_scope.get(var) {
                                // Just bring the outer scope value into current scope
                                self.define_variable(var, t.clone());
                            }
                            // Do nothing if not in any scope
                        }
                        // Case 4: Modified in both branches with same value
                        (Some(t1), Some(t2)) if t1 == t2 => {
                            // No phi needed, just use the same value
                            self.define_variable(var, t1.clone());
                        }
                        _ => unreachable!(),
                    }
                }
                if_result
            }
            Stmt::Block { statements } => {
                let mut ret = String::default();
                for stmt in statements {
                    ret = self.stmt_to_ir(stmt, instrs)
                }
                ret
            }
            Stmt::Var { name, initializer } => {
                let expr = initializer
                    .clone()
                    .unwrap_or(Expr::Literal { value: Object::Nil });
                let rhs_temp = self.expr_to_ir(&expr, instrs);
                let ssa_name = self.next_versioned_name(&name.lexeme);
                self.define_variable(&name.lexeme, ssa_name.clone());
                instrs.push(Instr::Assign(ssa_name.clone(), rhs_temp));
                ssa_name
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
            Expr::Assign { name, value } => {
                let rhs_temp = self.expr_to_ir(value, instrs);
                let ssa_name = self.next_versioned_name(&name.lexeme);
                instrs.push(Instr::Assign(ssa_name.clone(), rhs_temp));
                self.define_variable(&name.lexeme, ssa_name.clone());

                ssa_name
            }
            Expr::Variable { name } => match self.resolve_variable(&name.lexeme) {
                Some(ssa_name) => ssa_name.clone(),
                None => panic!("Unresolved variable {}", name.lexeme),
            },
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
                        // If both values are constant and identical, we can replace with a constant
                        (Some(&lv), Some(&rv)) if lv == rv => {
                            constants.insert(dest.clone(), lv);
                            new_instrs.push(Instr::Const(dest, lv));
                        }
                        // If both are constants but different, we need to keep the phi since it depends on control flow
                        (Some(_), Some(_)) => {
                            constants.remove(&dest);
                            new_instrs.push(Instr::Phi(dest, left, right));
                        }
                        // If either branch is not a constant, we can't fold
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
        // target that doesn't exist).
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
                Instr::Assign(dest, src) => {
                    if used.contains(dest) {
                        used.insert(src.clone());
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
