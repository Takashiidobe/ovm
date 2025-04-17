pub mod passes;
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

#[derive(Debug, PartialEq)]
pub struct SSA {
    temp_counter: usize,
    label_counter: usize,
    scopes: Vec<HashMap<String, String>>,
    instructions: Vec<Instr>,
    var_versions: HashMap<String, u64>,
}

impl Default for SSA {
    fn default() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            temp_counter: Default::default(),
            label_counter: Default::default(),
            instructions: Default::default(),
            var_versions: Default::default(),
        }
    }
}

impl SSA {
    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) -> HashMap<String, String> {
        self.scopes.pop().expect("No scope exists")
    }

    fn new_var_version(&mut self, name: &str) -> String {
        let version = self.var_versions.entry(name.to_string()).or_default();
        *version += 1;
        format!("{}_{}", name, version)
    }

    fn define_variable(&mut self, name: &str) -> String {
        let var_name = self.new_var_version(name);

        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), var_name.clone());

        var_name
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

    fn new_label(&mut self, base: &str) -> String {
        let name = format!("{}_{}", base, self.label_counter);
        self.label_counter += 1;
        name
    }

    fn new_temp(&mut self) -> String {
        let name = format!("t{}", self.temp_counter);
        self.temp_counter += 1;
        name
    }

    pub fn program_to_ir(&mut self, stmts: &[Stmt]) -> Vec<Instr> {
        for stmt in stmts {
            self.stmt_to_ir(stmt);
        }
        self.instructions.clone()
    }

    fn stmt_to_ir(&mut self, stmt: &Stmt) -> String {
        match stmt {
            Stmt::Expression { expr } => self.expr_to_ir(expr),
            Stmt::Print { expr } => {
                let to_print = self.expr_to_ir(expr);
                self.emit(Instr::Print(to_print.clone()));
                to_print
            }
            Stmt::While { condition, body } => {
                let loop_start = self.new_label("loop_start");
                let loop_body = self.new_label("loop_body");
                let loop_end = self.new_label("loop_end");

                // Step 1: Capture outer scope before loop
                let outer_scope = self.scopes.last().cloned().unwrap_or_default();

                // Step 2: Emit loop header
                self.emit(Instr::Label(loop_start.clone()));

                // Step 3: Create Phi nodes for loop-carried variables
                let mut phi_vars = HashMap::new(); // var_name -> phi_name
                for (var, outer_val) in &outer_scope {
                    let phi_temp = self.new_temp();
                    self.emit(Instr::Phi(
                        phi_temp.clone(),
                        outer_val.clone(),
                        outer_val.clone(),
                    ));
                    self.define_variable(var);
                    phi_vars.insert(var.clone(), phi_temp);
                }

                // Step 4: Emit condition check
                let cond_temp = self.expr_to_ir(condition);
                self.emit(Instr::BranchIf(
                    cond_temp.clone(),
                    loop_body.clone(),
                    loop_end.clone(),
                ));

                // Step 5: Loop body
                self.emit(Instr::Label(loop_body.clone()));

                let updated_vars = self.with_scope(|ssa| {
                    ssa.stmt_to_ir(body);
                    ssa.scopes.last().cloned().unwrap_or_default()
                });

                // Step 6: Update Phi backedges (only if values changed)
                for (var, phi_name) in &phi_vars {
                    if let Some(updated_val) = updated_vars.get(var) {
                        self.emit(Instr::Phi(
                            phi_name.clone(),
                            outer_scope.get(var).unwrap().clone(),
                            updated_val.clone(),
                        ));
                        self.define_variable(var); // Rebind phi result for next iterations
                    }
                }

                // Step 7: Loop back
                self.emit(Instr::Jump(loop_start.clone()));

                // Step 8: Exit loop
                self.emit(Instr::Label(loop_end.clone()));

                // Step 9: Dummy result (loops don't produce a meaningful value)
                let result = self.new_temp();
                self.emit(Instr::Const(result.clone(), 0));

                result
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // first, evaluate the expression.
                let cond_t = self.expr_to_ir(condition);

                // then emit the if branch, the else branch, and the merge label.
                // The if and else go to the merge label after execution.
                let then_label = self.new_label("cond_then");
                let else_label = self.new_label("cond_else");
                let merge_label = self.new_label("cond_merge");

                // Then emit the instruction
                self.emit(Instr::BranchIf(
                    cond_t.clone(),
                    then_label.clone(),
                    else_label.clone(),
                ));

                // We then create a new scope for the if branch
                let (then_result, then_scope) = {
                    // we emit the then label after the if
                    self.emit(Instr::Label(then_label.clone()));
                    let mut result = String::new();
                    let scope = self.with_scope(|ssa| {
                        result = ssa.stmt_to_ir(then_branch);
                        ssa.scopes.last().cloned().unwrap_or_default()
                    });
                    self.emit(Instr::Jump(merge_label.clone()));
                    (result, scope)
                };

                let (else_result, else_scope) = {
                    self.enter_scope();
                    self.emit(Instr::Label(else_label.clone()));

                    let (result, scope) = if let Some(else_stmt) = *else_branch.clone() {
                        (
                            self.stmt_to_ir(&else_stmt),
                            self.scopes.last().cloned().unwrap_or_default(),
                        )
                    } else {
                        let result = self.new_temp();
                        self.emit(Instr::Const(result.clone(), 0));
                        (result, self.scopes.last().cloned().unwrap_or_default())
                    };
                    self.emit(Instr::Jump(merge_label.clone()));

                    (result, scope)
                };

                self.emit(Instr::Label(merge_label.clone()));

                self.enter_scope(); // merge scope must be explicitly re-entered

                // Value result of the whole if-expression
                let if_result = self.new_temp();
                self.emit(Instr::Phi(
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
                            self.emit(Instr::Phi(phi.clone(), t1.clone(), t2.clone()));
                            self.define_variable(var);
                        }
                        // Case 2: Variable modified in only one branch
                        (Some(t), None) => {
                            // We need a phi node that uses the outer scope value for the
                            // branch where it wasn't modified
                            let outer_val = outer_scope.get(var).cloned().unwrap_or_else(|| {
                                // If not in outer scope, create a dummy value
                                let dummy = self.new_temp();
                                self.emit(Instr::Const(dummy.clone(), 0));
                                dummy
                            });

                            let phi = self.new_temp();
                            self.emit(Instr::Phi(phi.clone(), t.clone(), outer_val));
                            self.define_variable(var);
                        }
                        (None, Some(t)) => {
                            // Similar to above but for else branch
                            let outer_val = outer_scope.get(var).cloned().unwrap_or_else(|| {
                                let dummy = self.new_temp();
                                self.emit(Instr::Const(dummy.clone(), 0));
                                dummy
                            });

                            let phi = self.new_temp();
                            self.emit(Instr::Phi(phi.clone(), outer_val, t.clone()));
                            self.define_variable(var);
                        }
                        // Case 3: Variable not modified in either branch, use outer scope
                        (None, None) => {
                            if outer_scope.contains_key(var) {
                                // Just bring the outer scope value into current scope
                                self.define_variable(var);
                            }
                            // Do nothing if not in any scope
                        }
                        // Case 4: Modified in both branches with same value
                        (Some(t1), Some(t2)) if t1 == t2 => {
                            // No phi needed, just use the same value
                            self.define_variable(var);
                        }
                        _ => unreachable!(),
                    }
                }
                if_result
            }
            Stmt::Block { statements } => {
                self.with_scope(|ssa| {
                    for stmt in statements {
                        ssa.stmt_to_ir(stmt);
                    }
                });
                String::default()
            }
            Stmt::Var { name, initializer } => {
                let expr = initializer
                    .clone()
                    .unwrap_or(Expr::Literal { value: Object::Nil });
                let rhs_temp = self.expr_to_ir(&expr);
                let var_name = match name.literal.clone().unwrap() {
                    Object::String(name) => name,
                    _ => panic!(
                        "left side of variable is not an identifier, {:?}",
                        name.literal
                    ),
                };
                let ssa_name = self.define_variable(&var_name);
                self.emit(Instr::Assign(ssa_name.clone(), rhs_temp));
                ssa_name
            }
            stmt => panic!("Statement not supported: {stmt:?}"),
        }
    }

    fn expr_to_ir(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Literal { value } => match value {
                // TODO: support turning the other types into literals
                Object::Integer(n) => {
                    let temp = self.new_temp();
                    self.emit(Instr::Const(temp.clone(), *n));
                    temp
                }
                _ => todo!(),
            },
            Expr::Binary {
                left,
                right,
                operator,
            } => {
                let l = self.expr_to_ir(left);
                let r = self.expr_to_ir(right);
                let temp = self.new_temp();
                match operator.r#type {
                    TokenType::Plus => self.emit(Instr::BinOp(temp.clone(), l, Op::Add, r)),
                    TokenType::Minus => self.emit(Instr::BinOp(temp.clone(), l, Op::Sub, r)),
                    TokenType::Star => self.emit(Instr::BinOp(temp.clone(), l, Op::Mul, r)),
                    TokenType::Slash => self.emit(Instr::BinOp(temp.clone(), l, Op::Div, r)),
                    TokenType::EqualEqual => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Eq, r)),
                    TokenType::BangEqual => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Eq, r)),
                    TokenType::LessEqual => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Lte, r)),
                    TokenType::GreaterEqual => {
                        self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Gte, r))
                    }
                    TokenType::Less => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Lt, r)),
                    TokenType::Greater => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Gt, r)),
                    _ => unreachable!(),
                };

                temp
            }
            Expr::Grouping { expr } => self.expr_to_ir(expr),
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let l = self.expr_to_ir(left);
                let r = self.expr_to_ir(right);
                let temp = self.new_temp();
                match operator.r#type {
                    TokenType::EqualEqual => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Eq, r)),
                    TokenType::BangEqual => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Eq, r)),
                    TokenType::LessEqual => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Lte, r)),
                    TokenType::GreaterEqual => {
                        self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Gte, r))
                    }
                    TokenType::Less => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Lt, r)),
                    TokenType::Greater => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Gt, r)),
                    _ => unreachable!(),
                };
                temp
            }
            Expr::Assign { name, value } => {
                let rhs_temp = self.expr_to_ir(value);
                let var_name = match name.literal.clone().unwrap() {
                    Object::String(name) => name,
                    _ => panic!(
                        "left side of variable is not an identifier, {:?}",
                        name.literal
                    ),
                };
                let ssa_name = self.define_variable(&var_name);

                self.emit(Instr::Assign(ssa_name.clone(), rhs_temp));

                ssa_name
            }
            Expr::Variable { name } => match self.resolve_variable(&name.lexeme) {
                Some(ssa_name) => ssa_name.clone(),
                None => panic!("Variable called before definition {}", name.lexeme),
            },
            e => panic!("Unsupported expr: {e:?}"),
        }
    }

    fn emit(&mut self, instr: Instr) {
        self.instructions.push(instr);
    }
}
