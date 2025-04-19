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
    Phi(String, Vec<(String, String)>),
    Call { target: String, args: Vec<String>, result: Option<String> },
    Ret { value: Option<String> },
    Assign(String, String),
}

#[derive(Debug, PartialEq, Clone, Eq, Hash, Copy)]
pub enum CmpOp {
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

#[non_exhaustive]
#[derive(Debug, PartialEq, Clone, Eq, Hash, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    BitAnd,
    BitOr,
    And,
    Or,
    Shl,
    Mod,
    Shr,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct BasicBlock {
    pub label: String,
    pub instrs: Vec<Instr>,
    pub preds: Vec<String>,
    pub succs: Vec<String>,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct CFG {
    pub blocks: HashMap<String, BasicBlock>,
    pub current_block: Option<String>,
}

impl CFG {
    pub fn start_block(&mut self, label: &str) {
        self.current_block = Some(label.to_string());
        self.blocks.insert(
            label.to_string(),
            BasicBlock {
                label: label.to_string(),
                instrs: vec![],
                preds: vec![],
                succs: vec![],
            },
        );
    }

    pub fn emit(&mut self, instr: Instr) {
        if let Some(current_label) = &self.current_block {
            self.blocks
                .get_mut(current_label)
                .unwrap()
                .instrs
                .push(instr);
        } else {
            panic!("No current block set");
        }
    }

    pub fn add_edge(&mut self, from: &str, to: &str) {
        self.blocks
            .get_mut(from)
            .unwrap()
            .succs
            .push(to.to_string());
        self.blocks
            .get_mut(to)
            .unwrap()
            .preds
            .push(from.to_string());
    }
}

#[derive(Debug, PartialEq)]
pub struct SSA {
    temp_counter: usize,
    label_counter: usize,
    scopes: Vec<HashMap<String, String>>,
    instructions: Vec<Instr>,
    var_versions: HashMap<String, u64>,
    cfg: CFG,
}

impl Default for SSA {
    fn default() -> Self {
        let mut ssa = Self {
            scopes: vec![HashMap::new()],
            temp_counter: Default::default(),
            label_counter: Default::default(),
            instructions: Default::default(),
            var_versions: Default::default(),
            cfg: Default::default(),
        };
        let entry_label = ssa.new_label("main");
        ssa.cfg.start_block(&entry_label);
        ssa
    }
}

impl SSA {
    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) -> HashMap<String, String> {
        self.scopes.pop().expect("No scope exists")
    }

    fn assign_variable(&mut self, name: &str, ssa_temp: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), ssa_temp.to_string());
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
        self.instructions.push(Instr::Label("main".to_string()));
        for stmt in stmts {
            eprintln!("{:?}", stmt);
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
                // 0. Setup Labels and Predecessor Info
                let header_label = self.new_label("loop_header");
                let body_label = self.new_label("loop_body");
                let exit_label = self.new_label("loop_exit");

                // Get the label of the block immediately preceding the loop header.
                let pre_header_label = self
                    .cfg
                    .current_block
                    .clone()
                    .expect("Current block must be set before a while loop");

                // 2. Start Header Block in CFG & Add Edge from Predecessor
                self.cfg.start_block(&header_label);
                self.cfg.add_edge(&pre_header_label, &header_label);
                self.cfg.current_block = Some(header_label.clone()); // Update current block

                // Emit the label instruction
                self.emit(Instr::Label(header_label.clone()));

                let outer_scope_vars = self
                    .scopes
                    .get(self.scopes.len().saturating_sub(1))
                    .cloned()
                    .unwrap_or_default();

                // 3. Setup Loop Scope and Identify Loop Variables
                self.enter_scope(); // Scope for the loop (header + body)

                // 4. Emit Incomplete Phi Nodes & Update Scope
                let mut phi_data = HashMap::new(); // var_name -> (phi_ssa_name, outer_ssa_name, instr_idx)
                for (var_name, outer_ssa_name) in &outer_scope_vars {
                    // We assume any variable from outer scope *might* be modified.
                    let phi_ssa_name = self.new_temp();
                    let phi_instr = Instr::Phi(
                        phi_ssa_name.clone(),
                        vec![
                            // Edge from pre-header
                            (pre_header_label.clone(), outer_ssa_name.clone()), // Back-edge from body will be added later by patching
                        ],
                    );
                    let instr_idx = self.instructions.len();
                    self.emit(phi_instr);
                    // Update current scope to use the result of the phi node
                    self.assign_variable(var_name, &phi_ssa_name);
                    phi_data.insert(
                        var_name.clone(),
                        (phi_ssa_name, outer_ssa_name.clone(), instr_idx),
                    );
                }

                // 5. Evaluate Loop Condition (uses phi results via current scope)
                let cond_temp = self.expr_to_ir(condition);

                // 6. Conditional Branch to Body or Exit
                self.emit(Instr::BranchIf(
                    cond_temp,
                    body_label.clone(),
                    exit_label.clone(),
                ));

                // --- Ensure target blocks exist before adding edges ---
                self.cfg.start_block(&body_label); // Create body block
                self.cfg.start_block(&exit_label); // Create exit block
                // --- Now add edges from header ---
                self.cfg.add_edge(&header_label, &body_label);
                self.cfg.add_edge(&header_label, &exit_label);

                // 7. Set current block to Body Block & Emit Label
                self.cfg.current_block = Some(body_label.clone());
                self.emit(Instr::Label(body_label.clone()));

                // 8. Execute Loop Body statements directly in the current loop scope.
                //    Avoid creating an extra nested scope if the body is a block.
                match body.as_ref() {
                    Stmt::Block { statements } => {
                        for stmt in statements {
                            self.stmt_to_ir(stmt);
                        }
                    }
                    _ => {
                        // If the body is not a block, execute it directly.
                        self.stmt_to_ir(body);
                    }
                }

                // 9. Resolve variable values at the end of the body for Phi patching
                let mut resolved_back_edge_values = HashMap::new();
                for var_name in outer_scope_vars.keys() {
                    // Use resolve_variable to find the current SSA name at the end of the body
                    if let Some(resolved_ssa_name) = self.resolve_variable(var_name) {
                         resolved_back_edge_values.insert(var_name.clone(), resolved_ssa_name.clone());
                    } else {
                        // This case might indicate an issue, or perhaps the variable
                        // was introduced *only* within the loop body, which phi nodes
                        // based on outer_scope_vars wouldn't handle anyway.
                        // For now, let's report it, but maybe a different handling is needed.
                         eprintln!("Warning: Variable '{}' from outer scope not found at loop body end for Phi patching.", var_name);
                         // If the variable was originally from outer scope, we might need
                         // to use its initial phi value as fallback, but resolve_variable should find it
                         // if it was ever assigned. Let's use the phi name itself as a placeholder for now.
                         if let Some((phi_ssa_name, _, _)) = phi_data.get(var_name) {
                            resolved_back_edge_values.insert(var_name.clone(), phi_ssa_name.clone());
                         }
                    }
                }

                // 10. Prepare Phi Patches (use resolved values)
                let mut patches = vec![];
                for (var_name, (phi_ssa_name, _, instr_idx)) in &phi_data {
                    // Use the resolved value from the end of the body.
                    // Fallback to the phi_ssa_name itself if resolution failed (as per warning above).
                    let back_edge_val = resolved_back_edge_values
                        .get(var_name)
                        .cloned()
                        .unwrap_or_else(|| {
                             eprintln!("Warning: Using phi SSA name '{}' as fallback for back-edge value of '{}'.", phi_ssa_name, var_name);
                             phi_ssa_name.clone() // Fallback, though resolve should ideally work.
                        });
                    patches.push((*instr_idx, body_label.clone(), back_edge_val));
                }

                // 11. Jump from Body back to Header
                self.emit(Instr::Jump(header_label.clone()));
                self.cfg.add_edge(&body_label, &header_label);

                // 12. Apply Phi Patches (Modify the Phi instructions emitted in step 4)
                // This requires mutable access to self.instructions
                for (idx, back_edge_label, back_edge_val) in patches {
                    if let Some(Instr::Phi(_, edges)) = self.instructions.get_mut(idx) {
                        // Directly use the mutably borrowed `edges`
                        edges.push((back_edge_label, back_edge_val));
                    } else {
                        eprintln!(
                            "SSA Error: Expected Phi instruction at index {} for patching.",
                            idx
                        );
                    }
                }

                // 13. Set current block to Exit Block & Emit Label
                self.exit_scope(); // Exit the main loop scope
                self.cfg.current_block = Some(exit_label.clone()); // Update current block for exit path
                self.emit(Instr::Label(exit_label.clone()));
                self.enter_scope(); // Enter scope for code after the loop

                // 14. Populate Post-Loop Scope
                // Variables available after the loop take their value from the phi nodes
                // generated in the header, as that's the value available on the exit edge.
                for (var_name, (phi_ssa_name, _, _)) in phi_data {
                    self.assign_variable(&var_name, &phi_ssa_name);
                }

                self.new_temp()
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
                    vec![
                        (then_label.clone(), then_result.clone()),
                        (else_label.clone(), else_result.clone()),
                    ],
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

                            self.emit(Instr::Phi(
                                phi.clone(),
                                vec![
                                    (then_label.clone(), t1.clone()),
                                    (else_label.clone(), t2.clone()),
                                ],
                            ));
                            self.assign_variable(var, &phi);
                        }

                        // Case 2: Variable modified in only the 'then' branch
                        (Some(t), None) => {
                            let outer_val = outer_scope.get(var).cloned().unwrap_or_else(|| {
                                let dummy = self.new_temp();
                                self.emit(Instr::Const(dummy.clone(), 0));
                                dummy
                            });

                            let phi = self.new_temp();
                            self.emit(Instr::Phi(
                                phi.clone(),
                                vec![
                                    (then_label.clone(), t.clone()),
                                    (else_label.clone(), outer_val),
                                ],
                            ));

                            self.assign_variable(var, &phi);
                        }

                        // Case 2 (variant): Variable modified only in the 'else' branch
                        (None, Some(t)) => {
                            let outer_val = outer_scope.get(var).cloned().unwrap_or_else(|| {
                                let dummy = self.new_temp();
                                self.emit(Instr::Const(dummy.clone(), 0));
                                dummy
                            });

                            let phi = self.new_temp();
                            self.emit(Instr::Phi(
                                phi.clone(),
                                vec![
                                    (then_label.clone(), outer_val),
                                    (else_label.clone(), t.clone()),
                                ],
                            ));
                            self.assign_variable(var, &phi);
                        }

                        // Case 3: Variable not modified in either branch
                        (None, None) => {
                            if outer_scope.contains_key(var) {
                                // Just bring the outer scope value into current scope
                                let phi = self.new_temp();
                                self.assign_variable(var, &phi);
                            }
                            // Do nothing if not in any scope
                        }

                        // Case 4: Modified in both branches with same value
                        (Some(t1), Some(t2)) if t1 == t2 => {
                            // No phi needed, just reuse same value
                            let phi = self.new_temp();
                            self.assign_variable(var, &phi);
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
                self.new_temp()
            }
            Stmt::Function { name, params, body } => {
                let func_name = name.lexeme.clone();
                let func_label = self.new_label(&func_name);

                // TODO: Store function IR separately. For now, just emit inline.
                self.emit(Instr::Label(func_label.clone()));

                // Create a new scope for the function
                self.with_scope(|ssa| {
                    // Assign SSA temporaries to parameters
                    // In a real implementation, these would be linked to the Call instruction's arguments
                    for param in params {
                        let param_temp = ssa.new_temp(); // Placeholder for argument value
                        ssa.assign_variable(&param.lexeme, &param_temp);
                        // We might need an 'Arg' instruction later to represent parameters formally
                        // ssa.emit(Instr::Arg { index: i, dest: param_temp }); 
                    }

                    // Generate IR for the function body
                    for stmt in body {
                        ssa.stmt_to_ir(stmt);
                    }

                    // Ensure functions implicitly return if no explicit return is present
                    // Check if the last instruction emitted was a Ret
                    // Note: This check is basic and might not cover all control flow paths.
                    if !matches!(ssa.instructions.last(), Some(Instr::Ret { .. })) {
                         ssa.emit(Instr::Ret { value: None });
                    }
                });
                // Function declaration itself doesn't produce a value in the current flow
                self.new_temp() // Return a dummy temp
            }
            Stmt::Return { keyword: _, value } => {
                let ret_val_temp = value.as_ref().map(|expr| self.expr_to_ir(expr));
                self.emit(Instr::Ret { value: ret_val_temp });
                self.new_temp() // Return statement doesn't produce a value in the expression sense
            }
            Stmt::Var { name, initializer } => {
                let expr = initializer
                    .clone()
                    .unwrap_or(Expr::Literal { value: Object::Nil });
                let rhs_temp = self.expr_to_ir(&expr);
                let var_name = name.lexeme.clone();

                // Variable declaration simply associates the name with the initializer's value
                self.assign_variable(&var_name, &rhs_temp);
                // A var declaration statement doesn't really have a "value",
                // but we return the initializer's temp in case it's needed.
                rhs_temp
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
                    TokenType::Modulo => self.emit(Instr::BinOp(temp.clone(), l, Op::Mod, r)),
                    TokenType::LeftShift => self.emit(Instr::BinOp(temp.clone(), l, Op::Shl, r)),
                    TokenType::RightShift => self.emit(Instr::BinOp(temp.clone(), l, Op::Shr, r)),
                    TokenType::BitAnd => self.emit(Instr::BinOp(temp.clone(), l, Op::BitAnd, r)),
                    TokenType::BitOr => self.emit(Instr::BinOp(temp.clone(), l, Op::BitOr, r)),
                    TokenType::EqualEqual => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Eq, r)),
                    TokenType::BangEqual => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Neq, r)),
                    TokenType::Less => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Lt, r)),
                    TokenType::LessEqual => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Lte, r)),
                    TokenType::Greater => self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Gt, r)),
                    TokenType::GreaterEqual => {
                        self.emit(Instr::Cmp(temp.clone(), l, CmpOp::Gte, r))
                    }
                    _ => panic!("Unsupported binary operator token: {:?}", operator.r#type),
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
            Expr::Variable { name } => match self.resolve_variable(&name.lexeme) {
                Some(ssa_name) => ssa_name.clone(),
                None => panic!("Variable called before definition {}", name.lexeme),
            },
            Expr::Assign { name, value } => {
                let rhs_temp = self.expr_to_ir(value);
                let var_name = name.lexeme.clone();
                // Assigning a variable just means updating the scope map
                // to point the variable name to the SSA temp of the value.
                self.assign_variable(&var_name, &rhs_temp);
                // The "result" of an assignment expression is the value assigned.
                rhs_temp
            }
            Expr::Call { callee, paren: _, arguments } => {
                 // Evaluate the callee expression. For now, assume it resolves to a variable
                 // which holds the function name (or is the function name itself).
                 let target_name = match callee.as_ref() {
                     Expr::Variable { name } => name.lexeme.clone(), // Simple case: direct function name
                     _ => panic!("Unsupported callee expression: {:?}", callee),
                 };
                let arg_temps = arguments.iter().map(|arg| self.expr_to_ir(arg)).collect();
                let result_temp = self.new_temp(); // Assume calls can return a value
                self.emit(Instr::Call { target: target_name, args: arg_temps, result: Some(result_temp.clone()) });
                result_temp
            }
            e => panic!("Unsupported expr: {e:?}"),
        }
    }

    fn emit(&mut self, instr: Instr) {
        self.instructions.push(instr);
    }
}
