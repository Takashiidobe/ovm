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
    Call {
        target: String,
        args: Vec<String>,
        result: Option<String>,
    },
    Ret {
        value: Option<String>,
    },
    Assign(String, String),
    FuncParam {
        name: String,
        index: usize,
    },
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
    var_versions: HashMap<String, u64>,
    cfg: CFG,
}

impl Default for SSA {
    fn default() -> Self {
        let mut ssa = Self {
            scopes: vec![HashMap::new()],
            temp_counter: Default::default(),
            label_counter: Default::default(),
            var_versions: Default::default(),
            cfg: Default::default(),
        };
        let entry_label = "entry".to_string();
        ssa.cfg.start_block(&entry_label);
        ssa.cfg.current_block = Some(entry_label);
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

    pub fn program_to_ir(&mut self, stmts: &[Stmt]) -> CFG {
        let mut main_stmts = Vec::new();
        let mut func_stmts = Vec::new();
        for stmt in stmts {
            if matches!(stmt, Stmt::Function { .. }) {
                func_stmts.push(stmt);
            } else {
                main_stmts.push(stmt);
            }
        }

        let main_label = "main".to_string();
        let entry_label = self.cfg.current_block.clone().expect("Entry block not set");

        self.cfg.start_block(&main_label);
        self.cfg.add_edge(&entry_label, &main_label);
        self.cfg.current_block = Some(main_label.clone());

        for stmt in main_stmts {
            eprintln!("Processing main stmt: {:?}", stmt);
            self.stmt_to_ir(stmt);
        }

        if self.cfg.current_block.is_some() {
            self.cfg.emit(Instr::Ret { value: None });
        }

        for stmt in func_stmts {
            eprintln!("Processing func stmt: {:?}", stmt);
            let current_block_backup = self.cfg.current_block.clone();
            self.cfg.current_block = None;
            self.stmt_to_ir(stmt);
            self.cfg.current_block = current_block_backup;
        }

        self.cfg.clone()
    }

    fn stmt_to_ir(&mut self, stmt: &Stmt) -> String {
        if self.cfg.current_block.is_none() && !matches!(stmt, Stmt::Function { .. }) {
            let orphan_label = self.new_label("orphan");
            eprintln!(
                "Warning: Emitting statement {:?} into newly created orphan block '{}'. Check CFG logic.",
                stmt, orphan_label
            );
            self.cfg.start_block(&orphan_label);
        }

        match stmt {
            Stmt::Expression { expr } => self.expr_to_ir(expr),
            Stmt::Print { expr } => {
                let to_print = self.expr_to_ir(expr);
                self.cfg.emit(Instr::Print(to_print.clone()));
                to_print
            }
            Stmt::While { condition, body } => {
                let header_label = self.new_label("loop_header");
                let body_label = self.new_label("loop_body");
                let exit_label = self.new_label("loop_exit");

                let pre_header_label = self
                    .cfg
                    .current_block
                    .clone()
                    .expect("Current block must be set before a while loop");

                self.cfg.emit(Instr::Jump(header_label.clone()));

                self.cfg.start_block(&header_label);
                self.cfg.add_edge(&pre_header_label, &header_label);
                self.cfg.current_block = Some(header_label.clone());

                let outer_scope_vars = self
                    .scopes
                    .last()
                    .cloned()
                    .unwrap_or_default();

                self.enter_scope();

                let mut phi_data = HashMap::new();

                let mut phi_instructions = Vec::new();

                for (var_name, outer_ssa_name) in &outer_scope_vars {
                    let phi_ssa_name = self.new_temp();
                    let phi_instr = Instr::Phi(
                        phi_ssa_name.clone(),
                        vec![(pre_header_label.clone(), outer_ssa_name.clone())],
                    );
                    phi_instructions.push(phi_instr);
                    self.assign_variable(var_name, &phi_ssa_name);
                    phi_data.insert(var_name.clone(), (phi_ssa_name, outer_ssa_name.clone()));
                }

                for phi_instr in phi_instructions {
                    self.cfg.emit(phi_instr);
                }

                let cond_temp = self.expr_to_ir(condition);

                self.cfg.emit(Instr::BranchIf(
                    cond_temp,
                    body_label.clone(),
                    exit_label.clone(),
                ));
                self.cfg.current_block = None;

                self.cfg.start_block(&body_label);
                self.cfg.add_edge(&header_label, &body_label);
                self.cfg.current_block = Some(body_label.clone());

                match body.as_ref() {
                    Stmt::Block { statements } => {
                        for stmt in statements {
                            self.stmt_to_ir(stmt);
                        }
                    }
                    _ => {
                        self.stmt_to_ir(body);
                    }
                }

                let mut back_edge_values = HashMap::new();
                let current_body_scope = self.scopes.last().cloned().unwrap_or_default();

                for (var_name, (phi_ssa_name, _)) in &phi_data {
                    if let Some(end_of_body_ssa_name) = current_body_scope.get(var_name) {
                        back_edge_values.insert(var_name.clone(), end_of_body_ssa_name.clone());
                    } else {
                        eprintln!(
                            "Warning: Variable '{}' with phi node '{}' not found in scope at end of loop body. Using phi result itself for back-edge.",
                            var_name, phi_ssa_name
                        );
                        back_edge_values.insert(var_name.clone(), phi_ssa_name.clone());
                    }
                }

                if self.cfg.current_block.as_ref() == Some(&body_label) {
                    self.cfg.emit(Instr::Jump(header_label.clone()));
                    self.cfg.add_edge(&body_label, &header_label);
                } else {
                    eprintln!(
                        "Warning: Loop body block '{}' did not end naturally. Back-edge jump might be missing or incorrect.",
                        body_label
                    );
                    if self.cfg.blocks.contains_key(&body_label) {
                        self.cfg.add_edge(&body_label, &header_label);
                    }
                }
                self.cfg.current_block = None;

                if let Some(header_block) = self.cfg.blocks.get_mut(&header_label) {
                    for instr in header_block.instrs.iter_mut() {
                        if let Instr::Phi(phi_ssa_name, edges) = instr {
                            let maybe_var_name = phi_data.iter().find_map(|(vn, (psn, _))| if psn == phi_ssa_name { Some(vn) } else { None });
                            if let Some(var_name) = maybe_var_name {
                                if let Some(back_edge_val) = back_edge_values.get(var_name) {
                                    edges.push((body_label.clone(), back_edge_val.clone()));
                                } else {
                                    eprintln!("Error: Could not find back-edge value for variable '{}' (Phi: {})", var_name, phi_ssa_name);
                                }
                            } else {
                                eprintln!("Error: Could not find original variable name for Phi node result '{}'", phi_ssa_name);
                            }
                        }
                    }
                } else {
                    panic!("Loop header block '{}' not found for Phi patching.", header_label);
                }

                self.cfg.start_block(&exit_label);
                self.cfg.add_edge(&header_label, &exit_label);
                self.cfg.current_block = Some(exit_label.clone());

                self.exit_scope();
                self.enter_scope();

                for (var_name, (phi_ssa_name, _)) in phi_data {
                    self.assign_variable(&var_name, &phi_ssa_name);
                }

                self.new_temp()
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let pre_if_label = self.cfg.current_block.clone().expect("If statement needs a current block");

                let cond_t = self.expr_to_ir(condition);

                let then_label = self.new_label("if_then");
                let else_label = self.new_label("if_else");
                let merge_label = self.new_label("if_merge");

                // --- Create Merge Block *before* branches ---
                // This ensures the block exists when edges are added from then/else.
                self.cfg.start_block(&merge_label);
                // We don't set current_block to merge_label yet.

                // --- Emit Conditional Branch from pre_if_label block ---
                self.cfg.emit(Instr::BranchIf(
                    cond_t.clone(),
                    then_label.clone(),
                    else_label.clone(),
                ));
                // Add edges from pre_if to then/else *after* emitting branch
                // self.cfg.add_edge(&pre_if_label, &then_label); // Moved down
                // self.cfg.add_edge(&pre_if_label, &else_label); // Moved down
                self.cfg.current_block = None; // pre_if_label block is now terminated


                // --- Then Branch ---
                self.cfg.start_block(&then_label); // Create then block
                self.cfg.add_edge(&pre_if_label, &then_label); // Add edge now
                self.cfg.current_block = Some(then_label.clone());
                let then_scope = self.with_scope(|ssa| {
                    ssa.stmt_to_ir(then_branch); // Process the then branch statements
                    ssa.scopes.last().cloned().unwrap_or_default()
                });
                // Ensure 'then' block jumps to merge *if* it hasn't terminated otherwise
                if self.cfg.current_block.as_ref() == Some(&then_label) {
                    self.cfg.emit(Instr::Jump(merge_label.clone()));
                    self.cfg.add_edge(&then_label, &merge_label); // Add edge to merge (merge block exists now)
                } else if self.cfg.blocks.contains_key(&then_label) {
                     // Block exists but terminated differently (e.g., return).
                     // Still need edge for CFG completeness if block wasn't empty.
                     // Check if the block actually has instructions before adding edge.
                     if !self.cfg.blocks[&then_label].instrs.is_empty() {
                          // Add edge even if terminated, for liveness/analysis? Or skip?
                          // Let's add it for now.
                          self.cfg.add_edge(&then_label, &merge_label);
                     }
                }
                self.cfg.current_block = None; // Then branch finished


                // --- Else Branch ---
                self.cfg.start_block(&else_label); // Create else block
                self.cfg.add_edge(&pre_if_label, &else_label); // Add edge now
                self.cfg.current_block = Some(else_label.clone());
                let else_scope = self.with_scope(|ssa| {
                    if let Some(else_stmt) = &**else_branch { // Use if let Some to handle Option<&Stmt>
                        ssa.stmt_to_ir(else_stmt); // Process else branch statements
                    } else {
                        // Empty else block - still needs to jump to merge
                        // No value produced, handled by Phi logic later
                    }
                    ssa.scopes.last().cloned().unwrap_or_default()
                });
                 // Ensure 'else' block jumps to merge if it hasn't terminated otherwise
                if self.cfg.current_block.as_ref() == Some(&else_label) {
                    self.cfg.emit(Instr::Jump(merge_label.clone()));
                    self.cfg.add_edge(&else_label, &merge_label); // Add edge to merge (merge block exists now)
                } else if self.cfg.blocks.contains_key(&else_label) {
                     // Block exists but terminated differently. Add edge if not empty.
                     if !self.cfg.blocks[&else_label].instrs.is_empty() {
                         self.cfg.add_edge(&else_label, &merge_label);
                     }
                }
                self.cfg.current_block = None; // Else branch finished


                // --- Merge Block (already created) ---
                // Set current block to merge block to emit Phi nodes
                self.cfg.current_block = Some(merge_label.clone());

                self.enter_scope(); // Enter merge scope

                // --- Phi Nodes for Merging ---
                let pre_if_scope = self
                    .scopes
                    .get(self.scopes.len().saturating_sub(2)) // Scope before the if
                    .cloned()
                    .unwrap_or_default();

                let all_vars: HashSet<_> = then_scope // Use HashSet directly
                    .keys()
                    .chain(else_scope.keys())
                    .chain(pre_if_scope.keys()) // Include vars defined before the if
                    .cloned()
                    .collect();

                for var_name in &all_vars { // Iterate over reference
                    let then_val = then_scope.get(var_name);
                    let else_val = else_scope.get(var_name);
                    let pre_if_val = pre_if_scope.get(var_name); // Value before the if

                    match (then_val, else_val) {
                        // Defined/Modified in both branches
                        (Some(t_val), Some(e_val)) => {
                            if t_val != e_val {
                                // Different values, need Phi
                                let phi_res = self.new_temp();
                                self.cfg.emit(Instr::Phi(
                                    phi_res.clone(),
                                    vec![
                                        (then_label.clone(), t_val.clone()),
                                        (else_label.clone(), e_val.clone()),
                                    ],
                                ));
                                self.assign_variable(var_name, &phi_res); // Use var_name directly
                            } else {
                                // Same value, no Phi needed, just assign
                                self.assign_variable(var_name, t_val);
                            }
                        }
                        // Defined/Modified only in 'then'
                        (Some(t_val), None) => {
                            if let Some(pre_val) = pre_if_val {
                                // Need Phi with 'then' value and pre-if value
                                let phi_res = self.new_temp();
                                self.cfg.emit(Instr::Phi(
                                    phi_res.clone(),
                                    vec![
                                        (then_label.clone(), t_val.clone()),
                                        (else_label.clone(), pre_val.clone()), // Use value from before if
                                    ],
                                ));
                                self.assign_variable(var_name, &phi_res);
                            } else {
                                // Variable introduced only in 'then'.
                                eprintln!("Warning: Variable '{}' defined only in 'then' branch.", var_name);
                                // Assigning t_val might be incorrect. Consider undef/error?
                                // For now, assign it, but this relies on dominance or specific language rules.
                                self.assign_variable(var_name, t_val);
                            }
                        }
                         // Defined/Modified only in 'else'
                        (None, Some(e_val)) => {
                             if let Some(pre_val) = pre_if_val {
                                // Need Phi with pre-if value and 'else' value
                                let phi_res = self.new_temp();
                                self.cfg.emit(Instr::Phi(
                                    phi_res.clone(),
                                    vec![
                                        (then_label.clone(), pre_val.clone()), // Use value from before if
                                        (else_label.clone(), e_val.clone()),
                                    ],
                                ));
                                self.assign_variable(var_name, &phi_res);
                            } else {
                                eprintln!("Warning: Variable '{}' defined only in 'else' branch.", var_name);
                                self.assign_variable(var_name, e_val);
                            }
                        }
                        // Not defined/modified in either branch
                        (None, None) => {
                            if let Some(pre_val) = pre_if_val {
                                // Variable existed before, carries through unchanged. Assign it.
                                self.assign_variable(var_name, pre_val);
                            }
                            // Otherwise, variable doesn't exist here.
                        }
                    }
                }

                // If statements don't produce a value in this context
                self.new_temp() // Return dummy temp
            }
            Stmt::Block { statements } => {
                let mut last_val = self.new_temp();
                self.with_scope(|ssa| {
                    for stmt in statements {
                        last_val = ssa.stmt_to_ir(stmt);
                        if ssa.cfg.current_block.is_none() {
                            break;
                        }
                    }
                });
                last_val
            }
            Stmt::Function { name, params, body } => {
                let func_name = name.lexeme.clone();
                let func_label = func_name.clone();

                let outer_block_backup = self.cfg.current_block.take();

                self.cfg.start_block(&func_label);
                self.cfg.current_block = Some(func_label.clone());

                self.with_scope(|ssa| {
                    for (i, param) in params.iter().enumerate() {
                        let param_temp = ssa.new_temp();
                        ssa.cfg.emit(Instr::FuncParam {
                            name: param_temp.clone(),
                            index: i,
                        });
                        ssa.assign_variable(&param.lexeme, &param_temp);
                    }

                    for stmt in body {
                        ssa.stmt_to_ir(stmt);
                        if ssa.cfg.current_block.is_none() {
                            break;
                        }
                    }

                    if let Some(last_block_label) = &ssa.cfg.current_block {
                        let needs_ret = match ssa.cfg.blocks.get(last_block_label).and_then(|b| b.instrs.last()) {
                            Some(Instr::Ret { .. }) | Some(Instr::Jump(_)) | Some(Instr::BranchIf { .. }) => false,
                            _ => true,
                        };
                        if needs_ret {
                            ssa.cfg.emit(Instr::Ret { value: None });
                        }
                    }
                });

                self.cfg.current_block = outer_block_backup;

                self.new_temp()
            }
            Stmt::Return { keyword: _, value } => {
                let ret_val_temp = value.as_ref().map(|expr| self.expr_to_ir(expr));
                self.cfg.emit(Instr::Ret {
                    value: ret_val_temp,
                });
                self.cfg.current_block = None;
                self.new_temp()
            }
            Stmt::Var { name, initializer } => {
                let expr = initializer
                    .clone()
                    .unwrap_or(Expr::Literal { value: Object::Nil });
                let rhs_temp = self.expr_to_ir(&expr);
                let var_name = name.lexeme.clone();

                self.assign_variable(&var_name, &rhs_temp);

                if matches!(expr, Expr::Literal { value: Object::Nil }) && !rhs_temp.is_empty() {
                }

                rhs_temp
            }
            stmt => {
                eprintln!("Warning: Statement type {:?} not fully supported in SSA generation.", stmt);
                self.new_temp()
            }
        }
    }

    fn expr_to_ir(&mut self, expr: &Expr) -> String {
        if self.cfg.current_block.is_none() {
            panic!("Attempted to generate IR for expression {:?} without a current block.", expr);
        }

        match expr {
            Expr::Literal { value } => match value {
                Object::Integer(n) => {
                    let temp = self.new_temp();
                    self.cfg.emit(Instr::Const(temp.clone(), *n));
                    temp
                }
                Object::Nil => {
                    let temp = self.new_temp();
                    self.cfg.emit(Instr::Const(temp.clone(), 0));
                    temp
                }
                _ => {
                    eprintln!("Warning: Literal type {:?} not fully supported.", value);
                    let temp = self.new_temp();
                    self.cfg.emit(Instr::Const(temp.clone(), 0));
                    temp
                }
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
                    TokenType::Plus => self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::Add, r)),
                    TokenType::Minus => self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::Sub, r)),
                    TokenType::Star => self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::Mul, r)),
                    TokenType::Slash => self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::Div, r)),
                    TokenType::Modulo => self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::Mod, r)),
                    TokenType::LeftShift => self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::Shl, r)),
                    TokenType::RightShift => self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::Shr, r)),
                    TokenType::BitAnd => self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::BitAnd, r)),
                    TokenType::BitOr => self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::BitOr, r)),
                    TokenType::EqualEqual => self.cfg.emit(Instr::Cmp(temp.clone(), l, CmpOp::Eq, r)),
                    TokenType::BangEqual => self.cfg.emit(Instr::Cmp(temp.clone(), l, CmpOp::Neq, r)),
                    TokenType::Less => self.cfg.emit(Instr::Cmp(temp.clone(), l, CmpOp::Lt, r)),
                    TokenType::LessEqual => self.cfg.emit(Instr::Cmp(temp.clone(), l, CmpOp::Lte, r)),
                    TokenType::Greater => self.cfg.emit(Instr::Cmp(temp.clone(), l, CmpOp::Gt, r)),
                    TokenType::GreaterEqual => {
                        self.cfg.emit(Instr::Cmp(temp.clone(), l, CmpOp::Gte, r))
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
                let entry_block = self.cfg.current_block.clone().expect("Logical expr needs current block");
                let right_eval_label = self.new_label("logical_right");
                let merge_label = self.new_label("logical_merge");

                // --- Generate constants needed for Phi node *before* branching ---
                // ... (constants generation) ...
                // --- Generate constant 0 for comparison ---
                // ... (constant 0 generation) ...

                // --- Evaluate left operand ---
                let left_val = self.expr_to_ir(left);

                // --- Conditional Branch ---
                match operator.r#type {
                    TokenType::And => {

                        let phi = self.new_temp();
                        // ... Cmp ...
                        // If false (short-circuit), go to merge; otherwise, evaluate right.
                        self.cfg.emit(Instr::BranchIf(phi, merge_label.clone(), right_eval_label.clone()));
                    }
                    TokenType::Or => {
                        let phi = self.new_temp();
                        // ... Cmp ...
                        // If true (short-circuit), go to merge; otherwise, evaluate right.
                        self.cfg.emit(Instr::BranchIf(phi, merge_label.clone(), right_eval_label.clone()));
                    }
                    _ => unreachable!(),
                }
                // --- Move add_edge calls AFTER target blocks are created ---
                // self.cfg.add_edge(&entry_block, &merge_label); // Moved down
                // self.cfg.add_edge(&entry_block, &right_eval_label); // Moved down
                self.cfg.current_block = None; // Entry block terminated

                // --- Right Evaluation Block ---
                self.cfg.start_block(&right_eval_label); // Create right_eval block
                self.cfg.add_edge(&entry_block, &right_eval_label); // Add edge *after* creation
                self.cfg.current_block = Some(right_eval_label.clone());
                let right_val = self.expr_to_ir(right); // Evaluate right operand
                let right_eval_final_block = self.cfg.current_block.clone().unwrap_or_else(|| right_eval_label.clone());
                self.cfg.emit(Instr::Jump(merge_label.clone())); // Jump to merge
                // self.cfg.add_edge(&right_eval_final_block, &merge_label); // Moved down
                self.cfg.current_block = None; // Right eval block terminated

                // --- Merge Block ---
                self.cfg.start_block(&merge_label); // Create merge block
                self.cfg.add_edge(&entry_block, &merge_label); // Add edge *after* creation
                self.cfg.add_edge(&right_eval_final_block, &merge_label); // Add edge *after* creation
                self.cfg.current_block = Some(merge_label.clone());
                let result_temp = self.new_temp();

                // --- Phi Node ---
                // ... (Phi node generation) ...

                result_temp
            }
            Expr::Variable { name } => match self.resolve_variable(&name.lexeme) {
                Some(ssa_name) => ssa_name.clone(),
                None => panic!("Variable '{}' used before definition.", name.lexeme),
            },
            Expr::Assign { name, value } => {
                let rhs_temp = self.expr_to_ir(value);
                let var_name = name.lexeme.clone();
                self.assign_variable(&var_name, &rhs_temp);
                rhs_temp
            }
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                let target_name = match callee.as_ref() {
                    Expr::Variable { name } => name.lexeme.clone(),
                    _ => panic!("Unsupported callee expression: {:?}", callee),
                };
                let arg_temps = arguments.iter().map(|arg| self.expr_to_ir(arg)).collect();
                let result_temp = self.new_temp();
                self.cfg.emit(Instr::Call {
                    target: target_name,
                    args: arg_temps,
                    result: Some(result_temp.clone()),
                });
                result_temp
            }
            e => {
                eprintln!("Warning: Expression type {:?} not fully supported in SSA generation.", e);
                let temp = self.new_temp();
                self.cfg.emit(Instr::Const(temp.clone(), 0));
                temp
            }
        }
    }
}
