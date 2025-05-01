pub mod passes;
pub mod registers;

use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;

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
    pub blocks: IndexMap<String, BasicBlock>,
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
        Self {
            scopes: vec![HashMap::new()],
            temp_counter: Default::default(),
            label_counter: Default::default(),
            var_versions: Default::default(),
            cfg: Default::default(),
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

    pub fn program_to_ir(&mut self, program: &[Stmt]) -> CFG {
        let main_label = "main".to_string(); // Use "main" as the entry point

        // Start the main block directly
        self.cfg.start_block(&main_label);
        self.cfg.current_block = Some(main_label.clone());

        // Keep track of the label of the block active *before* the last statement potentially changed it.
        // Initialize with main_label.
        let mut last_natural_flow_block = main_label.clone();

        // Process the program statements
        for stmt in program {
            // Before processing a statement that might change control flow (like If),
            // record the current block label.
            if let Some(current) = &self.cfg.current_block {
                last_natural_flow_block = current.clone();
            }
            self.stmt_to_ir(stmt); // This might change self.cfg.current_block
        }

        // Determine the block where execution logically ends.
        // If the last statement was an If/Loop, current_block is the merge block.
        // If the last statement was a simple expression or assignment, current_block is where it was emitted.
        // If the last statement was an explicit Return, current_block might be None.
        // Use the block that was active *before* the last statement if current_block is None now.
        let final_block_label = self
            .cfg
            .current_block
            .clone()
            .unwrap_or(last_natural_flow_block);

        // Check if this final block needs a terminator
        let needs_terminator = match self
            .cfg
            .blocks
            .get(&final_block_label)
            .and_then(|b| b.instrs.last())
        {
            Some(Instr::Ret { .. }) | Some(Instr::Jump(_)) | Some(Instr::BranchIf(_, _, _)) => {
                false
            }
            _ => true, // Needs a terminator
        };

        if needs_terminator {
            // Add the implicit return to the determined final block
            // Temporarily set current_block to add the instruction
            let original_current = self.cfg.current_block.clone();
            self.cfg.current_block = Some(final_block_label.clone()); // Clone label here
            self.cfg.emit(Instr::Ret { value: None }); // Default return 0
            self.cfg.current_block = original_current; // Restore
        }

        self.cfg.current_block = None; // Ensure current_block is cleared

        self.cfg.clone() // Return the constructed CFG
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

                // Jump from block before loop into the header
                self.cfg.emit(Instr::Jump(header_label.clone()));

                self.cfg.start_block(&header_label);
                self.cfg.current_block = Some(pre_header_label.clone());
                self.cfg.add_edge(&pre_header_label, &header_label); // header_label exists due to next step
                self.cfg.current_block = None; // Terminate pre-header block

                // --- Create ALL loop blocks early ---
                self.cfg.start_block(&body_label);
                self.cfg.start_block(&exit_label);
                // Restore current block to header for Phi/condition emission
                self.cfg.current_block = Some(header_label.clone());

                // --- Phi Node Setup (in header) ---
                let outer_scope_vars = self.scopes.last().cloned().unwrap_or_default();
                self.enter_scope(); // Enter scope for loop variables (including Phis)
                let mut phi_data = HashMap::new();
                let mut phi_instructions = Vec::new();
                for (var_name, outer_ssa_name) in &outer_scope_vars {
                    let phi_ssa_name = self.new_temp();
                    let phi_instr = Instr::Phi(
                        phi_ssa_name.clone(),
                        vec![(pre_header_label.clone(), outer_ssa_name.clone())], // Edge from pre-header
                    );
                    phi_instructions.push(phi_instr);
                    self.assign_variable(var_name, &phi_ssa_name);
                    phi_data.insert(var_name.clone(), (phi_ssa_name, outer_ssa_name.clone()));
                }
                for phi_instr in phi_instructions {
                    self.cfg.emit(phi_instr); // Emit Phis into header
                }

                // --- Condition Check (in header) ---
                let cond_temp = self.expr_to_ir(condition); // Condition uses Phi results
                self.cfg.emit(Instr::BranchIf(
                    cond_temp,
                    body_label.clone(), // If true, go to body
                    exit_label.clone(), // If false, go to exit
                ));
                // Add edges *after* BranchIf (target blocks already exist)
                self.cfg.add_edge(&header_label, &body_label);
                self.cfg.add_edge(&header_label, &exit_label);
                self.cfg.current_block = None; // Header block terminated

                // --- Loop Body ---
                // Block already created, just set current_block
                self.cfg.current_block = Some(body_label.clone());
                // Process body recursively
                match body.as_ref() {
                    Stmt::Block { statements } => {
                        for stmt in statements {
                            self.stmt_to_ir(stmt);
                            if self.cfg.current_block.is_none() {
                                break;
                            }
                        }
                    }
                    _ => {
                        self.stmt_to_ir(body);
                    }
                }
                let body_final_block_label_opt = self.cfg.current_block.clone();

                // --- Back Edge ---
                if let Some(ref body_final_label) = body_final_block_label_opt {
                    let needs_jump = match self
                        .cfg
                        .blocks
                        .get(body_final_label)
                        .and_then(|b| b.instrs.last())
                    {
                        Some(Instr::Ret { .. })
                        | Some(Instr::Jump(_))
                        | Some(Instr::BranchIf(_, _, _)) => false,
                        _ => true,
                    };
                    if needs_jump {
                        self.cfg.emit(Instr::Jump(header_label.clone()));
                        self.cfg.add_edge(body_final_label, &header_label); // header exists
                    } else if self.cfg.blocks.contains_key(body_final_label) {
                        self.cfg.add_edge(body_final_label, &header_label); // header exists
                    }
                }
                self.cfg.current_block = None;

                // --- Phi Patching ---
                let final_body_scope = self.scopes.last().cloned().unwrap_or_default();
                let mut back_edge_values = HashMap::new();
                for (var_name, (phi_ssa_name, initial_value_ssa)) in &phi_data {
                    if let Some(end_of_body_ssa_name) = final_body_scope.get(var_name) {
                        back_edge_values.insert(phi_ssa_name.clone(), end_of_body_ssa_name.clone());
                    } else {
                        // Variable not reassigned in loop body, use initial value for back-edge
                        back_edge_values.insert(phi_ssa_name.clone(), initial_value_ssa.clone());
                    }
                }
                if let Some(ref final_label) = body_final_block_label_opt {
                    if let Some(header_block) = self.cfg.blocks.get_mut(&header_label) {
                        for instr in header_block.instrs.iter_mut() {
                            if let Instr::Phi(phi_ssa_name, edges) = instr {
                                if let Some(back_edge_val) = back_edge_values.get(phi_ssa_name) {
                                    edges.push((final_label.clone(), back_edge_val.clone()));
                                } else {
                                    eprintln!(
                                        "Error: Could not find back-edge value for Phi node result '{}'",
                                        phi_ssa_name
                                    );
                                }
                            }
                        }
                    } else {
                        panic!(
                            "Loop header block '{}' not found for Phi patching.",
                            header_label
                        );
                    }
                }

                // --- Loop Exit ---
                // Block already created, just set current_block
                self.cfg.current_block = Some(exit_label.clone());

                // --- Scope Handling ---
                self.exit_scope(); // Exit loop body scope
                if let Some(outer_scope) = self.scopes.last_mut() {
                    for (var_name, (phi_ssa_name, _)) in phi_data {
                        outer_scope.insert(var_name.clone(), phi_ssa_name.clone());
                    }
                } else {
                    panic!("Cannot update outer scope after loop - scope stack empty.");
                }

                self.new_temp() // Return dummy temp for the statement
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // 1. Get current block label (predecessor)
                let pre_if_label = self
                    .cfg
                    .current_block
                    .clone()
                    .expect("If statement needs a current block");

                // 2. Evaluate condition (emits instructions into pre_if_label block)
                let cond_t = self.expr_to_ir(condition);

                // 3. Create labels
                let then_label = self.new_label("if_then");
                let else_label = self.new_label("if_else");
                let merge_label = self.new_label("if_merge");

                // 4. Create Merge Block *instance* early (doesn't set current_block)
                self.cfg.start_block(&then_label);
                self.cfg.start_block(&else_label);
                self.cfg.start_block(&merge_label);
                // Note: start_block sets current_block, so we must reset it
                self.cfg.current_block = Some(pre_if_label.clone()); // Restore current block to pre_if_label

                // 5. Emit Conditional Branch into pre_if_label block
                //    current_block is still pre_if_label here. This is CRITICAL.
                self.cfg.emit(Instr::BranchIf(
                    cond_t.clone(),
                    then_label.clone(),
                    else_label.clone(),
                ));

                // 6. Add edges from pre_if_label to then/else blocks
                self.cfg.add_edge(&pre_if_label, &then_label);
                self.cfg.add_edge(&pre_if_label, &else_label);

                // 7. Terminate pre_if_label block processing - current_block becomes None
                self.cfg.current_block = None;

                // --- Then Branch ---
                self.cfg.start_block(&then_label); // Define the 'then' block
                self.cfg.current_block = Some(then_label.clone()); // Set current for processing
                let then_scope = self.with_scope(|ssa| {
                    ssa.stmt_to_ir(then_branch);
                    ssa.scopes.last().cloned().unwrap_or_default()
                });
                // Check if 'then' block needs explicit jump to merge
                let then_final_label = self.cfg.current_block.clone(); // Label after processing then_branch
                if let Some(current_then_label) = then_final_label {
                    // If the block is still active (didn't return/break/etc.)
                    let needs_jump = match self
                        .cfg
                        .blocks
                        .get(&current_then_label)
                        .and_then(|b| b.instrs.last())
                    {
                        Some(Instr::Ret { .. })
                        | Some(Instr::Jump(_))
                        | Some(Instr::BranchIf(_, _, _)) => false,
                        _ => true,
                    };
                    if needs_jump {
                        self.cfg.emit(Instr::Jump(merge_label.clone()));
                        self.cfg.add_edge(&current_then_label, &merge_label);
                    }
                }
                self.cfg.current_block = None; // Then branch processing finished

                // --- Else Branch ---
                self.cfg.start_block(&else_label); // Define the 'else' block
                self.cfg.current_block = Some(else_label.clone()); // Set current for processing
                let else_scope = self.with_scope(|ssa| {
                    if let Some(else_stmt) = &**else_branch {
                        ssa.stmt_to_ir(else_stmt);
                    }
                    ssa.scopes.last().cloned().unwrap_or_default()
                });
                // Check if 'else' block needs explicit jump to merge
                let else_final_label = self.cfg.current_block.clone();
                if let Some(current_else_label) = else_final_label {
                    let needs_jump = match self
                        .cfg
                        .blocks
                        .get(&current_else_label)
                        .and_then(|b| b.instrs.last())
                    {
                        Some(Instr::Ret { .. })
                        | Some(Instr::Jump(_))
                        | Some(Instr::BranchIf(_, _, _)) => false,
                        _ => true,
                    };
                    if needs_jump {
                        self.cfg.emit(Instr::Jump(merge_label.clone()));
                        self.cfg.add_edge(&current_else_label, &merge_label);
                    }
                }
                self.cfg.current_block = None; // Else branch processing finished

                // --- Merge Block ---
                // Set current block to merge block to emit Phi nodes AND for subsequent statements
                self.cfg.current_block = Some(merge_label.clone());

                self.enter_scope(); // Enter merge scope

                // --- Phi Nodes for Merging ---
                let pre_if_scope = self
                    .scopes
                    .get(self.scopes.len().saturating_sub(2)) // Scope before the if
                    .cloned()
                    .unwrap_or_default();

                let all_vars: HashSet<_> = then_scope
                    .keys()
                    .chain(else_scope.keys())
                    .chain(pre_if_scope.keys())
                    .cloned()
                    .collect();

                for var_name in &all_vars {
                    let then_val = then_scope.get(var_name);
                    let else_val = else_scope.get(var_name);
                    let pre_if_val = pre_if_scope.get(var_name);

                    match (then_val, else_val) {
                        // ... (existing Phi logic - seems okay) ...
                        (Some(t_val), Some(e_val)) => {
                            // Both defined
                            if t_val != e_val {
                                let phi_res = self.new_temp();
                                self.cfg.emit(Instr::Phi(
                                    phi_res.clone(),
                                    vec![
                                        (then_label.clone(), t_val.clone()),
                                        (else_label.clone(), e_val.clone()),
                                    ],
                                ));
                                self.assign_variable(var_name, &phi_res);
                            } else {
                                self.assign_variable(var_name, t_val);
                            }
                        }
                        (Some(t_val), None) => {
                            // Only in Then
                            if let Some(pre_val) = pre_if_val {
                                let phi_res = self.new_temp();
                                self.cfg.emit(Instr::Phi(
                                    phi_res.clone(),
                                    vec![
                                        (then_label.clone(), t_val.clone()),
                                        (else_label.clone(), pre_val.clone()),
                                    ],
                                ));
                                self.assign_variable(var_name, &phi_res);
                            } else {
                                self.assign_variable(var_name, t_val); /* Warning emitted before */
                            }
                        }
                        (None, Some(e_val)) => {
                            // Only in Else
                            if let Some(pre_val) = pre_if_val {
                                let phi_res = self.new_temp();
                                self.cfg.emit(Instr::Phi(
                                    phi_res.clone(),
                                    vec![
                                        (then_label.clone(), pre_val.clone()),
                                        (else_label.clone(), e_val.clone()),
                                    ],
                                ));
                                self.assign_variable(var_name, &phi_res);
                            } else {
                                self.assign_variable(var_name, e_val); /* Warning emitted before */
                            }
                        }
                        (None, None) => {
                            // Neither branch
                            if let Some(pre_val) = pre_if_val {
                                self.assign_variable(var_name, pre_val);
                            }
                        }
                    }
                } // End Phi loop

                // After processing Phis, the merge block remains the current continuation point.
                // Leave self.cfg.current_block as Some(merge_label)

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
                        let needs_ret = match ssa
                            .cfg
                            .blocks
                            .get(last_block_label)
                            .and_then(|b| b.instrs.last())
                        {
                            Some(Instr::Ret { .. })
                            | Some(Instr::Jump(_))
                            | Some(Instr::BranchIf { .. }) => false,
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

                if matches!(expr, Expr::Literal { value: Object::Nil }) {
                    rhs_temp.is_empty();
                }

                rhs_temp
            }
            stmt => {
                eprintln!(
                    "Warning: Statement type {:?} not fully supported in SSA generation.",
                    stmt
                );
                self.new_temp()
            }
        }
    }

    fn expr_to_ir(&mut self, expr: &Expr) -> String {
        if self.cfg.current_block.is_none() {
            panic!(
                "Attempted to generate IR for expression {:?} without a current block.",
                expr
            );
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
                    TokenType::LeftShift => {
                        self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::Shl, r))
                    }
                    TokenType::RightShift => {
                        self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::Shr, r))
                    }
                    TokenType::BitAnd => {
                        self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::BitAnd, r))
                    }
                    TokenType::BitOr => self.cfg.emit(Instr::BinOp(temp.clone(), l, Op::BitOr, r)),
                    TokenType::EqualEqual => {
                        self.cfg.emit(Instr::Cmp(temp.clone(), l, CmpOp::Eq, r))
                    }
                    TokenType::BangEqual => {
                        self.cfg.emit(Instr::Cmp(temp.clone(), l, CmpOp::Neq, r))
                    }
                    TokenType::Less => self.cfg.emit(Instr::Cmp(temp.clone(), l, CmpOp::Lt, r)),
                    TokenType::LessEqual => {
                        self.cfg.emit(Instr::Cmp(temp.clone(), l, CmpOp::Lte, r))
                    }
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
                let entry_block = self
                    .cfg
                    .current_block
                    .clone()
                    .expect("Logical expr needs current block");
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
                        self.cfg.emit(Instr::BranchIf(
                            phi,
                            merge_label.clone(),
                            right_eval_label.clone(),
                        ));
                    }
                    TokenType::Or => {
                        let phi = self.new_temp();
                        // ... Cmp ...
                        // If true (short-circuit), go to merge; otherwise, evaluate right.
                        self.cfg.emit(Instr::BranchIf(
                            phi,
                            merge_label.clone(),
                            right_eval_label.clone(),
                        ));
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
                let right_eval_final_block = self
                    .cfg
                    .current_block
                    .clone()
                    .unwrap_or_else(|| right_eval_label.clone());
                self.cfg.emit(Instr::Jump(merge_label.clone())); // Jump to merge
                // self.cfg.add_edge(&right_eval_final_block, &merge_label); // Moved down
                self.cfg.current_block = None; // Right eval block terminated

                // --- Merge Block ---
                self.cfg.start_block(&merge_label); // Create merge block
                self.cfg.add_edge(&entry_block, &merge_label); // Add edge *after* creation
                self.cfg.add_edge(&right_eval_final_block, &merge_label); // Add edge *after* creation
                self.cfg.current_block = Some(merge_label.clone());

                // --- Phi Node ---
                // ... (Phi node generation) ...

                self.new_temp()
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
                eprintln!(
                    "Warning: Expression type {:?} not fully supported in SSA generation.",
                    e
                );
                let temp = self.new_temp();
                self.cfg.emit(Instr::Const(temp.clone(), 0));
                temp
            }
        }
    }
}
