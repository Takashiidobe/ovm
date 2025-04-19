use crate::optimizer::{CmpOp, Instr, Op};
use std::collections::HashMap; // Make sure Instr, Op, CmpOp are imported

struct Vm<'a> {
    instrs: &'a [Instr],
    pc: usize,
    variables: HashMap<String, i64>,
    labels: HashMap<String, usize>,
    total_cycles: u64,
}

impl<'a> Vm<'a> {
    fn new(instrs: &'a [Instr]) -> Self {
        let mut labels = HashMap::new();
        for (idx, instr) in instrs.iter().enumerate() {
            if let Instr::Label(name) = instr {
                // Allow redefining labels, last definition wins (like assembly)
                labels.insert(name.clone(), idx);
            }
        }
        Vm {
            instrs,
            pc: 0,
            variables: HashMap::new(),
            labels,
            total_cycles: 0,
        }
    }

    fn get_var(&self, name: &str) -> i64 {
        *self
            .variables
            .get(name)
            .unwrap_or_else(|| panic!("Variable '{}' used before definition", name))
    }

    fn run(&mut self) -> u64 {
        while let Some(instr) = self.instrs.get(self.pc) {
            let mut next_pc = self.pc + 1; // Default next instruction

            let cycles = match instr {
                Instr::Const(dest, val) => {
                    self.variables.insert(dest.clone(), *val);
                    1
                }
                Instr::BinOp(dest, left, op, right) => {
                    let lval = self.get_var(left);
                    let rval = self.get_var(right);
                    let result = match op {
                        Op::Add => lval + rval,
                        Op::Sub => lval - rval,
                        Op::Mul => lval * rval,
                        Op::Div => {
                            if rval == 0 {
                                panic!("Division by zero");
                            }
                            lval / rval
                        }
                        Op::LShift => lval << rval,
                        Op::RShift => lval >> rval,
                        Op::BitAnd | Op::And => lval & rval, // Treat logical and bitwise AND similarly for i64
                        Op::BitOr | Op::Or => lval | rval, // Treat logical and bitwise OR similarly for i64
                    };
                    self.variables.insert(dest.clone(), result);
                    1 // Cycle cost
                }
                Instr::Print(var) => {
                    self.get_var(var);
                    10 // Cycle cost (I/O)
                }
                Instr::Cmp(dest, left, cmp_op, right) => {
                    let lval = self.get_var(left);
                    let rval = self.get_var(right);
                    let result = match cmp_op {
                        CmpOp::Eq => lval == rval,
                        CmpOp::Neq => lval != rval,
                        CmpOp::Lt => lval < rval,
                        CmpOp::Lte => lval <= rval,
                        CmpOp::Gt => lval > rval,
                        CmpOp::Gte => lval >= rval,
                    };
                    self.variables.insert(dest.clone(), result as i64); // Store bool as 0 or 1
                    1 // Cycle cost
                }
                Instr::BranchIf(cond, then_label, else_label) => {
                    let cond_val = self.get_var(cond);
                    let target_label = if cond_val != 0 {
                        then_label
                    } else {
                        else_label
                    };
                    next_pc = *self.labels.get(target_label).unwrap_or_else(|| {
                        panic!("Branch target label '{}' not found", target_label)
                    });
                    3 // Cycle cost (branch)
                }
                Instr::Jump(label) => {
                    next_pc = *self
                        .labels
                        .get(label)
                        .unwrap_or_else(|| panic!("Jump target label '{}' not found", label));
                    2 // Cycle cost (jump)
                }
                Instr::Label(_) => 0, // Labels themselves cost nothing to execute
                Instr::Phi(dest, sources) => {
                    // Phi nodes are complex in dynamic simulation without knowing the predecessor block.
                    // A simple VM often assumes Phi nodes are lowered/resolved beforehand.
                    // Here, we'll just assign a cost and maybe take the first source for simplicity,
                    // or panic if Phi nodes are unexpected at this stage.
                    // Let's assign a cost and copy the value from the first source as a placeholder.
                    if let Some((_label, src_var)) = sources.first() {
                        let val = self.get_var(src_var);
                        self.variables.insert(dest.clone(), val);
                    } else {
                        // Handle case with no sources if necessary, maybe default to 0?
                        // self.variables.insert(dest.clone(), 0);
                        println!("Warning: Phi node '{}' has no sources.", dest);
                    }
                    1 // Cycle cost (placeholder)
                }
                Instr::Assign(dest, src) => {
                    let val = self.get_var(src);
                    self.variables.insert(dest.clone(), val);
                    1 // Cycle cost
                }
            };

            self.total_cycles += cycles;
            self.pc = next_pc;
        }

        self.total_cycles
    }
}

/// Calculates the estimated cycle count by simulating the execution of instructions.
pub fn calculate_cycles(instrs: &[Instr]) -> u64 {
    let mut vm = Vm::new(instrs);
    vm.run()
}

#[cfg(test)]
mod tests {
    use super::*;
    // No longer need the crate::optimizer import here if Instr is imported above

    #[test]
    fn test_calculate_cycles_basic() {
        let instrs = vec![
            Instr::Label("entry".to_string()),  // pc=0, cycles=0
            Instr::Const("t1".to_string(), 10), // pc=1, cycles=1, t1=10
            Instr::Const("t2".to_string(), 20), // pc=2, cycles=1+1=2, t2=20
            Instr::BinOp(
                "t3".to_string(),
                "t1".to_string(),
                Op::Add,
                "t2".to_string(),
            ), // pc=3, cycles=2+1=3, t3=30
            Instr::Print("t3".to_string()),     // pc=4, cycles=3+10=13
            Instr::Assign("t4".to_string(), "t3".to_string()), // pc=5, cycles=13+1=14, t4=30
            Instr::Jump("exit".to_string()),    // pc=6, cycles=14+2=16, pc jumps to 7
            Instr::Label("exit".to_string()),   // pc=7, cycles=16+0=16
                                                // pc=8, loop terminates
        ];
        assert_eq!(calculate_cycles(&instrs), 16);
    }

    #[test]
    fn test_calculate_cycles_branch_taken() {
        let instrs = vec![
            Instr::Label("start".to_string()),   // pc=0, cycles=0
            Instr::Const("cond".to_string(), 1), // pc=1, cycles=1, cond=1
            Instr::Const("one".to_string(), 1),  // pc=2, cycles=1+1=2, one=1 (for comparison)
            Instr::Cmp(
                "cmp_res".to_string(),
                "cond".to_string(),
                CmpOp::Eq,
                "one".to_string(),
            ), // pc=3, cycles=2+1=3, cmp_res=1 (true)
            Instr::BranchIf(
                "cmp_res".to_string(),
                "then".to_string(),
                "else".to_string(),
            ), // pc=4, cycles=3+3=6, branches to "then" (pc=5)
            Instr::Label("then".to_string()),    // pc=5, cycles=6+0=6
            Instr::Print("cond".to_string()),    // pc=6, cycles=6+10=16
            Instr::Jump("end".to_string()),      // pc=7, cycles=16+2=18, jumps to "end" (pc=10)
            Instr::Label("else".to_string()),    // pc=8, skipped
            Instr::Const("zero".to_string(), 0), // pc=9, skipped
            Instr::Print("zero".to_string()),    // pc=10, skipped
            Instr::Label("end".to_string()),     // pc=10, cycles=18+0=18
                                                 // pc=11, loop terminates
        ];
        assert_eq!(calculate_cycles(&instrs), 18);
    }

    #[test]
    fn test_calculate_cycles_branch_not_taken() {
        let instrs = vec![
            Instr::Label("start".to_string()),   // pc=0, cycles=0
            Instr::Const("cond".to_string(), 0), // pc=1, cycles=1, cond=0
            Instr::Const("one".to_string(), 1),  // pc=2, cycles=1+1=2, one=1
            Instr::Cmp(
                "cmp_res".to_string(),
                "cond".to_string(),
                CmpOp::Eq,
                "one".to_string(),
            ), // pc=3, cycles=2+1=3, cmp_res=0 (false)
            Instr::BranchIf(
                "cmp_res".to_string(),
                "then".to_string(),
                "else".to_string(),
            ), // pc=4, cycles=3+3=6, branches to "else" (pc=8)
            Instr::Label("then".to_string()),    // pc=5, skipped
            Instr::Print("cond".to_string()),    // pc=6, skipped
            Instr::Jump("end".to_string()),      // pc=7, skipped
            Instr::Label("else".to_string()),    // pc=8, cycles=6+0=6
            Instr::Const("zero".to_string(), 0), // pc=9, cycles=6+1=7, zero=0
            Instr::Print("zero".to_string()),    // pc=10, cycles=7+10=17
            Instr::Label("end".to_string()),     // pc=11, cycles=17+0=17
                                                 // pc=12, loop terminates
        ];
        assert_eq!(calculate_cycles(&instrs), 17);
    }

    #[test]
    fn test_calculate_cycles_empty() {
        let instrs = vec![];
        assert_eq!(calculate_cycles(&instrs), 0);
    }

    #[test]
    #[should_panic(expected = "Variable 'x' used before definition")]
    fn test_panic_on_undefined_variable() {
        let instrs = vec![
            Instr::Label("start".to_string()),
            Instr::Print("x".to_string()),
        ];
        calculate_cycles(&instrs);
    }

    #[test]
    #[should_panic(expected = "Jump target label 'missing' not found")]
    fn test_panic_on_missing_jump_label() {
        let instrs = vec![
            Instr::Label("start".to_string()),
            Instr::Jump("missing".to_string()),
        ];
        calculate_cycles(&instrs);
    }

    #[test]
    #[should_panic(expected = "Branch target label 'missing_else' not found")]
    fn test_panic_on_missing_branch_label() {
        let instrs = vec![
            Instr::Label("start".to_string()),
            Instr::Const("cond".to_string(), 0),
            Instr::BranchIf(
                "cond".to_string(),
                "start".to_string(),
                "missing_else".to_string(),
            ),
        ];
        calculate_cycles(&instrs);
    }

    // Example with Phi (basic handling)
    #[test]
    fn test_calculate_cycles_with_phi() {
        let instrs = vec![
            Instr::Label("entry".to_string()),  // pc=0, cycles=0
            Instr::Const("a".to_string(), 5),   // pc=1, cycles=1, a=5
            Instr::Jump("block2".to_string()),  // pc=2, cycles=1+2=3, pc=4
            Instr::Label("block1".to_string()), // pc=3, skipped
            Instr::Const("b".to_string(), 10),  // pc=4, skipped
            Instr::Label("block2".to_string()), // pc=4, cycles=3+0=3
            // Simple Phi: takes first source 'a'. Assume came from entry->block2 implicitly.
            Instr::Phi(
                "c".to_string(),
                vec![("entry".to_string(), "a".to_string())],
            ), // pc=5, cycles=3+1=4, c=5 (from a)
            Instr::Print("c".to_string()), // pc=6, cycles=4+10=14
        ];
        assert_eq!(calculate_cycles(&instrs), 14);
    }
}
