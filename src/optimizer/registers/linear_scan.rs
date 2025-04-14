use std::collections::{HashMap, HashSet};

use crate::optimizer::Instr;

use super::{AVAILABLE_REGS, Location, RegisterAllocator};

pub fn liveness(instrs: &[Instr]) -> Vec<HashSet<String>> {
    let mut live_sets = vec![HashSet::new(); instrs.len() + 1]; // +1 for final empty set

    for i in (0..instrs.len()).rev() {
        let mut live = live_sets[i + 1].clone();

        match &instrs[i] {
            Instr::Const(name, _) => {
                live.remove(name);
            }
            Instr::BinOp(dest, left, _, right) => {
                live.remove(dest);
                live.insert(left.clone());
                live.insert(right.clone());
            }
            Instr::Cmp(_, left, _, right) => {
                // Comparison only uses operands, no defined result
                live.insert(left.clone());
                live.insert(right.clone());
            }
            Instr::BranchIf(cond, _, _) => {
                live.insert(cond.clone());
            }
            Instr::Print(name) => {
                live.insert(name.clone());
            }
            Instr::Phi(dest, left, right) => {
                // NOTE: This is tricky — discussed below
                live.remove(dest);
                live.insert(left.clone());
                live.insert(right.clone());
            }
            Instr::Jump(_) | Instr::Label(_) => {
                // control flow only — nothing live changes directly
            }
            Instr::Assign(dest, src) => {
                live.remove(dest);
                live.insert(src.clone());
            }
        }

        live_sets[i] = live;
    }

    live_sets.truncate(instrs.len()); // discard final dummy
    live_sets
}

pub struct LinearScan;

impl RegisterAllocator for LinearScan {
    fn allocate(&self, instrs: &[Instr]) -> (Vec<Instr>, HashMap<String, Location>) {
        use std::collections::HashMap;

        let liveness_sets = liveness(instrs);

        // 1. Build live intervals: Map<String, (start, end)>
        let mut intervals: HashMap<String, (usize, usize)> = HashMap::new();

        for (i, live) in liveness_sets.iter().enumerate() {
            for var in live {
                let entry = intervals.entry(var.clone()).or_insert((i, i));
                entry.1 = i; // update end
            }

            // Also update starts for defs
            match &instrs[i] {
                Instr::Const(name, _) | Instr::BinOp(name, _, _, _) | Instr::Phi(name, _, _) => {
                    let entry = intervals.entry(name.clone()).or_insert((i, i));
                    entry.0 = i; // ensure earliest def
                }
                _ => {}
            }
        }

        // 2. Convert to list and sort by start
        let mut sorted_intervals: Vec<(String, usize, usize)> = intervals
            .into_iter()
            .map(|(var, (start, end))| (var, start, end))
            .collect();

        sorted_intervals.sort_by_key(|(_, start, _)| *start);

        // 3. Perform linear scan
        let mut active: Vec<(String, usize)> = vec![]; // (var, end)
        let mut reg_pool: Vec<String> = AVAILABLE_REGS.iter().map(|r| r.to_string()).collect();
        let mut loc_map: HashMap<String, Location> = HashMap::new();

        for (var, start, end) in sorted_intervals {
            // Expire old variables
            active.retain(|(v, v_end)| {
                if *v_end >= start {
                    true
                } else {
                    // free their register
                    if let Some(Location::Register(reg)) = loc_map.get(v) {
                        reg_pool.push(reg.clone());
                    }
                    false
                }
            });

            // Allocate register or spill
            if let Some(reg) = reg_pool.pop() {
                loc_map.insert(var.clone(), Location::Register(reg.clone()));
                active.push((var, end));
            } else {
                loc_map.insert(var.clone(), Location::Spill);
            }
        }

        (instrs.to_vec(), loc_map)
    }
}

#[test]
fn test_register_allocation_with_liveness() {
    use crate::backend::{Backend as _, x86_64::Codegen};
    use crate::optimizer::Op;

    let instrs = vec![
        Instr::Const("t0".to_string(), 1),
        Instr::Const("t1".to_string(), 2),
        Instr::BinOp(
            "t2".to_string(),
            "t0".to_string(),
            Op::Add,
            "t1".to_string(),
        ),
        Instr::Const("t3".to_string(), 3),
        Instr::BinOp(
            "t4".to_string(),
            "t2".to_string(),
            Op::Mul,
            "t3".to_string(),
        ),
        Instr::Print("t4".to_string()),
    ];

    let reg_promoter = LinearScan;
    let (instrs, locs) = reg_promoter.allocate(&instrs);

    let mut codegen = Codegen::default();
    let asm = codegen.generate_assembly(&instrs, &locs);

    assert_eq!(
        asm,
        String::from(
            ".section .data\n  fmt: .string \"%ld\\n\"\n.section .bss\n.section .text\n.globl main\nmain:\n  movq $1, %r15\n  movq $2, %r14\n  movq %r15, %rax\n  addq %r14, %rax\n  movq %rax, %r13\n  movq $3, %r14\n  movq %r13, %rax\n  imulq %r14, %rax\n  movq %rax, %r15\n  movq %r15, %rsi\n  leaq fmt(%rip), %rdi\n  xor %rax, %rax\n  call printf\n  movl $0, %eax\n  ret"
        )
    );
}
