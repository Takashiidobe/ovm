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
            Instr::Print(name) => {
                live.insert(name.clone());
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
        let liveness_sets = liveness(instrs);
        let mut loc_map: HashMap<String, Location> = HashMap::new();
        let mut reg_pool: Vec<String> = AVAILABLE_REGS.iter().map(|r| r.to_string()).collect();
        let mut active: HashMap<String, String> = HashMap::new(); // temp -> reg

        for (i, instr) in instrs.iter().enumerate() {
            // Allocate for new definitions
            let defined = match instr {
                Instr::Const(name, _) => Some(name),
                Instr::BinOp(dest, _, _, _) => Some(dest),
                Instr::Print(_) => None,
            };

            if let Some(var) = defined {
                if let Some(reg) = reg_pool.pop() {
                    loc_map.insert(var.clone(), Location::Register(reg.clone()));
                    active.insert(var.clone(), reg);
                } else {
                    loc_map.insert(var.clone(), Location::Spill);
                }
            }

            // Free registers of variables no longer live
            let live = &liveness_sets[i];
            let no_longer_live: Vec<_> = active
                .keys()
                .filter(|v| !live.contains(*v))
                .cloned()
                .collect();

            for var in no_longer_live {
                if let Some(reg) = active.remove(&var) {
                    reg_pool.push(reg);
                }
            }

            // Ensure operands are recorded
            match instr {
                Instr::BinOp(_, l, _, r) => {
                    for op in [l, r] {
                        loc_map.entry(op.clone()).or_insert(Location::Spill);
                    }
                }
                Instr::Print(v) => {
                    loc_map.entry(v.clone()).or_insert(Location::Spill);
                }
                Instr::Const(_, _) => {}
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

    let codegen = Codegen;
    let asm = codegen.generate_assembly(&instrs, &locs);

    assert_eq!(
        asm,
        String::from(
            ".section .data\n  fmt: .string \"%ld\\n\"\n.section .bss\n.section .text\n.globl main\nmain:\n  movq $1, %r15\n  movq $2, %r15\n  movq %r15, %rax\n  addq %r15, %rax\n  movq %rax, %r15\n  movq $3, %r15\n  movq %r15, %rax\n  imulq %r15, %rax\n  movq %rax, %r15\n  movq %r15, %rsi\n  leaq fmt(%rip), %rdi\n  xor %rax, %rax\n  call printf\n  movl $0, %eax\n  ret"
        )
    );
}
