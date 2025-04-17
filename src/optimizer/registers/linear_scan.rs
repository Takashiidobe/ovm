use std::collections::{HashMap, HashSet};

use crate::optimizer::Instr;

use super::{AVAILABLE_REGS, Location, RegisterAllocator};

pub struct LinearScan;

impl RegisterAllocator for LinearScan {
    fn allocate(&self, instrs: &[Instr]) -> (Vec<Instr>, HashMap<String, Location>) {
        // Map to track which variables are assigned to which registers
        let mut var_locations = HashMap::new();

        // Collect all variables
        let mut all_vars = HashSet::new();
        let mut root_vars = HashSet::new();

        for instr in instrs {
            match instr {
                Instr::Const(dest, _) => {
                    all_vars.insert(dest.clone());
                    root_vars.insert(dest.to_string());
                }
                Instr::BinOp(dest, left, _, right) => {
                    all_vars.insert(dest.clone());
                    all_vars.insert(left.clone());
                    all_vars.insert(right.clone());
                    root_vars.insert(dest.to_string());
                    root_vars.insert(left.to_string());
                    root_vars.insert(right.to_string());
                }
                Instr::Cmp(dest, left, _, right) => {
                    all_vars.insert(dest.clone());
                    all_vars.insert(left.clone());
                    all_vars.insert(right.clone());
                    root_vars.insert(dest.to_string());
                    root_vars.insert(left.to_string());
                    root_vars.insert(right.to_string());
                }
                Instr::BranchIf(cond, _, _) => {
                    all_vars.insert(cond.clone());
                    root_vars.insert(cond.to_string());
                }
                Instr::Print(var) => {
                    all_vars.insert(var.clone());
                    root_vars.insert(var.to_string());
                }
                Instr::Phi(dest, left, right) => {
                    all_vars.insert(dest.clone());
                    all_vars.insert(left.clone());
                    all_vars.insert(right.clone());
                    root_vars.insert(dest.to_string());
                    root_vars.insert(left.to_string());
                    root_vars.insert(right.to_string());
                }
                Instr::Assign(dest, src) => {
                    all_vars.insert(dest.clone());
                    all_vars.insert(src.clone());
                    root_vars.insert(dest.to_string());
                    root_vars.insert(src.to_string());
                }
                _ => {}
            }
        }

        // Sort root variables for deterministic allocation
        let mut sorted_roots: Vec<_> = root_vars.into_iter().collect();
        sorted_roots.sort();

        // Assign same register to all variables with same root name
        let mut reg_idx = 0;
        let mut register_map = HashMap::new();

        for root in sorted_roots {
            if !register_map.contains_key(&root) && reg_idx < AVAILABLE_REGS.len() {
                register_map.insert(root.clone(), AVAILABLE_REGS[reg_idx].to_string());
                reg_idx += 1;
            }
        }

        // Now assign locations to all variables based on their root name
        for var in all_vars {
            match register_map.get(&var) {
                Some(reg) => var_locations.insert(var, Location::Register(reg.clone())),
                None => var_locations.insert(var, Location::Spill),
            };
        }

        // for debugging, print register assignments
        for (root, reg) in &register_map {
            dbg!("{} -> {}", root, reg);
        }

        (instrs.to_vec(), var_locations)
    }
}
