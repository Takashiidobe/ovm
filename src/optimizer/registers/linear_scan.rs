use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::ops::Range;

use crate::optimizer::Instr;

use super::{AVAILABLE_REGS, Location, RegisterAllocator};

#[derive(Debug, Default, Clone, PartialEq)]
struct LiveInterval {
    var: String,
    range: Range<usize>, // Instruction indices [start, end)
    location: Option<Location>,
}

#[derive(Debug, Default, Clone, PartialEq)]
struct BasicBlock {
    range: Range<usize>, // Instruction indices [start, end)
    instrs: Vec<Instr>,  // Optional: might not need to store them again
    preds: Vec<String>,
    succs: Vec<String>,
    defs: HashSet<String>,
    uses: HashSet<String>, // Variables used *before* being defined in this block
    live_in: HashSet<String>,
    live_out: HashSet<String>,
}

pub struct LinearScan;

impl LinearScan {
    // Helper function to get defined and used variables for an instruction
    fn get_instr_defs_uses(instr: &Instr) -> (HashSet<String>, HashSet<String>) {
        let mut defs = HashSet::new();
        let mut uses = HashSet::new();

        match instr {
            Instr::Const(dest, _) => {
                defs.insert(dest.clone());
            }
            Instr::BinOp(dest, left, _, right) => {
                defs.insert(dest.clone());
                uses.insert(left.clone());
                uses.insert(right.clone());
            }
            Instr::Print(var) => {
                uses.insert(var.clone());
            }
            Instr::Cmp(dest, left, _, right) => {
                defs.insert(dest.clone());
                uses.insert(left.clone());
                uses.insert(right.clone());
            }
            Instr::BranchIf(cond, _, _) => {
                uses.insert(cond.clone());
            }
            Instr::Jump(_) => {}
            Instr::Label(_) => {}
            Instr::Phi(dest, preds) => {
                // Definition happens conceptually *at the start* of the block containing the Phi.
                defs.insert(dest.clone());
                // Uses happen conceptually *at the end* of the predecessor blocks.
                for (_, val) in preds {
                    uses.insert(val.clone());
                }
            }
            Instr::Assign(dest, src) => {
                defs.insert(dest.clone());
                uses.insert(src.clone());
            }
        }
        (defs, uses)
    }
}

impl RegisterAllocator for LinearScan {
    fn allocate(&self, instrs: &[Instr]) -> (Vec<Instr>, HashMap<String, Location>) {
        if instrs.is_empty() {
            return (vec![], HashMap::new());
        }

        // --- 1. Identify Basic Blocks ---
        let mut blocks = BTreeMap::new(); // label -> BasicBlock
        let mut leaders = BTreeSet::new(); // Set of instruction indices that start a block
        leaders.insert(0); // First instruction is always a leader

        for (idx, instr) in instrs.iter().enumerate() {
            match instr {
                Instr::Jump(_) | Instr::BranchIf(_, _, _) => {
                    // Instruction *after* a jump/branch is a leader
                    if idx + 1 < instrs.len() {
                        leaders.insert(idx + 1);
                    }
                    // Target of a jump/branch is a leader (needs label mapping)
                    // We'll handle this using label_map later
                }
                Instr::Label(_) => {
                    leaders.insert(idx); // Label instruction starts a block
                }
                _ => {}
            }
        }

        // Create a map from label name to the instruction index where it's defined
        let mut label_map = HashMap::new();
        for (idx, instr) in instrs.iter().enumerate() {
            if let Instr::Label(label) = instr {
                label_map.insert(label.clone(), idx);
                // Ensure targets of jumps/branches are leaders
                // Need to find jumps/branches *pointing* to this label
            }
        }
        // Second pass to add actual jump targets to leaders
        for instr in instrs.iter() {
            match instr {
                Instr::Jump(target) | Instr::BranchIf(_, target, _) => {
                    if let Some(target_idx) = label_map.get(target) {
                        leaders.insert(*target_idx);
                    } else {
                        eprintln!("Warning: Jump/Branch target label '{}' not found.", target);
                    }
                }
                _ => {}
            }
        }

        // Create blocks based on leaders
        let mut sorted_leaders: Vec<_> = leaders.into_iter().collect();
        sorted_leaders.sort();
        let mut instr_block_labels = HashMap::new(); // Map instruction index to its block label

        for i in 0..sorted_leaders.len() {
            let start = sorted_leaders[i];
            let end = if i + 1 < sorted_leaders.len() {
                sorted_leaders[i + 1]
            } else {
                instrs.len()
            };

            let range = start..end;
            let label = match &instrs[start] {
                Instr::Label(l) => l.clone(),
                _ => format!("block_{}", start), // Generate synthetic label if needed
            };

            if label_map.get(&label) != Some(&start) && matches!(instrs[start], Instr::Label(_)) {
                // Add synthetic label if the first instruction wasn't the defining label
                // but a label exists
                label_map.entry(label.clone()).or_insert(start);
            } else if !matches!(instrs[start], Instr::Label(_)) {
                label_map.insert(label.clone(), start);
            }

            // Populate instr_block_labels mapping
            for idx in range.clone() {
                instr_block_labels.insert(idx, label.clone());
            }

            let block_instrs = instrs[range.clone()].to_vec(); // Store instructions if needed later

            blocks.insert(
                label.clone(),
                BasicBlock {
                    range: range.clone(),
                    instrs: block_instrs,
                    ..Default::default()
                },
            );
        }

        // --- 2. Build CFG (Predecessors/Successors) ---
        let block_labels_list: Vec<_> = blocks.keys().cloned().collect();
        for label in &block_labels_list {
            let block = blocks.get(label).unwrap(); // Clone block temporarily to satisfy borrow checker

            let last_instr_idx = if block.range.is_empty() {
                continue;
            } else {
                block.range.end - 1
            };
            if last_instr_idx >= instrs.len() {
                continue;
            }; // Handle empty blocks or end of program

            let last_instr = &instrs[last_instr_idx];

            let mut successors = Vec::new();
            match last_instr {
                Instr::Jump(target) => {
                    successors.push(target.clone());
                }
                Instr::BranchIf(_, then_target, else_target) => {
                    successors.push(then_target.clone());
                    successors.push(else_target.clone());
                }
                // If the last instruction is not a jump/branch,
                // the successor is the next block in sequence (if any)
                _ => {
                    // Find the block that starts immediately after this one
                    let next_block_start = block.range.end;
                    if next_block_start < instrs.len() {
                        // Find the label corresponding to this start index using instr_block_labels
                        if let Some(next_label) = instr_block_labels.get(&next_block_start) {
                            // Check if this label corresponds to a block starting at this index
                            if blocks
                                .get(next_label)
                                .is_some_and(|b| b.range.start == next_block_start)
                            {
                                successors.push(next_label.clone());
                            } else {
                                // This case might occur if labels aren't perfectly aligned with leader indices
                                eprintln!(
                                    "Warning: Fallthrough label mismatch for block ending at {}",
                                    last_instr_idx
                                );
                                // Attempt to find the next block by iterating sorted leaders
                                if let Some(leader_idx) =
                                    sorted_leaders.iter().position(|&l| l == block.range.start)
                                {
                                    if leader_idx + 1 < sorted_leaders.len() {
                                        let next_leader_start = sorted_leaders[leader_idx + 1];
                                        if let Some(actual_next_label) =
                                            instr_block_labels.get(&next_leader_start)
                                        {
                                            successors.push(actual_next_label.clone());
                                            eprintln!(
                                                "  -> Found next leader block: {}",
                                                actual_next_label
                                            );
                                        }
                                    }
                                }
                            }
                        } else {
                            eprintln!(
                                "Warning: Could not find label for fallthrough block starting at index {}",
                                next_block_start
                            );
                        }
                    }
                }
            }

            // Update successors for the current block and predecessors for the target blocks
            if let Some(current_block_mut) = blocks.get_mut(label) {
                current_block_mut.succs = successors.clone();
            } else {
                eprintln!("Warning: Block '{}' disappeared during CFG update.", label);
                continue;
            }

            for succ_label in successors {
                if let Some(succ_block) = blocks.get_mut(&succ_label) {
                    succ_block.preds.push(label.clone());
                } else {
                    eprintln!(
                        "Warning: Successor label '{}' not found in blocks map for edge from '{}'.",
                        succ_label, label
                    );
                }
            }
        }

        // --- 3. Liveness Analysis ---
        // Compute defs and uses for each block first
        let mut all_vars = HashSet::new(); // Collect all unique variable names
        for (_, block) in blocks.iter_mut() {
            let mut defined_in_block = HashSet::new();
            for instr in &block.instrs {
                let (defs, uses) = Self::get_instr_defs_uses(instr);
                for def in defs {
                    all_vars.insert(def.clone());
                    block.defs.insert(def.clone());
                    defined_in_block.insert(def);
                }
                for used in uses {
                    all_vars.insert(used.clone());
                    // Only add to block.uses if not defined *before* this use within the same block
                    if !defined_in_block.contains(&used) {
                        block.uses.insert(used);
                    }
                }
            }
        }

        // Iteratively compute live_in and live_out until stabilization
        let mut changed = true;
        while changed {
            changed = false;
            // Iterate backwards for faster convergence (optional but common)
            let block_labels_rev: Vec<_> = blocks.keys().cloned().rev().collect();
            for label in block_labels_rev {
                // Clone necessary info before mutable borrow
                let block_succs = blocks.get(label.as_str()).unwrap().succs.clone();
                let block_defs = blocks.get(label.as_str()).unwrap().defs.clone();
                let block_uses = blocks.get(label.as_str()).unwrap().uses.clone();
                let block_live_in_orig = blocks.get(label.as_str()).unwrap().live_in.clone();
                let block_live_out_orig = blocks.get(label.as_str()).unwrap().live_out.clone();

                // Calculate new live_out: union of live_in of successors
                let mut new_live_out = HashSet::new();
                for succ_label in &block_succs {
                    if let Some(succ_block) = blocks.get(succ_label) {
                        new_live_out.extend(succ_block.live_in.iter().cloned());
                    }
                }

                // Calculate new live_in: uses U (live_out - defs)
                let mut live_out_minus_defs = new_live_out.clone();
                for def in &block_defs {
                    live_out_minus_defs.remove(def);
                }
                let mut new_live_in = block_uses.clone();
                new_live_in.extend(live_out_minus_defs);

                // Check if sets changed
                let block_mut = blocks.get_mut(label.as_str()).unwrap();
                if new_live_in != block_live_in_orig || new_live_out != block_live_out_orig {
                    changed = true;
                    block_mut.live_in = new_live_in;
                    block_mut.live_out = new_live_out;
                }
            }
        }

        // --- 4. Compute Live Intervals ---
        let mut intervals = HashMap::new(); // var -> LiveInterval

        for (idx, instr) in instrs.iter().enumerate() {
            let block_label_opt = instr_block_labels.get(&idx);

            // Determine live variables at this specific point (after the instruction executes)
            let mut live_after_instr = HashSet::new();
            if let Some(block_label) = block_label_opt {
                if let Some(block) = blocks.get(block_label) {
                    // If last instruction of block, use live_out
                    if idx == block.range.end - 1 {
                        live_after_instr = block.live_out.clone();
                    } else {
                        // Otherwise, calculate live_in for the *next* instruction's point
                        // live_in[i+1] = uses[i+1] U (live_out[i+1] - defs[i+1])
                        // Simplified approach: Assume live_out of the block persists unless redefined
                        // A more precise way requires instruction-level liveness, but let's approximate:
                        live_after_instr = block.live_out.clone();
                        // Add uses from current instr, remove defs from current instr
                        let (defs_curr, uses_curr) = Self::get_instr_defs_uses(instr);
                        live_after_instr.extend(uses_curr);
                        for def in defs_curr {
                            live_after_instr.remove(&def);
                        }
                    }
                }
            }

            // Extend intervals for variables live *after* this instruction
            for live_var in &live_after_instr {
                let interval = intervals
                    .entry(live_var.clone())
                    .or_insert_with(|| LiveInterval {
                        var: live_var.clone(),
                        range: idx..idx, // Initialize with empty range
                        location: None,
                    });
                interval.range.end = interval.range.end.max(idx + 1);
                // Set start point if not set
                if interval.range.is_empty() {
                    interval.range.start = idx;
                }
            }

            // Handle definition: ensure interval starts here
            let (defs, _) = Self::get_instr_defs_uses(instr);
            for def_var in defs {
                let interval = intervals
                    .entry(def_var.clone())
                    .or_insert_with(|| LiveInterval {
                        var: def_var.clone(),
                        range: idx..idx + 1, // Define starts interval [def_idx, def_idx+1)
                        location: None,
                    });
                interval.range.start = interval.range.start.min(idx); // Set start point
                interval.range.end = interval.range.end.max(idx + 1); // Ensure ends at least past def
            }

            // Handle uses: ensure interval extends to cover use
            let (_, uses) = Self::get_instr_defs_uses(instr);
            for use_var in uses {
                let interval = intervals
                    .entry(use_var.clone())
                    .or_insert_with(|| LiveInterval {
                        var: use_var.clone(),
                        range: idx..idx,
                        location: None,
                    });
                // If this is the first time seeing var, set start
                if interval.range.is_empty() {
                    interval.range.start = idx;
                }
                interval.range.end = interval.range.end.max(idx + 1); // Extend past use point
            }

            // Special handling for Phi nodes: sources must be live up until the end of their respective predecessor blocks.
            if let Instr::Phi(dest, preds) = instr {
                if let Some(block_label) = block_label_opt {
                    if let Some(phi_block) = blocks.get(block_label) {
                        // Ensure destination interval covers at least the phi itself
                        let phi_start_idx = phi_block.range.start;
                        let dest_interval =
                            intervals
                                .entry(dest.clone())
                                .or_insert_with(|| LiveInterval {
                                    var: dest.clone(),
                                    range: phi_start_idx..phi_start_idx + 1,
                                    location: None,
                                });
                        dest_interval.range.start = dest_interval.range.start.min(phi_start_idx);
                        dest_interval.range.end = dest_interval.range.end.max(phi_start_idx + 1);

                        for (pred_label, pred_val) in preds {
                            if let Some(pred_block) = blocks.get(pred_label) {
                                let pred_end_idx = pred_block.range.end;
                                let pred_interval = intervals
                                    .entry(pred_val.clone())
                                    .or_insert_with(|| LiveInterval {
                                        var: pred_val.clone(),
                                        // Initialize range potentially empty or just at pred_end
                                        range: pred_end_idx..pred_end_idx,
                                        location: None,
                                    });
                                // Ensure the predecessor value's interval extends to the end of the predecessor block
                                pred_interval.range.end = pred_interval.range.end.max(pred_end_idx);
                                // If interval was empty, set start point based on earliest known liveness or def
                                // This part is tricky without full interval tracking before this loop
                            }
                        }
                    }
                }
            }
        }

        // Convert intervals map to a Vec and sort by start point
        let mut sorted_intervals: Vec<_> = intervals
            .values()
            .filter(|&i| !i.range.is_empty())
            .cloned() // Filter out potentially empty ranges
            .collect();
        sorted_intervals.sort_by_key(|i| i.range.start);

        // --- 5. Linear Scan Allocation ---
        let mut active: Vec<LiveInterval> = Vec::new();
        let mut free_regs: VecDeque<String> =
            AVAILABLE_REGS.iter().map(|s| s.to_string()).collect();
        let mut final_locations = HashMap::new();

        for current_interval_ref in sorted_intervals {
            let mut current = current_interval_ref.clone(); // Clone to modify location later if needed

            // Expire old intervals in active
            let mut expire_indices = Vec::new();
            for (i, active_interval) in active.iter().enumerate() {
                if active_interval.range.end <= current.range.start {
                    expire_indices.push(i);
                }
            }
            // Remove expired intervals and free their registers (process in reverse index order)
            expire_indices.sort_by(|a, b| b.cmp(a)); // Sort descending
            for i in expire_indices {
                let expired = active.remove(i);
                // Match on a reference to avoid moving the String
                if let Some(Location::Register(reg_name)) = &expired.location {
                    free_regs.push_back(reg_name.clone()); // Clone the reg name to push back
                    eprintln!(
                        "Expiring: {}, Register {:?} freed",
                        expired.var, expired.location
                    );
                } else {
                    eprintln!("Expiring spilled var: {}", expired.var);
                }
            }

            let location: Location;
            if active.len() == AVAILABLE_REGS.len() {
                // Spill: No free registers
                // Find interval in `active` that ends latest
                active.sort_by_key(|i| i.range.end); // Ensure active is sorted by end point
                let spill_candidate = &active[active.len() - 1]; // Last element ends latest

                // Spill the one ending later: current or the candidate from active
                if current.range.end > spill_candidate.range.end {
                    // Spill current interval
                    location = Location::Spill;
                    final_locations.insert(current.var.clone(), location.clone());
                    eprintln!(
                        "Spilling CURRENT: {} (ends {})",
                        current.var, current.range.end
                    );
                } else {
                    // Spill the active interval candidate
                    let spilled_interval = active.pop().unwrap(); // Remove the last one (ends latest)
                    let reg = match &spilled_interval.location {
                        // Match on reference
                        Some(Location::Register(r)) => r.clone(), // Clone the name
                        _ => panic!(
                            "Interval to spill had no register! Var: {}",
                            spilled_interval.var
                        ),
                    };
                    eprintln!(
                        "Spilling ACTIVE: {} (ends {}) for {} (ends {})",
                        spilled_interval.var,
                        spilled_interval.range.end,
                        current.var,
                        current.range.end
                    );
                    final_locations.insert(spilled_interval.var.clone(), Location::Spill);

                    // Assign the freed register to current
                    location = Location::Register(reg.clone());
                    final_locations.insert(current.var.clone(), location.clone());
                    current.location = Some(location.clone()); // Update current's state
                    // Log before moving current
                    eprintln!("Assigning Register to {}: {:?}", current.var, location);
                    // Add current to active list
                    active.push(current);
                    active.sort_by_key(|i| i.range.end); // Keep active sorted by end point
                }
            } else {
                // Allocate register
                let reg = free_regs
                    .pop_front()
                    .expect("Free regs available but list empty!");
                location = Location::Register(reg);
                final_locations.insert(current.var.clone(), location.clone());
                current.location = Some(location.clone()); // Update current's state
                // Log before moving current
                eprintln!("Assigning Register to {}: {:?}", current.var, location);
                // Add current to active list
                active.push(current);
                active.sort_by_key(|i| i.range.end); // Keep active sorted by end point
            }
        }

        // Ensure all variables from the original code have a location assigned.
        // Some might not have intervals if they were defined but never used/live out?
        // Default to spill for safety.
        for instr in instrs {
            let (defs, uses) = Self::get_instr_defs_uses(instr);
            for var in defs.iter().chain(uses.iter()) {
                final_locations.entry(var.clone()).or_insert_with(|| {
                     eprintln!("Warning: Variable {} was not assigned a location by LSRA, defaulting to Spill.", var);
                     Location::Spill
                 });
            }
        }

        // Debug print final locations
        eprintln!("--- Final Variable Locations ---");
        let mut sorted_locations: Vec<_> = final_locations.iter().collect();
        sorted_locations.sort_by_key(|(k, _)| *k);
        for (var, loc) in sorted_locations {
            eprintln!("{}: {:?}", var, loc);
        }
        eprintln!("--------------------------------");

        // TODO: Implement instruction rewriting for spills if necessary.
        // For now, the backend partially handles spills via move_memory.

        (instrs.to_vec(), final_locations)
    }
}
