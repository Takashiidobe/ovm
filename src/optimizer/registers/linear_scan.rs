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
            // --- Additions for functions ---
            Instr::Call { target: _, args, result } => {
                // The result register (if any) is defined by the call.
                if let Some(res_var) = result {
                    defs.insert(res_var.clone());
                }
                // The arguments are used by the call.
                for arg_var in args {
                    uses.insert(arg_var.clone());
                }
            }
            Instr::Ret { value } => {
                // The return value (if any) is used by the ret instruction.
                if let Some(ret_var) = value {
                    uses.insert(ret_var.clone());
                }
            }
            // --- End Additions ---
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
                Instr::Jump(_) | Instr::BranchIf(_, _, _) | Instr::Ret { .. } => { // Ret also ends a block
                    // Instruction *after* a jump/branch/ret is a leader
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
            // Also consider Call targets if they are labels within the same code unit
            if let Instr::Call { target, .. } = instr {
                 if let Some(target_idx) = label_map.get(target) {
                     // Assuming calls can target labels defined within this IR sequence
                     leaders.insert(*target_idx);
                 }
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
                // Call targets added in previous loop
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
                 // Ensure synthetic labels are also in the map
                 label_map.entry(label.clone()).or_insert(start);
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
             let block_opt = blocks.get(label); // Get immutable reference first
             if block_opt.is_none() {
                 eprintln!("Warning: Block '{}' not found during CFG construction.", label);
                 continue;
             }
             let block = block_opt.unwrap();


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
                Instr::Ret { .. } => {
                    // Return instruction has no successors within this function's CFG
                }
                // If the last instruction is not a jump/branch/ret,
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
                                             if blocks.contains_key(actual_next_label) { // Check if block exists
                                                 successors.push(actual_next_label.clone());
                                                 eprintln!(
                                                     "  -> Found next leader block: {}",
                                                     actual_next_label
                                                 );
                                             }
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
            // Need to re-borrow mutably here
            if let Some(current_block_mut) = blocks.get_mut(label) {
                 current_block_mut.succs = successors.clone(); // Clone successors list here
            } else {
                 eprintln!("Warning: Block '{}' disappeared during CFG update (successors).", label);
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
                 // Check if block still exists before proceeding
                 if !blocks.contains_key(&label) { continue; }

                 // Clone necessary info before mutable borrow
                 let block_succs = blocks.get(&label).unwrap().succs.clone();
                 let block_defs = blocks.get(&label).unwrap().defs.clone();
                 let block_uses = blocks.get(&label).unwrap().uses.clone();
                 let block_live_in_orig = blocks.get(&label).unwrap().live_in.clone();
                 let block_live_out_orig = blocks.get(&label).unwrap().live_out.clone();


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
                 // Re-borrow mutably
                 if let Some(block_mut) = blocks.get_mut(&label) {
                     if new_live_in != block_live_in_orig || new_live_out != block_live_out_orig {
                         changed = true;
                         block_mut.live_in = new_live_in;
                         block_mut.live_out = new_live_out;
                     }
                 } else {
                      eprintln!("Warning: Block '{}' disappeared during liveness update.", label);
                 }
            }
        }

        // --- 4. Compute Live Intervals ---
        let mut intervals = HashMap::new(); // var -> LiveInterval

        // Iterate instructions *backwards* for more accurate interval computation
        for idx in (0..instrs.len()).rev() {
            let instr = &instrs[idx];
            let block_label_opt = instr_block_labels.get(&idx);

            // Determine live variables *after* this instruction executes.
            // live_out[i] = U_{s successor of i} live_in[s]
            let mut live_after_instr = HashSet::new();
            if let Some(block_label) = block_label_opt {
                if let Some(block) = blocks.get(block_label) {
                    if idx == block.range.end - 1 {
                        // If last instruction of block, live_out is from block's live_out set
                        live_after_instr = block.live_out.clone();
                    } else {
                         // Otherwise, live_out[i] = live_in[i+1]
                         // live_in[i+1] = uses[i+1] U (live_out[i+1] - defs[i+1])
                         // We need liveness *before* the next instruction (i+1) executes.
                         let next_instr = &instrs[idx + 1];
                         let (defs_next, uses_next) = Self::get_instr_defs_uses(next_instr);

                         // Start with live vars *after* the next instruction (i+1)
                         // This requires careful calculation, potentially iterating within the block backward.
                         // Let's approximate using block live_out for now, as before,
                         // but acknowledge this is less precise for intervals fully within a block.
                         // A simpler way: live_in[i] = uses[i] U (live_out[i] - defs[i])
                         // where live_out[i] is calculated based on successors.
                         // Let's stick to the block-level approximation for simplicity here.
                         // We need the set of live variables *just before* instr 'idx' executes.
                         // live_in[idx] = uses[idx] U (live_after[idx] - defs[idx])

                         // Calculate live *after* instruction 'idx' more directly:
                         // If 'idx' is not the last instruction: live_after[idx] = live_in[idx+1]
                         // We need live_in for idx+1.
                         // Let's compute live_in for each instruction point backward within the block.

                         // Simplified approach (less accurate for intra-block):
                         // Assume block.live_out is a good proxy for live_after_instr unless it's the last instr.
                         // This needs refinement. Let's try calculating backward from block end.

                         let mut live = block.live_out.clone(); // Start with live out of the block
                         for i in (block.range.start..block.range.end).rev() {
                             let current_instr = &instrs[i];
                             let (defs_curr, uses_curr) = Self::get_instr_defs_uses(current_instr);

                             // Update live set before this instruction
                             // live_in[i] = uses[i] U (live_out[i] - defs[i]) where live_out[i] is 'live'
                             let mut live_before = live.clone();
                             for def in &defs_curr { live_before.remove(def); }
                             live_before.extend(uses_curr);

                             // If 'i' is the current instruction index 'idx', this is live_in[idx].
                             // We need live_out[idx], which is 'live'.
                             if i == idx {
                                 live_after_instr = live.clone();
                                 break; // Found the live set for the current instruction
                             }

                             // Update 'live' for the next iteration (previous instruction)
                             live = live_before;
                         }
                    }
                }
            }


            // --- Update Interval Ranges based on Live Variables ---
            // Extend interval for any variable live *after* this instruction
            for live_var in &live_after_instr {
                let interval = intervals
                    .entry(live_var.clone())
                    .or_insert_with(|| LiveInterval {
                        var: live_var.clone(),
                        range: idx..(idx + 1), // Initialize range at this point if new
                        location: None,
                    });
                // The interval must start *at or before* this instruction
                interval.range.start = interval.range.start.min(idx);
                // End remains as previously set (will be maxed later)
            }

            // Handle definition: ensure interval starts here or earlier
            let (defs, _) = Self::get_instr_defs_uses(instr);
            for def_var in defs {
                let interval = intervals
                    .entry(def_var.clone())
                    .or_insert_with(|| LiveInterval {
                        var: def_var.clone(),
                        range: idx..(idx + 1), // Definition starts interval
                        location: None,
                    });
                // A definition potentially shortens the required live range start
                // but the range must cover the definition itself.
                interval.range.start = interval.range.start.min(idx);
                // Ensure interval exists at least at the definition point
                interval.range.end = interval.range.end.max(idx + 1);
            }

            // Handle uses: ensure interval covers the use point
            let (_, uses) = Self::get_instr_defs_uses(instr);
            for use_var in uses {
                let interval = intervals
                    .entry(use_var.clone())
                    .or_insert_with(|| LiveInterval {
                        var: use_var.clone(),
                        range: idx..(idx + 1), // Use ensures liveness at this point
                        location: None,
                    });
                 // The interval must start *at or before* this instruction
                 interval.range.start = interval.range.start.min(idx);
                 // Ensure interval ends *at or after* this instruction + 1
                 interval.range.end = interval.range.end.max(idx + 1);
            }

             // Phi nodes definitions conceptually happen at the start of the block.
             // Their uses (sources) must be live at the end of predecessor blocks.
             // This backward pass handles the uses naturally.
             // The definition needs careful start point handling.
            if let Instr::Phi(dest, _) = instr {
                 if let Some(block_label) = block_label_opt {
                     if let Some(block) = blocks.get(block_label) {
                          let block_start_idx = block.range.start;
                          let interval = intervals.entry(dest.clone()).or_insert_with(|| LiveInterval {
                                var: dest.clone(),
                                range: block_start_idx..block_start_idx + 1,
                                location: None,
                            });
                          interval.range.start = interval.range.start.min(block_start_idx);
                          interval.range.end = interval.range.end.max(idx + 1); // Ensure covers phi instr itself
                     }
                 }
            }

        }


        // Convert intervals map to a Vec and sort by start point
        let mut sorted_intervals: Vec<_> = intervals
            .values()
            .filter(|&i| !i.range.is_empty()) // Filter out empty ranges
            .cloned()
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
                    // Don't add current to active list
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
                    eprintln!("Assigning Register to {}: {:?}
", current.var, location);
                    // Add current to active list
                    active.push(current); // current is moved here
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
                eprintln!("Assigning Register to {}: {:?}
", current.var, location);
                // Add current to active list
                active.push(current); // current is moved here
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
