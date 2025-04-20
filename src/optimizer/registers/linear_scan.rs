use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::ops::Range;

// Make sure CFG is imported
use crate::optimizer::CFG;
use crate::optimizer::Instr;

use super::{AVAILABLE_REGS, Location, RegisterAllocator};

#[derive(Debug, Default, Clone, PartialEq)]
struct LiveInterval {
    var: String,
    // Range now refers to indices in the *linearized* instruction list
    range: Range<usize>,
    location: Option<Location>,
}

// BasicBlock struct might not be needed here anymore if we directly use CFG's blocks
// Keeping it for now if liveness results need temporary storage outside CFG
#[derive(Debug, Default, Clone, PartialEq)]
struct BlockLivenessInfo {
    defs: HashSet<String>,
    uses: HashSet<String>, // Variables used *before* being defined in this block
    live_in: HashSet<String>,
    live_out: HashSet<String>,
}

// Helper function to expire intervals and free registers (operates on linear indices)
fn expire_old_intervals(
    active: &mut Vec<LiveInterval>,
    free_regs: &mut VecDeque<String>,
    current_linear_start_point: usize, // Use linear index
) {
    let mut expire_indices = Vec::new();
    for (i, active_interval) in active.iter().enumerate() {
        // Use linear range end
        if active_interval.range.end <= current_linear_start_point {
            expire_indices.push(i);
        }
    }
    // Remove expired intervals and free their registers (process in reverse index order)
    expire_indices.sort_by(|a, b| b.cmp(a)); // Sort descending
    for i in expire_indices {
        let expired = active.remove(i);
        // Match on a reference to avoid moving the String
        if let Some(Location::Register(reg_name)) = &expired.location {
            // TODO: Add back proper handling for pre-colored registers if needed
            free_regs.push_back(reg_name.clone()); // Clone the reg name to push back
            eprintln!(
                "Expiring: {} ({:?}), Register {:?} freed",
                expired.var, expired.range, expired.location
            );
        } else {
            eprintln!(
                "Expiring spilled var: {} ({:?})",
                expired.var, expired.range
            );
        }
    }
}

pub struct LinearScan;

impl LinearScan {
    // Helper function to get defined and used variables for an instruction (no change needed)
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
            Instr::Label(_) => {} // Labels don't def/use temps directly
            Instr::Phi(dest, preds) => {
                defs.insert(dest.clone());
                for (_, val) in preds {
                    // Uses happen conceptually at the end of predecessor blocks,
                    // but the variable name is needed for liveness.
                    uses.insert(val.clone());
                }
            }
            Instr::Assign(dest, src) => {
                defs.insert(dest.clone());
                uses.insert(src.clone());
            }
            Instr::Call {
                target: _,
                args,
                result,
            } => {
                if let Some(res_var) = result {
                    defs.insert(res_var.clone());
                }
                for arg_var in args {
                    uses.insert(arg_var.clone());
                }
                // TODO: Consider registers clobbered by call convention as 'defs'
            }
            Instr::Ret { value } => {
                if let Some(ret_var) = value {
                    uses.insert(ret_var.clone());
                }
            }
            Instr::FuncParam { name, index: _ } => {
                defs.insert(name.clone()); // FuncParam defines the parameter variable
            }
        }
        (defs, uses)
    }
}

impl RegisterAllocator for LinearScan {
    // Updated signature
    fn allocate(&self, cfg: &CFG) -> (CFG, BTreeMap<String, Location>) {
        if cfg.blocks.is_empty() {
            return (cfg.clone(), BTreeMap::new());
        }

        // --- 1. Linearize Instructions & Build Mappings ---
        let mut linearized_instrs = Vec::new();
        // Map linear index -> (block_label, index_within_block)
        let mut linear_to_original_map: HashMap<usize, (String, usize)> = HashMap::new();
        // Map (block_label, index_within_block) -> linear index
        let mut original_to_linear_map: HashMap<(String, usize), usize> = HashMap::new();
        // Map block label -> linear index range [start, end)
        let mut block_linear_ranges: HashMap<String, Range<usize>> = HashMap::new();

        // Sort block labels for deterministic linearization order
        // Consider using Reverse Postorder (RPO) for better results, but simple sort works.
        let mut sorted_block_labels: Vec<_> = cfg.blocks.keys().cloned().collect();
        sorted_block_labels.sort(); // Simple alphabetical sort for now

        let mut current_linear_idx = 0;
        for label in &sorted_block_labels {
            if let Some(block) = cfg.blocks.get(label) {
                let block_start_linear_idx = current_linear_idx;
                for (idx_in_block, instr) in block.instrs.iter().enumerate() {
                    linear_to_original_map.insert(current_linear_idx, (label.clone(), idx_in_block));
                    original_to_linear_map.insert((label.clone(), idx_in_block), current_linear_idx);
                    linearized_instrs.push(instr.clone());
                    current_linear_idx += 1;
                }
                block_linear_ranges.insert(label.clone(), block_start_linear_idx..current_linear_idx);
            }
        }
        let total_linear_instrs = linearized_instrs.len();
        if total_linear_instrs == 0 {
             return (cfg.clone(), BTreeMap::new()); // Handle empty CFG case
        }


        // --- 2. Liveness Analysis (using CFG structure) ---
        // Store liveness results temporarily
        let mut liveness_info: HashMap<String, BlockLivenessInfo> = HashMap::new();
        let mut all_vars = HashSet::new(); // Collect all unique variable names

        // Compute defs and uses for each block first
        for (label, block) in &cfg.blocks {
            let mut info = BlockLivenessInfo::default();
            let mut defined_in_block = HashSet::new();
            for instr in &block.instrs {
                let (defs, uses) = Self::get_instr_defs_uses(instr);
                for def in defs {
                    all_vars.insert(def.clone());
                    info.defs.insert(def.clone());
                    defined_in_block.insert(def);
                }
                for used in uses {
                    all_vars.insert(used.clone());
                    // Only add to block.uses if not defined *before* this use within the same block
                    if !defined_in_block.contains(&used) {
                        info.uses.insert(used);
                    }
                }
            }
            liveness_info.insert(label.clone(), info);
        }

        // Iteratively compute live_in and live_out until stabilization
        let mut changed = true;
        while changed {
            changed = false;
            // Iterate backwards over sorted labels for potentially faster convergence
            for label in sorted_block_labels.iter().rev() {
                // Check if block exists before proceeding
                if !cfg.blocks.contains_key(label) { continue; }

                let block = cfg.blocks.get(label).unwrap(); // Known to exist
                let current_info = liveness_info.get(label).unwrap(); // Known to exist

                // Clone necessary info before mutable borrow
                let block_succs = &block.succs; // Use reference
                let block_defs = &current_info.defs; // Use reference
                let block_uses = &current_info.uses; // Use reference
                let block_live_in_orig = current_info.live_in.clone();
                let block_live_out_orig = current_info.live_out.clone();

                // Calculate new live_out: union of live_in of successors
                let mut new_live_out = HashSet::new();
                for succ_label in block_succs {
                    if let Some(succ_info) = liveness_info.get(succ_label) {
                        new_live_out.extend(succ_info.live_in.iter().cloned());
                    }
                }

                // Calculate new live_in: uses U (live_out - defs)
                let mut live_out_minus_defs = new_live_out.clone();
                for def in block_defs {
                    live_out_minus_defs.remove(def);
                }
                let mut new_live_in = block_uses.clone();
                new_live_in.extend(live_out_minus_defs);

                // Check if sets changed and update if necessary
                if new_live_in != block_live_in_orig || new_live_out != block_live_out_orig {
                    if let Some(info_mut) = liveness_info.get_mut(label) {
                        changed = true;
                        info_mut.live_in = new_live_in;
                        info_mut.live_out = new_live_out;
                    }
                }
            }
        }

        // --- 3. Compute Live Intervals (using linearized instructions) ---
        let mut intervals = HashMap::new(); // var -> LiveInterval

        // Iterate instructions *backwards* over the linearized list
        for linear_idx in (0..total_linear_instrs).rev() {
            let instr = &linearized_instrs[linear_idx];
            let (block_label, idx_in_block) = linear_to_original_map
                .get(&linear_idx)
                .expect("Linear index must map back to original location")
                .clone(); // Clone label needed

            let block_info = liveness_info
                .get(&block_label)
                .expect("Block must have liveness info");
            let block_range_linear = block_linear_ranges
                .get(&block_label)
                .expect("Block must have linear range");

            // Determine live variables *after* this instruction executes in the linear order.
            let mut live_after_instr: HashSet<String> = HashSet::new();

            if linear_idx == block_range_linear.end - 1 {
                // If last instruction of block, live_out is from block's live_out set
                live_after_instr = block_info.live_out.clone();
            } else {
                // Otherwise, live_out[i] = live_in[i+1] (in linear order)
                // Calculate live_in for the *next* instruction (linear_idx + 1)
                let next_instr = &linearized_instrs[linear_idx + 1];
                let (defs_next, uses_next) = Self::get_instr_defs_uses(next_instr);

                // Need live *after* next instruction.
                // This requires iterating backward *within* the block from its end
                // or using the already computed live_in[next]. Let's try the latter.

                // Find live_in for linear_idx + 1
                let mut live_in_next;
                let (next_block_label, _) = linear_to_original_map[&(linear_idx + 1)].clone();
                let next_block_info = &liveness_info[&next_block_label];
                let next_block_range_linear = &block_linear_ranges[&next_block_label];

                if linear_idx + 1 == next_block_range_linear.end - 1 {
                    // Next instruction is the last in its block
                    let live_out_next_block = next_block_info.live_out.clone();
                    let mut live_out_minus_defs = live_out_next_block;
                    for def in &defs_next { live_out_minus_defs.remove(def); }
                    live_in_next = uses_next;
                    live_in_next.extend(live_out_minus_defs);
                } else {
                    // Need live_in[linear_idx + 2]... this gets recursive.
                    // Let's use the backward pass within block approach for live_after_instr.

                    let mut live = block_info.live_out.clone(); // Start with live out of the block
                    let mut found_live_after = false;
                    for i_lin in (block_range_linear.start..block_range_linear.end).rev() {
                         let current_instr = &linearized_instrs[i_lin];
                         let (defs_curr, uses_curr) = Self::get_instr_defs_uses(current_instr);

                         // 'live' currently holds live_out[i_lin]
                         if i_lin == linear_idx {
                             live_after_instr = live.clone();
                             found_live_after = true;
                             break;
                         }

                         // Calculate live_in[i_lin] = uses[i_lin] U (live_out[i_lin] - defs[i_lin])
                         let mut live_before = live.clone();
                         for def in &defs_curr { live_before.remove(def); }
                         live_before.extend(uses_curr);

                         // Update 'live' for the next iteration (previous instruction)
                         live = live_before;
                    }
                    if !found_live_after {
                        // Should not happen if linear_idx is within the block range
                        panic!("Could not determine live_after_instr for linear index {}", linear_idx);
                    }
                }
            }


            // --- Update Interval Ranges based on Live Variables ---
            // Extend interval for any variable live *after* this instruction
            for live_var in live_after_instr {
                let interval = intervals
                    .entry(live_var.clone())
                    .or_insert_with(|| LiveInterval {
                        var: live_var.clone(),
                        range: linear_idx..(linear_idx + 1), // Initialize range
                        location: None,
                    });
                interval.range.start = interval.range.start.min(linear_idx);
            }

            // Handle definition: ensure interval starts here or earlier
            let (defs, _) = Self::get_instr_defs_uses(instr);
            for def_var in defs {
                let interval = intervals
                    .entry(def_var.clone())
                    .or_insert_with(|| LiveInterval {
                        var: def_var.clone(),
                        range: linear_idx..(linear_idx + 1), // Definition starts interval
                        location: None,
                    });
                interval.range.start = interval.range.start.min(linear_idx);
                interval.range.end = interval.range.end.max(linear_idx + 1); // Cover def point
            }

            // Handle uses: ensure interval covers the use point
            let (_, uses) = Self::get_instr_defs_uses(instr);
            for use_var in uses {
                let interval = intervals
                    .entry(use_var.clone())
                    .or_insert_with(|| LiveInterval {
                        var: use_var.clone(),
                        range: linear_idx..(linear_idx + 1), // Use ensures liveness
                        location: None,
                    });
                interval.range.start = interval.range.start.min(linear_idx);
                interval.range.end = interval.range.end.max(linear_idx + 1); // Cover use point
            }

            // Phi nodes definitions conceptually happen at the start of the block.
            if let Instr::Phi(dest, _) = instr {
                 let block_start_linear = block_range_linear.start;
                 let interval = intervals.entry(dest.clone()).or_insert_with(|| LiveInterval {
                     var: dest.clone(),
                     range: block_start_linear..block_start_linear + 1,
                     location: None,
                 });
                 interval.range.start = interval.range.start.min(block_start_linear);
                 // End is extended by uses/liveness check
            }
        }


        // --- 4. Pre-color Function Parameters based on ABI ---
        let arg_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
        let mut precolored_regs = HashSet::new(); // Track regs used by params

        // Iterate through the *original* CFG blocks to find FuncParam easily
        for (label, block) in &cfg.blocks {
            for (idx_in_block, instr) in block.instrs.iter().enumerate() {
                if let Instr::FuncParam { name, index } = instr {
                    if *index < arg_regs.len() {
                        let reg = arg_regs[*index].to_string();
                        if let Some(interval) = intervals.get_mut(name) {
                            // Find the linear index for logging/debugging if needed
                            let linear_idx = original_to_linear_map[&(label.clone(), idx_in_block)];
                            eprintln!(
                                "Pre-coloring param {}: {} (linear idx {}, range {:?}) -> Register {}",
                                index, name, linear_idx, interval.range, reg
                            );
                            interval.location = Some(Location::Register(reg.clone()));
                            precolored_regs.insert(reg.clone());
                        } else {
                            eprintln!("Warning: Live interval not found for FuncParam {}", name);
                        }
                    }
                    // TODO: Handle params > 6 (stack allocation)
                }
            }
        }

        // Convert intervals map to a Vec and sort by start point (linear index)
        let mut sorted_intervals: Vec<_> = intervals
            .values()
            .filter(|&i| !i.range.is_empty()) // Filter out empty ranges
            .cloned()
            .collect();
        sorted_intervals.sort_by_key(|i| i.range.start);

        // --- 5. Linear Scan Allocation ---
        let mut active: Vec<LiveInterval> = Vec::new();
        let mut free_regs: VecDeque<String> = AVAILABLE_REGS
            .iter()
            .map(|s| s.to_string())
            .filter(|r| !precolored_regs.contains(r)) // Don't initially offer precolored regs
            .collect();
        let mut final_locations = BTreeMap::new();

        for current_interval_ref in sorted_intervals {
            let mut current = current_interval_ref.clone(); // Clone to modify location

            // --- Check for Pre-colored Interval ---
            if let Some(preassigned_loc) = &current.location {
                if let Location::Register(preassigned_reg) = preassigned_loc {
                    eprintln!(
                        "Processing pre-colored interval: {} at {:?} ({:?})",
                        current.var, preassigned_loc, current.range
                    );
                    final_locations.insert(current.var.clone(), preassigned_loc.clone());
                    expire_old_intervals(&mut active, &mut free_regs, current.range.start);

                    // Remove the pre-colored register from free_regs if it's there
                    // (it shouldn't be due to the filter earlier, but double-check)
                    free_regs.retain(|r| r != preassigned_reg);

                    // Add this pre-colored interval to active list
                    active.push(current); // current is moved here
                    active.sort_by_key(|i| i.range.end);
                    continue; // Skip normal allocation/spilling
                }
            }

            // Expire old intervals in active
            expire_old_intervals(&mut active, &mut free_regs, current.range.start);

            let location: Location;
            // Check available registers considering *all* registers minus active ones
            let num_available_total = AVAILABLE_REGS.len();
            if active.len() >= num_available_total { // Check against total available regs
                // Spill: No free registers among the total set
                active.sort_by_key(|i| i.range.end); // Ensure active is sorted by end point
                let spill_candidate = &active[active.len() - 1]; // Last element ends latest

                if current.range.end > spill_candidate.range.end {
                    // Spill current interval
                    location = Location::Spill;
                    final_locations.insert(current.var.clone(), location.clone());
                    eprintln!(
                        "Spilling CURRENT: {} (range {:?})",
                        current.var, current.range
                    );
                    // Don't add current to active list
                } else {
                    // Spill the active interval candidate
                    let spilled_interval = active.pop().unwrap(); // Remove the last one
                    let reg = match &spilled_interval.location {
                        Some(Location::Register(r)) => r.clone(),
                        _ => panic!("Interval to spill had no register! Var: {}", spilled_interval.var),
                    };
                    eprintln!(
                        "Spilling ACTIVE: {} (range {:?}) for {} (range {:?})",
                        spilled_interval.var, spilled_interval.range,
                        current.var, current.range
                    );
                    final_locations.insert(spilled_interval.var.clone(), Location::Spill);

                    // Assign the freed register to current
                    location = Location::Register(reg.clone());
                    final_locations.insert(current.var.clone(), location.clone());
                    current.location = Some(location.clone());
                    eprintln!("Assigning Register to {}: {:?}", current.var, location);
                    active.push(current); // Add current to active list
                    active.sort_by_key(|i| i.range.end);
                }
            } else {
                // Allocate register
                let reg = free_regs.pop_front().expect("Free regs available but list empty!");
                location = Location::Register(reg);
                final_locations.insert(current.var.clone(), location.clone());
                current.location = Some(location.clone());
                eprintln!("Assigning Register to {}: {:?}", current.var, location);
                active.push(current); // Add current to active list
                active.sort_by_key(|i| i.range.end);
            }
        }

        // Ensure all variables have a location assigned. Default to spill.
        for var in all_vars {
             final_locations.entry(var.clone()).or_insert_with(|| {
                  eprintln!("Warning: Variable {} was not assigned a location by LSRA, defaulting to Spill.", var);
                  Location::Spill
              });
        }

        // Debug print final locations
        eprintln!("--- Final Variable Locations ---");
        let mut sorted_locations_dbg: Vec<_> = final_locations.iter().collect();
        sorted_locations_dbg.sort_by_key(|(k, _)| *k);
        for (var, loc) in sorted_locations_dbg {
            eprintln!("{}: {:?}", var, loc);
        }
        eprintln!("--------------------------------");

        // TODO: Implement instruction rewriting for spills if necessary.
        // This would modify the CFG before returning it.

        // Return the original CFG and the locations map
        (cfg.clone(), final_locations)
    }
}
