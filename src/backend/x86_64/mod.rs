use std::collections::BTreeMap;

use crate::optimizer::{CFG, registers::Location, CmpOp, Instr, Op}; // Added cfg::CFG

use super::Backend;

#[derive(Default, Clone)]
pub struct Codegen {
    // Removed phi_prep_moves and current_block
    asm: Vec<String>,
}

// Removed BlockInfo struct

impl Backend for Codegen {
    // Updated signature to accept CFG
    fn generate_assembly(
        &mut self,
        cfg: &CFG, // Changed from instrs: &[Instr]
        locations: &BTreeMap<String, Location>,
    ) -> String {
        self.asm.clear(); // Clear previous assembly

        // --- Helper to resolve SSA temps to locations ---
        let resolve = |t: &String| -> String {
            match locations
                .get(t)
                .unwrap_or_else(|| panic!("Codegen: Could not find location for temporary variable '{}'", t))
            {
                Location::Register(reg) => reg.clone(),
                Location::Spill => format!("{}(%rip)", t), // Assumes temps are globals in .bss
            }
        };

        // --- Emit Static Data ---
        self.add(".section .data");
        self.add("fmt: .string \"%ld\\n\""); // Format string for print

        // --- Emit BSS for Spilled Variables ---
        self.add(".section .bss");
        // Sort locations to ensure deterministic output
        let mut sorted_locations: Vec<_> = locations.iter().collect();
        sorted_locations.sort_by_key(|(name, _)| *name);
        for (temp, loc) in sorted_locations {
            if let Location::Spill = loc {
                self.add(format!("{}: .quad 0", temp)); // Reserve 8 bytes for each spilled temp
            }
        }

        // --- Emit Code Section ---
        self.add(".section .text");
        // Assuming 'main' is the entry point for now. Proper function handling needed later.
        // Find the 'main' block label if it exists, otherwise assume the first block is main.
        let entry_label = cfg.blocks.keys().next().cloned().unwrap_or_else(|| "main".to_string());
        if entry_label == "main" {
             self.add(".globl main");
        }
        // Could add .globl for other function entry points if CFG distinguishes them.


        // --- Iterate Through CFG Blocks (Sorted for Determinism) ---
        let sorted_block_labels: Vec<_> = cfg.blocks.keys().cloned().collect();
        // Consider sorting based on a topological sort or reverse postorder for potentially better code layout later.
        // For now, alphabetical sort is deterministic.

        for block_label in &sorted_block_labels {
            let block = cfg.blocks.get(block_label).expect("Block label not found in CFG");

            // Emit the block label
            self.add(format!("{}:", block_label));

            // --- Process Instructions within the Block ---
            let num_instrs = block.instrs.len();
            for (i, instr) in block.instrs.iter().enumerate() {
                // Skip Phi nodes here; they are handled at the end of predecessors.
                if matches!(instr, Instr::Phi(_, _)) {
                    continue;
                }

                // Check if this is the terminator instruction
                let is_terminator = i == num_instrs - 1;

                // --- Insert Phi Moves BEFORE Terminator ---
                if is_terminator {
                    match instr {
                        Instr::Jump(target_label) => {
                            self.emit_phi_moves(block_label, target_label, cfg, &resolve);
                        }
                        Instr::BranchIf(_, then_label, else_label) => {
                            // Moves for the 'then' edge (taken if condition is true/jne)
                            // These need to be emitted *before* the conditional jump instruction
                            // because the jne jumps *over* them if the condition is false.
                            // This is complex. A common technique is to duplicate moves or use conditional moves.
                            // Simpler approach: Emit moves *after* the label for the target block,
                            // but that breaks the "at end of predecessor" rule.
                            // Let's stick to emitting *before* the terminator for now,
                            // acknowledging potential control flow issues with BranchIf.
                            // A better approach involves dedicated edge blocks or careful layout.

                            // Emit moves for the 'else' edge (fallthrough/jmp path)
                            self.emit_phi_moves(block_label, else_label, cfg, &resolve);
                            // Emit moves for the 'then' edge
                            self.emit_phi_moves(block_label, then_label, cfg, &resolve);

                        }
                        Instr::Ret { .. } => {
                            // No successors, no Phi moves needed.
                        }
                        _ => {
                            // Instruction is not a standard terminator, but it's the last one.
                            // This might indicate fallthrough, which should ideally be an explicit Jump.
                            // If fallthrough is intended, find the next block label and emit Phi moves.
                            // For now, assume explicit terminators are required by the CFG.
                            eprintln!("Warning: Block '{}' ends with non-terminator instruction: {:?}", block_label, instr);
                        }
                    }
                }

                // --- Generate Assembly for the Current Instruction ---
                match instr {
                    Instr::Const(name, val) => {
                        let dst = resolve(name);
                        self.add(format!("movq ${}, {}", val, dst));
                    }
                    Instr::BinOp(dest, left, op, right) => {
                        let l = resolve(left);
                        let r = resolve(right);
                        let d = resolve(dest);

                        match op {
                            Op::Div | Op::Mod => {
                                // Ensure dividend (left) is in %rax, divisor (right) is not %rax or %rdx
                                // Ensure %rdx is zeroed before idivq
                                // Use temporary registers if needed to avoid clobbering inputs during setup
                                let rax = "%rax";
                                let rdx = "%rdx";

                                // Simple case: left is already %rax, right is not %rax/%rdx
                                if l == rax && r != rax && r != rdx {
                                    self.add("cqto"); // Sign extend %rax into %rdx:%rax
                                    self.add(format!("idivq {}", r));
                                }
                                // Case: right is %rax, left is not %rax/%rdx
                                else if r == rax && l != rax && l != rdx {
                                    // Swap using another register, e.g., %r11 (caller-saved, less likely used)
                                    let temp_reg = "%r11";
                                    self.add(format!("movq {}, {}", rax, temp_reg)); // Save divisor
                                    self.add(format!("movq {}, {}", l, rax));    // Move dividend to %rax
                                    self.add("cqto");
                                    self.add(format!("idivq {}", temp_reg));     // Divide by saved divisor
                                }
                                // General case: Use %r10, %r11 as temps
                                else {
                                    let temp_reg_l = "%r10";
                                    let temp_reg_r = "%r11";
                                    self.move_memory(&l, temp_reg_l); // Move left to r10
                                    self.move_memory(&r, temp_reg_r); // Move right to r11
                                    self.add(format!("movq {}, {}", temp_reg_l, rax)); // Dividend to %rax
                                    self.add("cqto");
                                    self.add(format!("idivq {}", temp_reg_r)); // Divide by r11
                                }

                                // Move result (%rax for Div, %rdx for Mod) to destination
                                if *op == Op::Div {
                                    self.move_memory(rax, &d);
                                } else {
                                    self.move_memory(rdx, &d);
                                }
                            }
                            _ => {
                                // Standard binary ops often use a 2-operand form (op src, dest)
                                // Move left to dest, then perform op with right
                                let op_instr = match op {
                                    Op::Add => "addq",
                                    Op::Sub => "subq",
                                    Op::Mul => "imulq", // imulq src, dest (dest = dest * src)
                                    Op::BitOr | Op::Or => "orq",
                                    Op::BitAnd | Op::And => "andq",
                                    Op::Shl => "salq", // Use arithmetic shift for signed? Or logical 'shlq'? Assuming shlq.
                                    Op::Shr => "sarq", // Use arithmetic shift for signed? Or logical 'shrq'? Assuming shrq.
                                    _ => unreachable!("Op {:?} should be handled by Div/Mod", op),
                                };

                                // Handle shifts: requires count in %cl or immediate
                                if *op == Op::Shl || *op == Op::Shr {
                                     // If right operand is a constant immediate, use that directly
                                     // Need to check if 'r' resolves to an immediate value - requires more info
                                     // For now, assume 'r' is a register/memory, move to %cl
                                     let cl = "%cl";
                                     self.move_memory(&r, cl); // Move count to %cl
                                     self.move_memory(&l, &d); // Move value to destination
                                     self.add(format!("{} {}, {}", op_instr, cl, d)); // Shift dest by %cl
                                }
                                // Handle multiplication where src and dest can be different
                                else if *op == Op::Mul {
                                     // imul src, dest (dest = dest * src)
                                     // imul src (rax = rax * src, result in rdx:rax) - less useful here
                                     // imul src, reg (reg = reg * src)
                                     // imul imm, src, dest (dest = src * imm)
                                     self.move_memory(&l, &d); // Move left to destination
                                     self.add(format!("{} {}, {}", op_instr, r, d)); // d = d * r
                                }
                                // Handle Add, Sub, Or, And
                                else {
                                     self.move_memory(&l, &d); // Move left to destination
                                     self.add(format!("{} {}, {}", op_instr, r, d)); // d = d op r
                                }
                            }
                        }
                    }
                    Instr::Print(name) => {
                        let src = resolve(name);
                        self.add(format!("movq {}, %rsi", src)); // Value to print in %rsi
                        self.add("leaq fmt(%rip), %rdi"); // Format string address in %rdi
                        self.add("xor %rax, %rax"); // No vector args
                        self.add("call printf");
                    }
                    Instr::Cmp(dest, left, cmp_op, right) => {
                        let dest_loc = resolve(dest);
                        let l = resolve(left);
                        let r = resolve(right);
                        self.add(format!("cmpq {}, {}", r, l)); // Compare r with l, sets flags based on l - r
                        let set_instr = match cmp_op {
                            CmpOp::Eq => "sete",   // Set if Equal (ZF=1)
                            CmpOp::Neq => "setne", // Set if Not Equal (ZF=0)
                            CmpOp::Lt => "setl",   // Set if Less (SF!=OF)
                            CmpOp::Lte => "setle", // Set if Less or Equal (ZF=1 or SF!=OF)
                            CmpOp::Gt => "setg",   // Set if Greater (ZF=0 and SF=OF)
                            CmpOp::Gte => "setge", // Set if Greater or Equal (SF=OF)
                        };
                        // sete/setne/... operate on a byte register (%al, %bl, etc.)
                        self.add(format!("{} %al", set_instr));
                        // Zero-extend the byte result (%al) into a quadword register (%rax)
                        self.add("movzbq %al, %rax");
                        // Move the quadword result to the destination
                        self.move_memory("%rax", &dest_loc);
                    }
                    Instr::BranchIf(cond, then_label, else_label) => {
                        // Phi moves should have been emitted before this instruction
                        let cond_loc = resolve(cond);
                        self.add(format!("cmpq $0, {}", cond_loc)); // Test if condition is non-zero
                        self.add(format!("jne {}", then_label)); // Jump to then_label if non-zero (true)
                        // If zero (false), execution falls through to the jump to else_label
                        self.add(format!("jmp {}", else_label));
                    }
                    Instr::Jump(label) => {
                        // Phi moves should have been emitted before this instruction
                        self.add(format!("jmp {}", label));
                    }
                    Instr::Label(_) => {
                        // Label was already emitted at the start of the block processing.
                    }
                    Instr::Phi(_, _) => {
                        // Phi nodes handled by moves in predecessors. Do nothing here.
                    }
                    Instr::Call {
                        target,
                        args,
                        result,
                    } => {
                        // TODO: Handle > 6 arguments (stack passing)
                        // TODO: Handle floating point arguments
                        // TODO: Handle caller/callee saved registers based on calling convention & liveness
                        let arg_regs = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

                        // Move arguments into registers
                        for (idx, arg_ssa) in args.iter().enumerate() {
                            if idx >= arg_regs.len() {
                                panic!("Codegen: More than 6 arguments not yet supported for call to '{}'", target);
                            }
                            let arg_loc = resolve(arg_ssa);
                            let target_reg = arg_regs[idx];
                            // Avoid clobbering if arg_loc is needed later or is another arg reg
                            // This requires careful register allocation or temporary moves.
                            // Simple move for now:
                            self.move_memory(&arg_loc, target_reg);
                        }

                        // Call the function
                        self.add(format!("call {}", target));

                        // Move result from %rax (if any)
                        if let Some(res_ssa) = result {
                            let res_loc = resolve(res_ssa);
                            // Avoid clobbering %rax if res_loc is %rax
                            if res_loc != "%rax" {
                                self.move_memory("%rax", &res_loc);
                            }
                        }
                    }
                    Instr::Ret { value } => {
                        if let Some(val_ssa) = value {
                            let val_loc = resolve(val_ssa);
                            // Move return value to %rax if it's not already there
                            if val_loc != "%rax" {
                                self.move_memory(&val_loc, "%rax");
                            }
                        } else {
                            // No return value specified, convention is often to return 0
                            self.add("xor %rax, %rax");
                        }
                        // TODO: Add function epilogue (restore stack frame, etc.) if needed
                        self.add("ret");
                    }
                    Instr::Assign(dest, src) => {
                        // This might be redundant if register allocation handles coalescing.
                        // If present, it's a simple move.
                        let dest_loc = resolve(dest);
                        let src_loc = resolve(src);
                        self.move_memory(&src_loc, &dest_loc);
                    }
                    Instr::FuncParam { .. } => {
                        // No assembly generated for FuncParam itself.
                        // Handled by register allocator assigning parameter registers/stack slots.
                    }
                    // Handle other instructions if any
                } // End match instr
            } // End loop instructions in block
        } // End loop blocks in CFG

        self.format_asm(&self.asm)
    }
}

impl Codegen {
    fn format_asm(&self, lines: &[String]) -> String {
        lines
            .iter()
            .map(|line| {
                let trimmed = line.trim_end();
                if trimmed.ends_with(':') // Labels
                    || trimmed.starts_with('.') // Directives
                    || trimmed.is_empty()
                {
                    line.to_string() // No indentation
                } else {
                    format!("  {}", line) // Indent instructions
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    // Helper to emit moves for Phi nodes targeting `target_block_label`
    // from the `current_block_label`.
    fn emit_phi_moves(
        &mut self,
        current_block_label: &str,
        target_block_label: &str,
        cfg: &CFG,
        resolve: &impl Fn(&String) -> String,
    ) {
        if let Some(target_block) = cfg.blocks.get(target_block_label) {
            for instr in &target_block.instrs {
                if let Instr::Phi(phi_dest_ssa, edges) = instr {
                    // Find the value coming from the current block
                    if let Some((_, pred_val_ssa)) = edges
                        .iter()
                        .find(|(pred_label, _)| pred_label == current_block_label)
                    {
                        let dest_loc = resolve(phi_dest_ssa);
                        let src_loc = resolve(pred_val_ssa);

                        // Check if source and destination are the same (common after reg alloc)
                        if dest_loc != src_loc {
                             eprintln!(
                                 "Phi Move (at end of {} for edge to {}): {} <- {}",
                                 current_block_label, target_block_label, dest_loc, src_loc
                             );
                            self.move_memory(&src_loc, &dest_loc);
                        }
                    } else {
                         // This case should ideally not happen if the CFG and Phi nodes are consistent
                         eprintln!(
                             "Warning: Phi node in block '{}' has no edge from predecessor '{}'. Phi: {:?}",
                             target_block_label, current_block_label, instr
                         );
                    }
                } else {
                    // Phi nodes only appear at the beginning of a block.
                    break;
                }
            }
        } else {
             eprintln!(
                 "Warning: Target block '{}' for Phi moves not found in CFG.",
                 target_block_label
             );
        }
    }


    // Move data between registers and/or memory, handling memory-to-memory restriction.
    fn move_memory(&mut self, src: &str, dest: &str) {
        // Avoid redundant moves
        if src == dest {
            return;
        }

        let is_src_mem = src.contains("(%rip)");
        let is_dest_mem = dest.contains("(%rip)");

        if is_src_mem && is_dest_mem {
            // Use %rax as temporary for memory-to-memory moves.
            // WARNING: This clobbers %rax. Assumes %rax is not live with a value
            // that needs preserving across this move. A robust implementation
            // would check liveness or use a dedicated scratch register.
            self.add(format!("movq {}, %rax", src));
            self.add(format!("movq %rax, {}", dest));
        } else {
            // Direct move allowed (reg-reg, reg-mem, mem-reg)
            self.add(format!("movq {}, {}", src, dest));
        }
    }

    fn add<S: AsRef<str>>(&mut self, line: S) {
        self.asm.push(line.as_ref().to_string());
    }

    // Removed preprocess_ir_and_build_maps function
}
