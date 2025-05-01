use crate::optimizer::passes::pass::Pass;
use crate::optimizer::{CFG, Instr, Op};
use std::collections::HashMap;

/// Strength Reduction Optimization Pass
///
/// Replaces expensive operations with cheaper ones:
/// - Multiplication by power of 2 -> Left shift (x * 2^n => x << n)
/// - Integer division by power of 2 -> Right shift (x / 2^n => x >> n)
pub struct StrengthReduction;

impl StrengthReduction {
    /// Checks if a number is a power of two and returns its exponent.
    /// Returns None if the number is not a positive power of two.
    fn get_power_of_two_exponent(n: i64) -> Option<u32> {
        if n <= 0 || (n & (n - 1)) != 0 {
            // Not a power of two (or is zero/negative)
            None
        } else {
            // Calculate the exponent (log base 2)
            // n.trailing_zeros() is efficient for this when n is known to be a power of 2.
            Some(n.trailing_zeros())
        }
    }
}

impl Pass for StrengthReduction {
    fn optimize(&self, mut cfg: CFG) -> CFG {
        // Track constants across the CFG
        let mut constants: HashMap<String, i64> = HashMap::new();

        // Process each block
        for block in cfg.blocks.values_mut() {
            let mut optimized_instrs = Vec::with_capacity(block.instrs.len());

            for instr in &block.instrs {
                match instr {
                    Instr::Const(name, val) => {
                        constants.insert(name.clone(), *val);
                        optimized_instrs.push(instr.clone()); // Keep the const instruction
                    }
                    Instr::BinOp(dest, left, op, right) => {
                        let left_val = constants.get(left);
                        let right_val = constants.get(right);
                        let mut replaced = false;

                        match op {
                            Op::Mul => {
                                let mut handled_mul = false; // Flag to track if multiplication was handled

                                // --- Optimization: Multiplication by Constant ---

                                // Helper function to generate the shift-and-add sequence
                                let generate_shift_add =
                                    |optimized_instrs: &mut Vec<Instr>,
                                     dest: &String,
                                     x: &String,
                                     c: i64| {
                                        let mut exponents = Vec::new();
                                        for k in 0..63 {
                                            // Check bits 0 to 62
                                            if (c >> k) & 1 == 1 {
                                                exponents.push(k as u32);
                                            }
                                        }

                                        if exponents.is_empty() {
                                            // Should not happen if c > 0
                                            return false;
                                        }

                                        let mut last_sum_reg = String::new();
                                        let mut is_first_term = true;

                                        for (i, &k) in exponents.iter().enumerate() {
                                            // 1. Create shift amount constant
                                            let shift_amount_reg = format!("{}_shift_{}", dest, k);
                                            optimized_instrs.push(Instr::Const(
                                                shift_amount_reg.clone(),
                                                k as i64,
                                            ));

                                            // 2. Perform the shift: x << k
                                            let shl_temp = if is_first_term && exponents.len() == 1
                                            {
                                                // If only one term, result goes directly to dest
                                                dest.clone()
                                            } else {
                                                format!("{}_shl_{}", dest, k)
                                            };
                                            optimized_instrs.push(Instr::BinOp(
                                                shl_temp.clone(),
                                                x.clone(),
                                                Op::Shl,
                                                shift_amount_reg,
                                            ));

                                            // 3. Add to the sum
                                            if is_first_term {
                                                last_sum_reg = shl_temp;
                                                is_first_term = false;
                                            } else {
                                                let current_sum_reg = if i == exponents.len() - 1 {
                                                    // Last term addition writes to final dest
                                                    dest.clone()
                                                } else {
                                                    format!("{}_sum_{}", dest, i)
                                                };
                                                optimized_instrs.push(Instr::BinOp(
                                                    current_sum_reg.clone(),
                                                    last_sum_reg,
                                                    Op::Add,
                                                    shl_temp,
                                                ));
                                                last_sum_reg = current_sum_reg;
                                            }
                                        }
                                        true // Indicate success
                                    };

                                // Check right operand first
                                if let Some(&rv) = right_val {
                                    if rv == 0 {
                                        // Optimization: x * 0 => 0
                                        optimized_instrs.push(Instr::Const(dest.clone(), 0));
                                        handled_mul = true;
                                    } else if rv == 1 {
                                        // Optimization: x * 1 => x
                                        optimized_instrs
                                            .push(Instr::Assign(dest.clone(), left.clone()));
                                        handled_mul = true;
                                    } else if rv > 1 {
                                        if let Some(exponent) = Self::get_power_of_two_exponent(rv)
                                        {
                                            // Optimization: x * 2^k => x << k
                                            let shift_amount_reg = format!("{}_shift_amt", dest);
                                            optimized_instrs.push(Instr::Const(
                                                shift_amount_reg.clone(),
                                                exponent as i64,
                                            ));
                                            optimized_instrs.push(Instr::BinOp(
                                                dest.clone(),
                                                left.clone(),
                                                Op::Shl,
                                                shift_amount_reg,
                                            ));
                                            handled_mul = true;
                                        } else {
                                            // Optimization: x * c => shift and add
                                            handled_mul = generate_shift_add(
                                                &mut optimized_instrs,
                                                dest,
                                                left,
                                                rv,
                                            );
                                        }
                                    }
                                }

                                // If not handled, check left operand (commutative)
                                if !handled_mul {
                                    if let Some(&lv) = left_val {
                                        if lv == 0 {
                                            // Optimization: 0 * x => 0
                                            optimized_instrs.push(Instr::Const(dest.clone(), 0));
                                            handled_mul = true;
                                        } else if lv == 1 {
                                            // Optimization: 1 * x => x
                                            optimized_instrs
                                                .push(Instr::Assign(dest.clone(), right.clone()));
                                            handled_mul = true;
                                        } else if lv > 1 {
                                            if let Some(exponent) =
                                                Self::get_power_of_two_exponent(lv)
                                            {
                                                // Optimization: 2^k * x => x << k
                                                let shift_amount_reg =
                                                    format!("{}_shift_amt", dest);
                                                optimized_instrs.push(Instr::Const(
                                                    shift_amount_reg.clone(),
                                                    exponent as i64,
                                                ));
                                                optimized_instrs.push(Instr::BinOp(
                                                    dest.clone(),
                                                    right.clone(), // Shift the variable part
                                                    Op::Shl,
                                                    shift_amount_reg,
                                                ));
                                                handled_mul = true;
                                            } else {
                                                // Optimization: c * x => shift and add
                                                handled_mul = generate_shift_add(
                                                    &mut optimized_instrs,
                                                    dest,
                                                    right,
                                                    lv,
                                                );
                                            }
                                        }
                                    }
                                }

                                // Update the replaced flag based on whether we handled the multiplication
                                replaced = handled_mul;
                            }
                            Op::Div => {
                                // Only handle division where the divisor is the power of two constant
                                if let Some(exponent) =
                                    right_val.and_then(|&rv| Self::get_power_of_two_exponent(rv))
                                {
                                    // x / (2^exponent) => x >> exponent_const
                                    let shift_amount_reg = format!("{}_shift_amt", dest);
                                    optimized_instrs.push(Instr::Const(
                                        shift_amount_reg.clone(),
                                        exponent as i64,
                                    ));
                                    optimized_instrs.push(Instr::BinOp(
                                        dest.clone(),
                                        left.clone(),
                                        Op::Shr,
                                        shift_amount_reg,
                                    ));
                                    replaced = true;
                                }
                            }
                            Op::Mod => {
                                // Handle modulo where the divisor is the power of two constant
                                if let Some(exponent) =
                                    right_val.and_then(|&rv| Self::get_power_of_two_exponent(rv))
                                {
                                    // x % (2^exponent) => x & ((2^exponent) - 1)
                                    let mask_val = (1i64 << exponent) - 1;
                                    let mask_reg = format!("{}_mask", dest);
                                    optimized_instrs.push(Instr::Const(mask_reg.clone(), mask_val));
                                    optimized_instrs.push(Instr::BinOp(
                                        dest.clone(),
                                        left.clone(),
                                        Op::BitAnd, // Use bitwise AND
                                        mask_reg,
                                    ));
                                    replaced = true;
                                }
                            }
                            _ => {}
                        }

                        if !replaced {
                            // If not replaced, keep the original instruction
                            // Also, invalidate the destination register in constants map if it was there
                            constants.remove(dest);
                            optimized_instrs.push(instr.clone());
                        }
                        // If replaced, the new BinOp might produce a non-constant result,
                        // so we should also invalidate the destination register.
                        constants.remove(dest);
                    }
                    Instr::Assign(dest, src) => {
                        // Propagate constants through assignments
                        if let Some(&val) = constants.get(src) {
                            constants.insert(dest.clone(), val);
                        } else {
                            constants.remove(dest);
                        }
                        optimized_instrs.push(instr.clone());
                    }
                    Instr::FuncParam { name, .. } => {
                        constants.remove(name);
                        optimized_instrs.push(Instr::FuncParam {
                            name: name.clone(),
                            index: 0,
                        }); // Keep instruction
                    }
                    _ => {
                        // For other instructions, invalidate potential definitions
                        // A more robust approach would track defs per instruction type
                        if let Some(def_reg) = get_defined_register(instr) {
                            constants.remove(&def_reg);
                        }
                        optimized_instrs.push(instr.clone());
                    }
                }
            }

            block.instrs = optimized_instrs;
        }

        cfg
    }

    fn name(&self) -> &'static str {
        "StrengthReduction"
    }
}

// Helper to get the register defined by an instruction (if any)
// This is a simplified version; a more complete SSA representation might track this better.
fn get_defined_register(instr: &Instr) -> Option<String> {
    match instr {
        Instr::Const(d, _)
        | Instr::BinOp(d, _, _, _)
        | Instr::Cmp(d, _, _, _)
        | Instr::Phi(d, _)
        | Instr::Assign(d, _) => Some(d.clone()),
        Instr::Call { result, .. } => result.as_ref().cloned(), // Call defines its result
        Instr::FuncParam { name, .. } => Some(name.clone()),    // FuncParam defines name
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::passes::test_helpers::*;

    #[test]
    fn test_multiply_by_power_of_two() {
        let pass = StrengthReduction;
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("t0", 5),
                cnst("t1", 8),
                binop("t2", "t0", Op::Mul, "t1"),
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        let expected = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("t0", 5),
                cnst("t1", 8),
                cnst("t2_shift_amt", 3),
                binop("t2", "t0", Op::Shl, "t2_shift_amt"),
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        assert_eq!(pass.optimize(cfg), expected);
    }

    #[test]
    fn test_divide_by_power_of_two() {
        let pass = StrengthReduction;
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("t0", 40),
                cnst("t1", 4),
                binop("t2", "t0", Op::Div, "t1"),
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        let expected = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("t0", 40),
                cnst("t1", 4),
                cnst("t2_shift_amt", 2),
                binop("t2", "t0", Op::Shr, "t2_shift_amt"),
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        assert_eq!(pass.optimize(cfg), expected);
    }

    #[test]
    fn test_strength_reduction_across_blocks() {
        let pass = StrengthReduction;
        let cfg = create_test_cfg(vec![
            (
                "entry",
                vec![
                    cnst("c", 8), // Power of 2 constant
                    branch("cond", "then", "else"),
                ],
                vec![],
                vec!["then", "else"],
            ),
            (
                "then",
                vec![
                    cnst("x", 5),
                    binop("y", "x", Op::Mul, "c"), // Should be reduced to shift
                    jump("merge"),
                ],
                vec!["entry"],
                vec!["merge"],
            ),
            (
                "else",
                vec![
                    cnst("x", 10),
                    binop("y", "x", Op::Mul, "c"), // Should be reduced to shift
                    jump("merge"),
                ],
                vec!["entry"],
                vec!["merge"],
            ),
            (
                "merge",
                vec![
                    phi("result", vec![("then", "y"), ("else", "y")]),
                    print("result"),
                ],
                vec!["then", "else"],
                vec![],
            ),
        ]);

        let expected = create_test_cfg(vec![
            (
                "entry",
                vec![cnst("c", 8), branch("cond", "then", "else")],
                vec![],
                vec!["then", "else"],
            ),
            (
                "then",
                vec![
                    cnst("x", 5),
                    cnst("y_shift_amt", 3),
                    binop("y", "x", Op::Shl, "y_shift_amt"),
                    jump("merge"),
                ],
                vec!["entry"],
                vec!["merge"],
            ),
            (
                "else",
                vec![
                    cnst("x", 10),
                    cnst("y_shift_amt", 3),
                    binop("y", "x", Op::Shl, "y_shift_amt"),
                    jump("merge"),
                ],
                vec!["entry"],
                vec!["merge"],
            ),
            (
                "merge",
                vec![
                    phi("result", vec![("then", "y"), ("else", "y")]),
                    print("result"),
                ],
                vec!["then", "else"],
                vec![],
            ),
        ]);

        assert_eq!(pass.optimize(cfg), expected);
    }

    #[test]
    fn test_divide_by_non_power_of_two() {
        let pass = StrengthReduction;
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("t0", 40),
                cnst("t1", 5), // Not power of 2
                binop("t2", "t0", Op::Div, "t1"),
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        let original_cfg = cfg.clone();
        let optimized = pass.optimize(cfg);

        // Should not be changed
        assert_eq!(optimized, original_cfg);
    }

    #[test]
    fn test_divide_non_constant_divisor() {
        let pass = StrengthReduction;
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("t0", 40),
                assign("t1", "some_var"), // t1 is not constant
                binop("t2", "t0", Op::Div, "t1"),
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        let original_cfg = cfg.clone();
        let optimized = pass.optimize(cfg);

        // Should not be changed
        assert_eq!(optimized, original_cfg);
    }

    #[test]
    fn test_multiply_non_constant() {
        let pass = StrengthReduction;
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                assign("t0", "some_var"), // t0 is not constant
                cnst("t1", 8),
                binop("t2", "t0", Op::Mul, "t1"),
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        // Expect the multiply with const 8 on the right to be converted
        let optimized = pass.optimize(cfg);
        let expected = create_test_cfg(vec![(
            "entry",
            vec![
                assign("t0", "some_var"),
                cnst("t1", 8),
                cnst("t2_shift_amt", 3),
                binop("t2", "t0", Op::Shl, "t2_shift_amt"),
                print("t2"),
            ],
            vec![],
            vec![],
        )]);
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_constant_propagation_needed() {
        // This test demonstrates that the pass handles simple constant propagation
        // through Assign instructions due to its internal `constants` map.
        let pass = StrengthReduction;
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("c8", 8),
                assign("t1", "c8"), // t1 = 8, propagated internally
                cnst("t0", 5),
                binop("t2", "t0", Op::Mul, "t1"), // t2 = t0 * t1 (should be reduced)
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        // Because the pass tracks constants and propagates them through Assign,
        // it correctly identifies t1 as 8 and performs strength reduction.
        let expected = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("c8", 8),
                assign("t1", "c8"),
                cnst("t0", 5),
                cnst("t2_shift_amt", 3),
                binop("t2", "t0", Op::Shl, "t2_shift_amt"),
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        assert_eq!(
            pass.optimize(cfg),
            expected,
            "Optimization should happen due to internal constant propagation through Assign"
        );
    }

    #[test]
    fn test_multiply_by_constant_shift_add() {
        let pass = StrengthReduction;
        let cfg = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("t0", 5),  // x = 5
                cnst("t1", 10), // c = 10 (binary 1010)
                binop("t2", "t0", Op::Mul, "t1"),
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        // Expected: t2 = (t0 << 1) + (t0 << 3)
        let expected = create_test_cfg(vec![(
            "entry",
            vec![
                cnst("t0", 5),
                cnst("t1", 10),
                // Shift for bit 1 (k=1)
                cnst("t2_shift_1", 1),
                binop("t2_shl_1", "t0", Op::Shl, "t2_shift_1"),
                // Shift for bit 3 (k=3)
                cnst("t2_shift_3", 3),
                binop("t2_shl_3", "t0", Op::Shl, "t2_shift_3"),
                // Add the results: (x << 1) + (x << 3)
                binop("t2", "t2_shl_1", Op::Add, "t2_shl_3"),
                print("t2"),
            ],
            vec![],
            vec![],
        )]);

        assert_eq!(pass.optimize(cfg), expected);
    }
}
