use std::collections::HashMap;

use crate::optimizer::passes::pass::Pass;
use crate::optimizer::{Instr, Op};

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
    fn optimize(&self, instrs: Vec<Instr>) -> Vec<Instr> {
        let mut optimized_instrs = Vec::with_capacity(instrs.len());
        // Need to track constant values to perform the reduction
        let mut constants: HashMap<String, i64> = HashMap::new();

        for instr in instrs {
            match instr {
                Instr::Const(ref name, val) => {
                    constants.insert(name.clone(), val);
                    optimized_instrs.push(instr.clone()); // Keep the const instruction
                }
                Instr::BinOp(ref dest, ref left, op, ref right) => {
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
                                        optimized_instrs
                                            .push(Instr::Const(shift_amount_reg.clone(), k as i64));

                                        // 2. Perform the shift: x << k
                                        let shl_temp = if is_first_term && exponents.len() == 1 {
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
                                    if let Some(exponent) = Self::get_power_of_two_exponent(rv) {
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
                                        if let Some(exponent) = Self::get_power_of_two_exponent(lv)
                                        {
                                            // Optimization: 2^k * x => x << k
                                            let shift_amount_reg = format!("{}_shift_amt", dest);
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
                                optimized_instrs
                                    .push(Instr::Const(shift_amount_reg.clone(), exponent as i64));
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
                Instr::Assign(ref dest, ref src) => {
                    // Propagate constants through assignments
                    if let Some(&val) = constants.get(src) {
                        constants.insert(dest.clone(), val);
                    } else {
                        constants.remove(dest);
                    }
                    optimized_instrs.push(instr.clone());
                }
                Instr::FuncParam { name, .. } => {
                    constants.remove(&name);
                    optimized_instrs.push(Instr::FuncParam { name, index: 0 }); // Keep instruction
                }
                _ => {
                    // For other instructions, invalidate potential definitions
                    // A more robust approach would track defs per instruction type
                    if let Some(def_reg) = get_defined_register(&instr) {
                        constants.remove(&def_reg);
                    }
                    optimized_instrs.push(instr.clone());
                }
            }
        }

        optimized_instrs
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
    use crate::optimizer::Instr;

    #[test]
    fn test_multiply_by_power_of_two() {
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 5), // x = 5
            Instr::Const("t1".to_string(), 8), // y = 8 (which is 2^3)
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Mul,
                "t1".to_string(),
            ), // t2 = t0 * t1
            Instr::Print("t2".to_string()),
        ];
        let pass = StrengthReduction;
        let optimized = pass.optimize(instrs);

        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 5),
            Instr::Const("t1".to_string(), 8),
            Instr::Const("t2_shift_amt".to_string(), 3), // exponent = 3
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Shl,
                "t2_shift_amt".to_string(),
            ), // t2 = t0 << 3
            Instr::Print("t2".to_string()),
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_divide_by_power_of_two() {
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 40), // x = 40
            Instr::Const("t1".to_string(), 4),  // y = 4 (which is 2^2)
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Div,
                "t1".to_string(),
            ), // t2 = t0 / t1
            Instr::Print("t2".to_string()),
        ];
        let pass = StrengthReduction;
        let optimized = pass.optimize(instrs);

        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 40),
            Instr::Const("t1".to_string(), 4),
            Instr::Const("t2_shift_amt".to_string(), 2), // exponent = 2
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Shr,
                "t2_shift_amt".to_string(),
            ), // t2 = t0 >> 2
            Instr::Print("t2".to_string()),
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_divide_by_non_power_of_two() {
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 40),
            Instr::Const("t1".to_string(), 5), // Not power of 2
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Div,
                "t1".to_string(),
            ),
            Instr::Print("t2".to_string()),
        ];
        let pass = StrengthReduction;
        let original_instrs = instrs.clone();
        let optimized = pass.optimize(instrs);

        // Should not be changed
        assert_eq!(optimized, original_instrs);
    }

    #[test]
    fn test_divide_non_constant_divisor() {
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 40),
            Instr::Assign("t1".to_string(), "some_var".to_string()), // t1 is not constant
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Div,
                "t1".to_string(),
            ),
            Instr::Print("t2".to_string()),
        ];
        let pass = StrengthReduction;
        let original_instrs = instrs.clone();
        let optimized = pass.optimize(instrs);

        // Should not be changed
        assert_eq!(optimized, original_instrs);
    }

    #[test]
    fn test_multiply_non_constant() {
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Assign("t0".to_string(), "some_var".to_string()), // t0 is not constant
            Instr::Const("t1".to_string(), 8),
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Mul,
                "t1".to_string(),
            ),
            Instr::Print("t2".to_string()),
        ];
        let pass = StrengthReduction;

        // Expect the multiply with const 8 on the right to be converted
        let optimized = pass.optimize(instrs);
        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Assign("t0".to_string(), "some_var".to_string()),
            Instr::Const("t1".to_string(), 8),
            Instr::Const("t2_shift_amt".to_string(), 3),
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Shl,
                "t2_shift_amt".to_string(),
            ),
            Instr::Print("t2".to_string()),
        ];
        assert_eq!(optimized, expected);
    }

    #[test]
    fn test_constant_propagation_needed() {
        // This test demonstrates that the pass handles simple constant propagation
        // through Assign instructions due to its internal `constants` map.
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("c8".to_string(), 8),
            Instr::Assign("t1".to_string(), "c8".to_string()), // t1 = 8, propagated internally
            Instr::Const("t0".to_string(), 5),
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Mul,
                "t1".to_string(),
            ), // t2 = t0 * t1 (should be reduced)
            Instr::Print("t2".to_string()),
        ];
        let pass = StrengthReduction;
        let optimized = pass.optimize(instrs);

        // Because the pass tracks constants and propagates them through Assign,
        // it correctly identifies t1 as 8 and performs strength reduction.
        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("c8".to_string(), 8),
            Instr::Assign("t1".to_string(), "c8".to_string()),
            Instr::Const("t0".to_string(), 5),
            Instr::Const("t2_shift_amt".to_string(), 3),
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Shl,
                "t2_shift_amt".to_string(),
            ),
            Instr::Print("t2".to_string()),
        ];

        assert_eq!(
            optimized, expected,
            "Optimization should happen due to internal constant propagation through Assign"
        );
    }
    #[test]
    fn test_multiply_by_constant_shift_add() {
        let instrs = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 5),  // x = 5
            Instr::Const("t1".to_string(), 10), // c = 10 (binary 1010)
            Instr::BinOp(
                "t2".to_string(),
                "t0".to_string(),
                Op::Mul,
                "t1".to_string(),
            ), // t2 = t0 * t1
            Instr::Print("t2".to_string()),
        ];
        let pass = StrengthReduction;
        let optimized = pass.optimize(instrs);

        // Expected: t2 = (t0 << 1) + (t0 << 3)
        let expected = vec![
            Instr::Label("entry".to_string()),
            Instr::Const("t0".to_string(), 5),
            Instr::Const("t1".to_string(), 10),
            // Shift for bit 1 (k=1)
            Instr::Const("t2_shift_1".to_string(), 1),
            Instr::BinOp(
                "t2_shl_1".to_string(), // Intermediate result for x << 1
                "t0".to_string(),
                Op::Shl,
                "t2_shift_1".to_string(),
            ),
            // Shift for bit 3 (k=3)
            Instr::Const("t2_shift_3".to_string(), 3),
            Instr::BinOp(
                "t2_shl_3".to_string(), // Intermediate result for x << 3
                "t0".to_string(),
                Op::Shl,
                "t2_shift_3".to_string(),
            ),
            // Add the results: (x << 1) + (x << 3)
            Instr::BinOp(
                "t2".to_string(), // Final result
                "t2_shl_1".to_string(),
                Op::Add,
                "t2_shl_3".to_string(),
            ),
            Instr::Print("t2".to_string()),
        ];

        assert_eq!(optimized, expected);
    }
}
