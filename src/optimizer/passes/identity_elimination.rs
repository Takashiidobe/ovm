use crate::optimizer::{CFG, BasicBlock, Instr}; // Import CFG and BasicBlock
use crate::optimizer::passes::pass::Pass;
use indexmap::IndexMap; // Assuming CFG uses IndexMap

/// Identity Function Elimination Pass
///
/// Removes instructions of the form `Assign(x, x)` within each basic block of a CFG.
pub struct IdentityElimination;

impl Pass for IdentityElimination {
    // Updated signature to work with CFG
    fn optimize(&self, cfg: CFG) -> CFG {
        let mut optimized_blocks = IndexMap::new();

        for (label, block) in cfg.blocks {
            // Filter instructions within the current block
            let optimized_instrs = block.instrs
                .into_iter()
                .filter(|instr| {
                    // Keep instruction if it's NOT an Assign(x, x) identity
                    !matches!(instr, Instr::Assign(dest, src) if dest == src)
                })
                .collect();

            // Create a new block with the optimized instructions, preserving other fields
            let optimized_block = BasicBlock {
                label: block.label, // Keep original label
                instrs: optimized_instrs,
                preds: block.preds, // Keep original predecessors
                succs: block.succs, // Keep original successors
            };
            optimized_blocks.insert(label, optimized_block);
        }

        // Return a new CFG with the optimized blocks
        CFG {
            blocks: optimized_blocks,
            // current_block is transient state for SSA construction, should likely be None here
            // or managed differently post-optimization. Let's default to None.
            current_block: None,
        }
    }

    fn name(&self) -> &'static str {
        "IdentityElimination"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::optimizer::{CFG, BasicBlock, Instr, Op};
    use indexmap::IndexMap;
    use std::collections::HashSet;

    // Helper to create a simple CFG for testing
    fn create_test_cfg(blocks_data: Vec<(&str, Vec<Instr>, Vec<&str>, Vec<&str>)>) -> CFG {
        let mut blocks = IndexMap::new();
        for (label, instrs, preds, succs) in blocks_data {
            blocks.insert(
                label.to_string(),
                BasicBlock {
                    label: label.to_string(),
                    instrs,
                    preds: preds.into_iter().map(|s| s.to_string()).collect::<HashSet<_>>().into_iter().collect(),
                    succs: succs.into_iter().map(|s| s.to_string()).collect::<HashSet<_>>().into_iter().collect(),
                },
            );
        }
        CFG { blocks, current_block: None }
    }

     // Helper to create instructions more easily
     fn assign(dest: &str, src: &str) -> Instr { Instr::Assign(dest.to_string(), src.to_string()) }
     fn cnst(dest: &str, val: i64) -> Instr { Instr::Const(dest.to_string(), val) }
     fn print(src: &str) -> Instr { Instr::Print(src.to_string()) }
     fn binop(dest: &str, l: &str, op: Op, r: &str) -> Instr { Instr::BinOp(dest.to_string(), l.to_string(), op, r.to_string()) }
     fn jump(target: &str) -> Instr { Instr::Jump(target.to_string()) }


    #[test]
    fn test_removes_identity_assign_cfg() {
        let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("t0", 5),
                assign("t1", "t0"), // Keep: t1 = t0
                assign("t1", "t1"), // Remove: t1 = t1
                print("t1"),
                jump("exit"),
            ], vec![], vec!["exit"]),
            ("exit", vec![Instr::Ret{value: None}], vec!["entry"], vec![]),
        ]);

        let pass = IdentityElimination;
        let optimized_cfg = pass.optimize(cfg);

        let expected_cfg = create_test_cfg(vec![
             ("entry", vec![
                cnst("t0", 5),
                assign("t1", "t0"),
                // assign("t1", "t1") removed
                print("t1"),
                jump("exit"),
            ], vec![], vec!["exit"]),
             ("exit", vec![Instr::Ret{value: None}], vec!["entry"], vec![]),
        ]);

        assert_eq!(optimized_cfg.blocks.len(), expected_cfg.blocks.len());
        for (label, block) in &optimized_cfg.blocks {
             assert_eq!(block.instrs, expected_cfg.blocks[label].instrs);
             assert_eq!(block.preds, expected_cfg.blocks[label].preds);
             assert_eq!(block.succs, expected_cfg.blocks[label].succs);
        }
    }

    #[test]
    fn test_no_identity_assign_cfg() {
         let cfg = create_test_cfg(vec![
            ("entry", vec![
                cnst("t0", 10),
                assign("t1", "t0"),
                binop("t2", "t1", Op::Add, "t0"),
                print("t2"),
                jump("exit"),
            ], vec![], vec!["exit"]),
            ("exit", vec![Instr::Ret{value: None}], vec!["entry"], vec![]),
        ]);
        let original_cfg = cfg.clone(); // Clone before optimize consumes it
        let pass = IdentityElimination;
        let optimized_cfg = pass.optimize(cfg);

        assert_eq!(optimized_cfg.blocks.len(), original_cfg.blocks.len());
         for (label, block) in &optimized_cfg.blocks {
             assert_eq!(block.instrs, original_cfg.blocks[label].instrs);
             assert_eq!(block.preds, original_cfg.blocks[label].preds);
             assert_eq!(block.succs, original_cfg.blocks[label].succs);
        }
    }

    #[test]
    fn test_multiple_identity_assigns_cfg() {
         let cfg = create_test_cfg(vec![
            ("start", vec![
                assign("a", "a"), // Remove
                cnst("b", 1),
                assign("c", "b"),
                assign("c", "c"), // Remove
                assign("b", "b"), // Remove
                print("c"),
                jump("end"),
            ], vec![], vec!["end"]),
            ("end", vec![Instr::Ret{value: None}], vec!["start"], vec![]),
        ]);
        let pass = IdentityElimination;
        let optimized_cfg = pass.optimize(cfg);

        let expected_cfg = create_test_cfg(vec![
             ("start", vec![
                // assign("a", "a") removed
                cnst("b", 1),
                assign("c", "b"),
                // assign("c", "c") removed
                // assign("b", "b") removed
                print("c"),
                jump("end"),
            ], vec![], vec!["end"]),
             ("end", vec![Instr::Ret{value: None}], vec!["start"], vec![]),
        ]);

        assert_eq!(optimized_cfg.blocks.len(), expected_cfg.blocks.len());
         for (label, block) in &optimized_cfg.blocks {
             assert_eq!(block.instrs, expected_cfg.blocks[label].instrs);
             assert_eq!(block.preds, expected_cfg.blocks[label].preds);
             assert_eq!(block.succs, expected_cfg.blocks[label].succs);
        }
    }

    #[test]
    fn test_empty_cfg() {
        let cfg = CFG { blocks: IndexMap::new(), current_block: None };
        let pass = IdentityElimination;
        let optimized_cfg = pass.optimize(cfg);
        let expected_cfg = CFG { blocks: IndexMap::new(), current_block: None };
        assert_eq!(optimized_cfg.blocks.len(), expected_cfg.blocks.len());
    }

     #[test]
    fn test_multiple_blocks_cfg() {
         let cfg = create_test_cfg(vec![
            ("entry", vec![cnst("x", 0), assign("x", "x"), jump("loop")], vec![], vec!["loop"]),
            ("loop", vec![assign("y", "x"), assign("y", "y"), print("y"), jump("exit")], vec!["entry"], vec!["exit"]),
            ("exit", vec![Instr::Ret{value: None}], vec!["loop"], vec![]),
        ]);
        let pass = IdentityElimination;
        let optimized_cfg = pass.optimize(cfg);

        let expected_cfg = create_test_cfg(vec![
            ("entry", vec![cnst("x", 0), jump("loop")], vec![], vec!["loop"]), // assign("x", "x") removed
            ("loop", vec![assign("y", "x"), print("y"), jump("exit")], vec!["entry"], vec!["exit"]), // assign("y", "y") removed
            ("exit", vec![Instr::Ret{value: None}], vec!["loop"], vec![]),
        ]);

        assert_eq!(optimized_cfg.blocks.len(), expected_cfg.blocks.len());
         for (label, block) in &optimized_cfg.blocks {
             assert_eq!(block.instrs, expected_cfg.blocks[label].instrs, "Block '{}' instructions mismatch", label);
             assert_eq!(block.preds, expected_cfg.blocks[label].preds, "Block '{}' predecessors mismatch", label);
             assert_eq!(block.succs, expected_cfg.blocks[label].succs, "Block '{}' successors mismatch", label);
        }
    }
}