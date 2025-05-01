use std::collections::HashSet;

use crate::optimizer::{BasicBlock, CFG, CmpOp, Instr, Op};
use indexmap::IndexMap;
// Removed unused HashSet import

// Helper to create a simple CFG for testing
pub fn create_test_cfg(blocks_data: Vec<(&str, Vec<Instr>, Vec<&str>, Vec<&str>)>) -> CFG {
    let mut blocks = IndexMap::new();
    let mut entry_label: Option<String> = None; // Track the first block as entry

    for (i, (label, instrs, preds, succs)) in blocks_data.into_iter().enumerate() {
        if i == 0 {
            entry_label = Some(label.to_string()); // Assume first block is entry
        }
        blocks.insert(
            label.to_string(),
            BasicBlock {
                label: label.to_string(),
                instrs,
                preds: preds
                    .into_iter()
                    .map(|s| s.to_string())
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .collect(),
                succs: succs
                    .into_iter()
                    .map(|s| s.to_string())
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .collect(),
            },
        );
    }
    // Basic validation: Ensure all listed predecessors/successors exist as block labels
    // More robust validation could be added if needed.
    // for block in blocks.values() {
    //     for pred in &block.predecessors {
    //         assert!(blocks.contains_key(pred), "Predecessor '{}' not found for block '{}'", pred, block.label);
    //     }
    //     for succ in &block.successors {
    //         assert!(blocks.contains_key(succ), "Successor '{}' not found for block '{}'", succ, block.label);
    //     }
    // }
    CFG {
        blocks,
        current_block: entry_label,
    } // Set entry point
}

pub fn cnst(dest: &str, val: i64) -> Instr {
    Instr::Const(dest.to_string(), val)
}
pub fn branch(cond: &str, t: &str, f: &str) -> Instr {
    Instr::BranchIf(cond.to_string(), t.to_string(), f.to_string())
}
pub fn jump(target: &str) -> Instr {
    Instr::Jump(target.to_string())
}
pub fn ret() -> Instr {
    Instr::Ret { value: None }
}
pub fn ret_val(val: &str) -> Instr {
    Instr::Ret {
        value: Some(val.to_string()),
    }
} // Added Ret with value helper
pub fn print(s: &str) -> Instr {
    Instr::Print(s.to_string())
}
pub fn assign(d: &str, s: &str) -> Instr {
    Instr::Assign(d.to_string(), s.to_string())
}
pub fn binop(dest: &str, l: &str, op: Op, r: &str) -> Instr {
    Instr::BinOp(dest.to_string(), l.to_string(), op, r.to_string())
}
pub fn cmp(dest: &str, l: &str, op: CmpOp, r: &str) -> Instr {
    Instr::Cmp(dest.to_string(), l.to_string(), op, r.to_string())
}
pub fn phi(dest: &str, preds: Vec<(&str, &str)>) -> Instr {
    Instr::Phi(
        dest.to_string(),
        preds
            .into_iter()
            .map(|(l, v)| (l.to_string(), v.to_string()))
            .collect(),
    )
}
pub fn func_param(dest: &str, index: usize) -> Instr {
    Instr::FuncParam {
        name: dest.to_string(),
        index,
    }
}
// Added Call helper
pub fn call(target: &str, args: Vec<&str>, result: Option<&str>) -> Instr {
    Instr::Call {
        target: target.to_string(),
        args: args.into_iter().map(|s| s.to_string()).collect(),
        result: result.map(|s| s.to_string()),
    }
}
