# OVM

Optimizing Virtual Machine - An optimizing compiler inspired by LLVM.

## Overview

OVM is a programming language compiler with an SSA optimizer and x86_64 code generator.
It supports a few language features and implements compiler optimizations.

## Getting Started

### Prerequisites

- Rust
- An x86_64 assembler.

### Building

```bash
cargo build --release
```

### Usage

The CLI accepts a program and generates x86_64 assembly.

Basic usage:

```bash
# Simple example
cargo run -- 'print 1+2 ;'

# More complex example with variables and control flow
cargo run -- 'var x = 10; if (x > 5) { x = x * 2; } else { x = x - 1; } print x;'
```

#### Example Program and Generated Assembly

Source program:
```
var x = 10;
if (x > 5) {
  x = x * 2;
} else {
  x = x - 1;
}
print(x);
```

Generated SSA Instructions:
```
Const("t0", 10)
Assign("x_1", "t0")
Const("t1", 5)
Cmp("t2", "x_1", Gt, "t1")
BranchIf("t2", "then_3", "else_4")
Label("then_3")
Const("t6", 2)
BinOp("t7", "x_1", Mul, "t6")
Assign("x_2", "t7")
Jump("merge_5")
Label("else_4")
Const("t8", 1)
BinOp("t9", "x_1", Sub, "t8")
Assign("x_3", "t9")
Jump("merge_5")
Label("merge_5")
Phi("t11", "x_2", "x_3")
Print("t11")
```

Generated x86_64 Assembly:
```asm
.section .data
  fmt: .string "%ld\n"
.section .bss
.section .text
.globl main
main:
  movq $10, %r15       # Initialize x to 10
  movq %r15, %r14
  movq $5, %r15
  cmpq %r15, %r14      # Compare x > 5
  jg then_3            # Jump if greater
  jmp else_4
then_3:
  movq $2, %r13
  movq %r14, %rax      # x * 2
  imulq %r13, %rax
  movq %rax, %r15
  movq %r15, %r13
  jmp merge_5
else_4:
  movq $1, %r15
  movq %r14, %rax      # x - 1
  subq %r15, %rax
  movq %rax, %r12
  movq %r12, %r15
  jmp merge_5
merge_5:
  movq %r12, %rsi      # Print the result
  leaq fmt(%rip), %rdi
  xor %rax, %rax
  call printf
  movl $0, %eax
  ret
```

## Language Features

- Data types: integers
- Arithmetic operations: `+`, `-`, `*`, `/`
- Comparison operations: `<`, `<=`, `>`, `>=`, `==`, `!=`
- Logical operations: `and`, `or`
- Bitwise operations: `&`, `|`
- Variable declarations and assignments
- Control flow: if/else,

## Optimizations

- Static Single Assignment (SSA) form
  - Supports if/else control flow with phi nodes
  - Handles variable declarations and assignments
- Constant folding for compile-time evaluation
- Dead code elimination
- Register allocation via linear scan algorithm

## Project Structure

- `src/frontend/`: Tokenizer and parser
- `src/optimizer/`: SSA optimizer and register allocation
- `src/backend/`: Code generation for x86_64
- `tests/`: Test cases for the compiler
