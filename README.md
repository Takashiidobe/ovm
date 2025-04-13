# OVM

Optimizing Virtual Machine - A simple compiler infrastructure inspired by LLVM.

## Overview

OVM is a simple programming language compiler with an SSA optimizer.

## Getting Started

### Prerequisites

- Rust and Cargo
- A C compiler, invoked as `cc`.

### Building

```bash
cargo build --release
```

### Usage

The CLI currently accepts a program of statements and generates x86_64 assembly that returns with that number.

Basic usage:

```bash
cargo run -- 'print(1+2);' > main.S
```

## Project Structure

- `src/frontend/`: Tokenizer and parser
- `src/optimizer/`: SSA optimizer and register allocation
- `src/backend/`: Code generation for x86_64
