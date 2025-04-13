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

The CLI currently accepts an integer argument and generates x86_64 assembly that returns with that number.

Basic usage:

```bash
cargo run -- 42
```

## Project Structure

- `src/frontend/`: Language parser and IR generator
- `src/optimizer/`: SSA optimizer components
- `src/backend/`: Code generation for x86_64
