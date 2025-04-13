# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build/Test Commands
- Build: `cargo build`
- Run: `cargo run -- [value]`
- Test: `cargo test`
- Run single test: `cargo test [test_name]`
- Format: `cargo fmt`
- Lint: `cargo clippy`

## Code Style Guidelines
- Use 4-space indentation
- Follow Rust naming conventions (snake_case for variables/functions, CamelCase for types)
- Group imports: std library first, then external crates, then local modules
- Handle errors with Result/Option types, avoid unwrap() in production code
- Use descriptive variable names and add comments for complex logic
- Format code with rustfmt before committing
- Use strong typing and avoid unnecessary type conversions
- Keep functions focused on a single responsibility