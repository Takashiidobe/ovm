use ovm::backend::Backend;
use ovm::backend::x86_64::Codegen;
use ovm::cli::Cli;
use ovm::frontend::parser::Parser;
use ovm::frontend::tokenizer::{Token, Tokenizer};
use ovm::optimizer::{Optimizer, SSA};

fn main() {
    let cli = Cli::parse();

    // Parse the input expression
    let tokens: Vec<Token> = Tokenizer::tokenize(&cli.expression).expect("failed to tokenize");

    let program = match Parser::parse(&tokens) {
        Ok(program) => program,
        Err(err) => {
            eprintln!("Error parsing expression: {}", err);
            std::process::exit(1);
        }
    };

    // select backend to use
    let backend: Box<dyn Backend> = match cli.arch.as_str() {
        "x86_64" => Box::new(Codegen),
        _ => panic!("Invalid backend provided"),
    };

    let mut ssa = SSA::default();
    let ssa_instrs = ssa.program_to_ir(&program.statements);
    let optimizer = Optimizer;
    let instrs = optimizer.run_all(ssa_instrs);

    // Generate assembly
    let asm = backend.generate_assembly(&instrs);

    // Print assembly
    println!("{}", asm);
}
