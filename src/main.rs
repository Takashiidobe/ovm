use ovm::backend::Backend;
use ovm::backend::x86_64::X86_64;
use ovm::cli::Cli;
use ovm::frontend::parser::Parser;
use ovm::frontend::tokenizer::{Token, Tokenizer};

fn main() {
    let cli = Cli::parse();

    // Parse the input expression
    let tokens: Vec<Token> = Tokenizer::tokenize(&cli.expression).expect("failed to tokenize");

    let expr = match Parser::parse(&tokens) {
        Ok(expr) => expr,
        Err(err) => {
            eprintln!("Error parsing expression: {}", err);
            std::process::exit(1);
        }
    };

    let backend: Box<dyn Backend> = match cli.arch.as_str() {
        "x86_64" => Box::new(X86_64),
        _ => panic!("Invalid backend provided"),
    };

    // Generate assembly
    let asm = backend.generate_assembly(&expr);

    // Print assembly
    println!("{}", asm);
}
