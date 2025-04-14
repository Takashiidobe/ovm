use ovm::backend::Backend;
use ovm::backend::x86_64::Codegen;
use ovm::cli::Cli;
use ovm::frontend::parser::Parser;
use ovm::frontend::token::Token;
use ovm::frontend::tokenizer::Tokenizer;
use ovm::optimizer::registers::RegisterAllocator as _;
use ovm::optimizer::registers::linear_scan::LinearScan;
use ovm::optimizer::{Optimizer, SSA};

fn main() {
    let cli = Cli::parse();

    let mut tokenizer = Tokenizer::new(cli.expression);

    let tokens: Vec<Token> = tokenizer.scan_tokens();

    let mut parser = Parser::new(tokens);

    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            eprintln!("Error parsing expression: {}", err);
            std::process::exit(1);
        }
    };

    // select backend to use
    let mut backend: Box<dyn Backend> = match cli.arch.as_str() {
        "x86_64" => Box::new(Codegen::default()),
        _ => panic!("Invalid backend provided"),
    };

    let mut ssa = SSA::default();
    let ssa_instrs = ssa.program_to_ir(&program);
    let optimizer = Optimizer;
    let instrs = optimizer.run_all(ssa_instrs);

    let register_allocator = LinearScan;
    let (allocated_instrs, reg_map) = register_allocator.allocate(&instrs);

    // Generate assembly
    let asm = backend.generate_assembly(&allocated_instrs, &reg_map);

    // Print assembly
    println!("{}", asm);
}
