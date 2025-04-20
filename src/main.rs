use ovm::backend::Backend;
use ovm::backend::x86_64::Codegen;
use ovm::cli::Cli;
use ovm::frontend::parser::Parser;
use ovm::frontend::token::Token;
use ovm::frontend::tokenizer::Tokenizer;
use ovm::optimizer::SSA;
use ovm::optimizer::passes::Optimizer;
use ovm::optimizer::registers::RegisterAllocator as _;
use ovm::optimizer::registers::linear_scan::LinearScan;

fn main() {
    let cli = Cli::parse();

    let input = if cli.expression.ends_with(".ovm") {
        std::fs::read_to_string(&cli.expression).unwrap()
    } else {
        cli.expression.clone()
    };

    let mut tokenizer = Tokenizer::new(input);

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
    let optimized_instrs = optimizer.run_all(ssa_instrs);

    let allocator = LinearScan;
    let (allocated_instrs, reg_map) = allocator.allocate(&optimized_instrs);

    // Generate assembly
    let asm = backend.generate_assembly(&allocated_instrs, &reg_map);

    // Print assembly
    println!("{}", asm);
}
