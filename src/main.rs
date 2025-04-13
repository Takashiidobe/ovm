use ovm::backend::Backend;
use ovm::backend::arm::ARM;
use ovm::backend::x86_64::X86_64;
use ovm::cli::Cli;

fn main() {
    let cli = Cli::parse();

    let backend: Box<dyn Backend> = match cli.arch.as_str() {
        "x86_64" => Box::new(X86_64),
        "arm" => Box::new(ARM),
        _ => panic!("Invalid backend provided"),
    };

    // Generate assembly
    let asm = backend.generate_assembly(cli.value);

    // Print assembly
    println!("{}", asm);
}
