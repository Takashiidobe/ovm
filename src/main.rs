use ovm::backend::x86_64::X86_64Backend;
use ovm::cli::Cli;

fn main() {
    let cli = Cli::parse();
    let backend = X86_64Backend;

    // Generate assembly
    let asm = backend.generate_assembly(cli.value);

    // Print assembly
    println!("{}", asm);
}

