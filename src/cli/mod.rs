use clap::Parser;

#[derive(Parser)]
#[command(name = "ovm")]
#[command(about = "A simple compiler that generates assembly from arithmetic expressions")]
pub struct Cli {
    /// The expression to compile (e.g., "5+20-4")
    #[arg(default_value = "0")]
    pub expression: String,

    #[clap(short, long, default_value = "x86_64")]
    pub arch: String,
}

impl Cli {
    pub fn parse() -> Self {
        Parser::parse()
    }
}