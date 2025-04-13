use clap::Parser;

#[derive(Parser)]
#[command(name = "ovm")]
#[command(about = "A simple compiler that generates assembly for the specified integer value")]
pub struct Cli {
    /// The integer value to return
    #[arg(default_value_t = 0)]
    pub value: i64,

    #[clap(short, long, default_value = "x86_64")]
    pub arch: String,
}

impl Cli {
    pub fn parse() -> Self {
        Parser::parse()
    }
}
