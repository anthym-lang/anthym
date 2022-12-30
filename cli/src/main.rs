use anyhow::Result;
use clap::{Parser, Subcommand};
use lib::fmt::format_text;
use lib::run::run_text;
use std::fs::{read_to_string, write};
use std::path::PathBuf;

#[derive(Parser)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    cmd: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Format some code
    Fmt {
        /// The file to format
        file: PathBuf,
    },
    /// Run some code
    Run {
        /// The file to run
        file: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.cmd {
        Command::Fmt { file } => {
            let contents = read_to_string(&file)?;
            let formatted = format_text(&contents)?;
            write(file, formatted)?;
        }
        Command::Run { file } => {
            let contents = read_to_string(file)?;
            run_text(&contents)?;
        }
    }

    Ok(())
}
