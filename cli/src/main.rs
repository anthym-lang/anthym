use anyhow::Result;
use clap::{Parser, Subcommand, ValueEnum};
use lib::fmt::format_text;
use lib::jit::{self, jit_text, BuildOptions, JitOptions, Options};
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
        /// Optimization level
        #[arg(short, long)]
        opt_level: Option<OptLevel>,
    },
    /// Build an executable
    Build {
        /// The file to build
        file: PathBuf,
        /// The output file
        #[arg(short = 'O', long)]
        output: PathBuf,
        /// Optimization level
        #[arg(short, long)]
        opt_level: Option<OptLevel>,
    },
}

#[derive(ValueEnum, Debug, Clone)]
enum OptLevel {
    None,
    Speed,
    SpeedAndSize,
}

impl From<OptLevel> for jit::OptLevel {
    fn from(val: OptLevel) -> Self {
        match val {
            OptLevel::None => jit::OptLevel::None,
            OptLevel::Speed => jit::OptLevel::Speed,
            OptLevel::SpeedAndSize => jit::OptLevel::SpeedAndSize,
        }
    }
}

fn main() -> Result<()> {
    pretty_env_logger::init();
    let cli = Cli::parse();

    match cli.cmd {
        Command::Fmt { file } => {
            let contents = read_to_string(&file)?;
            let formatted = format_text(&contents)?;
            write(file, formatted)?;
        }
        Command::Run { file, opt_level } => {
            let contents = read_to_string(file)?;
            jit_text(
                Options::Jit(JitOptions {
                    opt_level: opt_level.unwrap_or(OptLevel::None).into(),
                }),
                contents,
            )?;
        }
        Command::Build {
            file,
            output,
            opt_level,
        } => {
            let contents = read_to_string(file)?;
            jit_text(
                Options::Build(BuildOptions {
                    output,
                    opt_level: opt_level.unwrap_or(OptLevel::SpeedAndSize).into(),
                }),
                contents,
            )?;
        }
    }

    Ok(())
}
