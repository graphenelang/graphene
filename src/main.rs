use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::io::{Read, Result};
use unicode_normalization::UnicodeNormalization;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    file: Option<String>,
}

fn build_source(source: String) {
    let source = source.nfc().collect::<String>();
    let mut lexer = graphene::lexer::Lexer::new(&source);
    let result = lexer.tokenize();
    if let Err(err) = result {
        for err in err {
            println!("{}", err);
        }
    } else if let Ok(tokens) = result {
        println!("tokens: {:?}", tokens);
    }
}

fn build_file(file: String) {
    let source = std::fs::read_to_string(file);
    let source = match source {
        Ok(source) => source,
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    build_source(source);
}

fn repl() {
    let mut rl = DefaultEditor::new().expect("Failed to initialize readline");

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).unwrap();
                build_source(line);
            }
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                panic!("{}", err);
            }
        }
    }
}

fn main() {
    let cli = Cli::parse();
    if let Some(file) = cli.file {
        build_file(file);
    } else {
        repl();
    }
}
