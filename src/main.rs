use ronkey::{
    environment, eval,
    lexer::{self, next_token},
    parser::{self, parse_program},
    tokens,
};
use std::{io::Write, sync};

enum ReplModes {
    Normal,
    Lexer,
    Parser,
}

fn title(titles: &[&str]) {
    let max_len = titles.iter().fold(0, |max_len, title| {
        let title_len = title.len();
        if title_len > max_len {
            title_len
        } else {
            max_len
        }
    });

    let empty_line = " ".repeat(max_len);

    println!("###{}###", "#".repeat(max_len));
    println!("#  {}  #", empty_line);
    titles.iter().for_each(|title| {
        let title = title.to_string();
        let padding = " ".repeat(max_len - title.len());
        println!("#  {}{}  #", title, padding)
    });
    println!("#  {}  #", empty_line);
    println!("###{}###", "#".repeat(max_len));
}

fn main() {
    title(&[
        "Welcome to the Ronkey Programming Language!",
        "",
        ">> Let's get Rusty!",
    ]);

    let mut mode = ReplModes::Normal;
    let environment = sync::Arc::new(sync::Mutex::new(environment::Environment::new()));

    loop {
        let mut input = String::new();
        match mode {
            ReplModes::Normal => print!("ronkey> "),
            ReplModes::Lexer => print!("ronkey:lexer> "),
            ReplModes::Parser => print!("ronkey:parser> "),
        }
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();

        let input = match input.as_str().trim() {
            ":lexer" => {
                mode = ReplModes::Lexer;
                continue;
            }
            ":parser" => {
                mode = ReplModes::Parser;
                continue;
            }
            ":normal" => {
                mode = ReplModes::Normal;
                continue;
            }
            ":alba" => {
                println!("💐");
                continue;
            }
            ":help" => {
                println!("Available commands:");
                println!(":help - Isn't it obvious?");
                println!(":lexer - Lex the input");
                println!(":parser - Lex the input");
                println!(":quit - Quit the REPL");
                continue;
            }
            ":quit" => {
                println!("Bye!");
                break;
            }
            input => input,
        };

        match mode {
            ReplModes::Normal => {
                let mut lexer = lexer::new(input);
                let mut parser = parser::new(&mut lexer);

                let program = parse_program(&mut parser);

                if parser.errors.len() > 0 {
                    print_parser_errors(&parser);
                    continue;
                }

                let jar = eval::eval_program(&program, environment.clone());
                println!("{}", jar);
            }
            ReplModes::Lexer => {
                let mut lexer = lexer::new(input);
                let mut token = next_token(&mut lexer);
                while token.token_type != tokens::TokenType::Eof
                    && token.token_type != tokens::TokenType::Illegal
                {
                    println!("{:#?}", token);
                    token = next_token(&mut lexer);
                }
            }
            ReplModes::Parser => {
                let mut lexer = lexer::new(input);
                let mut parser = parser::new(&mut lexer);
                let program = parse_program(&mut parser);

                match parser.errors.len() > 0 {
                    true => {
                        print_parser_errors(&parser);
                    }
                    false => {
                        println!("{}", program.to_string());
                    }
                }
            }
        }
    }
}

fn print_parser_errors(parser: &parser::Parser) {
    eprintln!("There was some monkey business going on: ");
    parser.errors.iter().for_each(|error| {
        eprintln!("\tError: {}", error);
    });
}
