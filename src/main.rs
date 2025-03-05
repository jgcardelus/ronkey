use std::io::Write;

use lexer::next_token;

pub mod lexer;
pub mod tokens;

enum ReplModes {
    Normal,
    Lexer,
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

    println!("#  {}  #", "#".repeat(max_len));
    println!("#  {}  #", empty_line);
    titles.iter().for_each(|title| {
        let title = title.to_string();
        let padding = " ".repeat(max_len - title.len());
        println!("#  {}{}  #", title, padding)
    });
    println!("#  {}  #", empty_line);
    println!("#  {}  #", "#".repeat(max_len));
}

fn main() {
    title(&[
        "Welcome to the Ronkey Programming Language!",
        "",
        ">> Let's get Rusty!",
    ]);
    println!();

    let mut mode = ReplModes::Normal;

    loop {
        let mut input = String::new();
        match mode {
            ReplModes::Normal => print!("ronkey> "),
            ReplModes::Lexer => print!("ronkey:lexer> "),
        }
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_line(&mut input).unwrap();

        let input = match input.as_str().trim() {
            ":lexer" => {
                mode = ReplModes::Lexer;
                continue;
            }
            ":normal" => {
                mode = ReplModes::Normal;
                continue;
            }
            ":help" => {
                println!("Available commands:");
                println!(":help - Isn't it obvious?");
                println!(":lexer - Lex the input");
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
                println!("Warning: Not implemented yet!\nType :help for more info");
                println!("{}", input);
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
        }
    }
}
