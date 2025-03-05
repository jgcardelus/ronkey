use crate::tokens::{Token, TokenType};

struct Lexer<'str> {
    input: &'str str,
    position: i32,
    read_position: i32,
    char: u8,
}

fn new<'str>(input: &'str str) -> Lexer<'str> {
    let mut lexer = Lexer {
        input,
        position: 0,
        read_position: 0,
        char: 0,
    };
    read_char(&mut lexer);

    lexer
}

fn next_token(lexer: &mut Lexer) -> Token {
    eat_nam_nam_whitespace(lexer);

    let token = match lexer.char {
        b'=' => Token::new(TokenType::Assign, String::from("=")),
        b'+' => Token::new(TokenType::Plus, String::from("+")),
        b'(' => Token::new(TokenType::Lparen, String::from("(")),
        b')' => Token::new(TokenType::Rparen, String::from(")")),
        b'{' => Token::new(TokenType::Lbrace, String::from("{")),
        b'}' => Token::new(TokenType::Rbrace, String::from("}")),
        b',' => Token::new(TokenType::Comma, String::from(",")),
        b';' => Token::new(TokenType::Semicolon, String::from(";")),
        0 => Token::new(TokenType::Eof, String::from("")),
        current_char => {
            if is_letter(current_char) {
                let literal = read_identifier(lexer);
                let token_type = TokenType::from_literal(&literal);

                return Token::new(token_type, literal);
            } else if is_digit(current_char) {
                let literal = read_digits(lexer);
                return Token::new(TokenType::Int, literal);
            } else {
                Token::new(TokenType::Illegal, String::from(current_char as char))
            }
        }
    };

    read_char(lexer);

    token
}

fn read_char(lexer: &mut Lexer) {
    if lexer.read_position >= lexer.input.len() as i32 {
        lexer.char = 0;
    } else {
        lexer.char = lexer.input.as_bytes()[lexer.read_position as usize];
    }
    lexer.position = lexer.read_position;
    lexer.read_position += 1;
}

fn read_identifier(lexer: &mut Lexer) -> String {
    let mut identifier = String::new();

    while is_letter(lexer.char) {
        identifier.push(lexer.char as char);
        read_char(lexer);
    }

    identifier
}

fn read_digits(lexer: &mut Lexer) -> String {
    let mut digits = String::new();

    while is_digit(lexer.char) {
        digits.push(lexer.char as char);
        read_char(lexer);
    }

    digits
}

fn eat_nam_nam_whitespace(lexer: &mut Lexer) {
    while is_whitespace(lexer.char) {
        read_char(lexer);
    }
}

fn is_letter(current_char: u8) -> bool {
    return b'a' <= current_char && current_char <= b'z'
        || b'A' <= current_char && current_char <= b'Z'
        || current_char == b'_'
        || current_char == b'?';
}

fn is_digit(current_char: u8) -> bool {
    current_char >= b'0' && current_char <= b'9'
}

fn is_whitespace(current_char: u8) -> bool {
    current_char == b' ' || current_char == b'\t' || current_char == b'\n' || current_char == b'\r'
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tokens::*;

    #[test]
    fn test_next_token() {
        let input = "
        	let five = 5;
         	let ten = 10;

          	let add = fn (x, y) {
           		x + y;
           	};

            let result = add(five, ten);
        ";

        let tests = [
            // let five = 5;
            Token::new(TokenType::Let, String::from("let")),
            Token::new(TokenType::Ident, String::from("five")),
            Token::new(TokenType::Assign, String::from("=")),
            Token::new(TokenType::Int, String::from("5")),
            Token::new(TokenType::Semicolon, String::from(";")),
            // let ten = 5;
            Token::new(TokenType::Let, String::from("let")),
            Token::new(TokenType::Ident, String::from("ten")),
            Token::new(TokenType::Assign, String::from("=")),
            Token::new(TokenType::Int, String::from("10")),
            Token::new(TokenType::Semicolon, String::from(";")),
            // let add = fn (x, y) { x + y; };
            Token::new(TokenType::Let, String::from("let")),
            Token::new(TokenType::Ident, String::from("add")),
            Token::new(TokenType::Assign, String::from("=")),
            Token::new(TokenType::Function, String::from("fn")),
            Token::new(TokenType::Lparen, String::from("(")),
            Token::new(TokenType::Ident, String::from("x")),
            Token::new(TokenType::Comma, String::from(",")),
            Token::new(TokenType::Ident, String::from("y")),
            Token::new(TokenType::Rparen, String::from(")")),
            Token::new(TokenType::Lbrace, String::from("{")),
            Token::new(TokenType::Ident, String::from("x")),
            Token::new(TokenType::Plus, String::from("+")),
            Token::new(TokenType::Ident, String::from("y")),
            Token::new(TokenType::Semicolon, String::from(";")),
            Token::new(TokenType::Rbrace, String::from("}")),
            Token::new(TokenType::Semicolon, String::from(";")),
            // let result = add(five, ten);
            Token::new(TokenType::Let, String::from("let")),
            Token::new(TokenType::Ident, String::from("result")),
            Token::new(TokenType::Assign, String::from("=")),
            Token::new(TokenType::Ident, String::from("add")),
            Token::new(TokenType::Lparen, String::from("(")),
            Token::new(TokenType::Ident, String::from("five")),
            Token::new(TokenType::Comma, String::from(",")),
            Token::new(TokenType::Ident, String::from("ten")),
            Token::new(TokenType::Rparen, String::from(")")),
            Token::new(TokenType::Semicolon, String::from(";")),
            // EOF
            Token::new(TokenType::Eof, String::from("")),
        ];

        let mut lexer = new(input);

        tests.iter().enumerate().for_each(|(i, expected)| {
            let token = next_token(&mut lexer);

            assert_eq!(
                token.token_type, expected.token_type,
                "tests[{}] -- tokentype wrong. expected: {}, got: {}",
                i, expected.token_type, token.token_type
            );

            assert_eq!(
                token.literal, expected.literal,
                "tests[{}] -- literal wrong. expected: {}, got: {}",
                i, expected.literal, token.literal,
            );
        });

        read_char(&mut lexer);
        assert_eq!(
            lexer.char, 0,
            "there are more chars to be read. check that the tests include all chars"
        );
    }
}
