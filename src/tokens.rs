use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    Illegal,
    Eof,

    // Identifiers & literals
    Ident,
    Int,

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
}

impl TokenType {
    /// Returns the token type given a literal.
    /// It only works for non symbol literals
    pub fn from_literal(s: &str) -> TokenType {
        match s {
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            _ => TokenType::Ident,
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            TokenType::Illegal => "ILLEGAL",
            TokenType::Eof => "EOF",

            TokenType::Ident => "IDENT",
            TokenType::Int => "INT",

            TokenType::Assign => "=",
            TokenType::Plus => "+",

            TokenType::Comma => ",",
            TokenType::Semicolon => ";",

            TokenType::Lparen => "(",
            TokenType::Rparen => ")",
            TokenType::Lbrace => "{",
            TokenType::Rbrace => "}",
            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",
        };

        write!(f, "{}", s)
    }
}

pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type,
            literal,
        }
    }
}
