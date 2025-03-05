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
    Minus,
    Bang,
    Slash,
    Asterisk,
    Less,
    Greater,

    // Double Operators
    Equals,
    NotEquals,
    LessEquals,
    GreaterEquals,
    Pipe,

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
    Return,
    True,
    False,
    If,
    Else,
}

impl TokenType {
    /// Returns the token type given a literal.
    /// It only works for non symbol literals
    pub fn from_literal(s: &str) -> TokenType {
        match s {
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            "return" => TokenType::Return,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "if" => TokenType::If,
            "else" => TokenType::Else,
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
            TokenType::Minus => "-",
            TokenType::Bang => "!",
            TokenType::Slash => "/",
            TokenType::Asterisk => "*",

            TokenType::Less => "<",
            TokenType::Greater => ">",

            TokenType::Equals => "==",
            TokenType::NotEquals => "!=",
            TokenType::LessEquals => "<=",
            TokenType::GreaterEquals => ">=",
            TokenType::Pipe => "|>",

            TokenType::Comma => ",",
            TokenType::Semicolon => ";",

            TokenType::Lparen => "(",
            TokenType::Rparen => ")",
            TokenType::Lbrace => "{",
            TokenType::Rbrace => "}",

            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",
            TokenType::Return => "RETURN",
            TokenType::True => "TRUE",
            TokenType::False => "FALSE",
            TokenType::If => "IF",
            TokenType::Else => "ELSE",
        };

        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
