use crate::{
    ast,
    lexer::{self, Lexer},
    tokens::{Token, TokenType},
};

pub struct Parser<'l> {
    pub lexer: &'l mut Lexer<'l>,
    pub errors: Vec<String>,

    pub current_token: Token,
    pub peek_token: Token,
}

pub fn new<'l>(lexer: &'l mut Lexer<'l>) -> Parser<'l> {
    let current_token = lexer::next_token(lexer);
    let peek_token = lexer::next_token(lexer);

    Parser {
        lexer,
        errors: vec![],
        current_token,
        peek_token,
    }
}

pub fn next_token(parser: &mut Parser) {
    parser.current_token = parser.peek_token.clone();
    parser.peek_token = lexer::next_token(parser.lexer);
}

pub fn parse_program(parser: &mut Parser) -> ast::Program {
    let mut program = ast::Program { statements: vec![] };

    while parser.current_token.token_type != TokenType::Eof {
        let statement = parse_statement(parser);
        if let Some(statement) = statement {
            program.statements.push(statement);
        }
        next_token(parser);
    }

    program
}

pub fn parse_statement(parser: &mut Parser) -> Option<ast::Statement> {
    match parser.current_token.token_type {
        TokenType::Let => parse_let_statement(parser).map(ast::Statement::Let),
        TokenType::Return => parse_return_statement(parser).map(ast::Statement::Return),
        _ => None,
    }
}

pub fn parse_let_statement(parser: &mut Parser) -> Option<ast::LetStatement> {
    let token = parser.current_token.clone();

    if !assert_peek_token(parser, TokenType::Ident) {
        return None;
    }

    let name = ast::Identifier {
        token: parser.current_token.clone(),
        value: parser.current_token.literal.clone(),
    };

    if !assert_peek_token(parser, TokenType::Assign) {
        return None;
    }
    eat_expression(parser);

    Some(ast::LetStatement {
        token,
        name,
        value: ast::Expression::Empty,
    })
}

pub fn parse_return_statement(parser: &mut Parser) -> Option<ast::ReturnStatement> {
    let token = parser.current_token.clone();

    next_token(parser);
    eat_expression(parser);

    Some(ast::ReturnStatement {
        token,
        value: ast::Expression::Empty,
    })
}

/// This function is for prototyping only and shouldn't be used in production code.
#[deprecated(
    since = "0.1.0",
    note = "This function simply skips tokens without proper parsing. Use a proper expression parser instead."
)]
fn eat_expression(parser: &mut Parser) {
    while parser.current_token.token_type != TokenType::Semicolon
        && parser.current_token.token_type != TokenType::Eof
    {
        next_token(parser);
    }
}

fn assert_peek_token(parser: &mut Parser, token_type: TokenType) -> bool {
    if parser.peek_token.token_type == token_type {
        next_token(parser);
        return true;
    } else {
        peek_error(parser, token_type);
        return false;
    }
}

fn peek_error(parser: &mut Parser, token_type: TokenType) {
    let message = format!(
        "expected {}, got {}",
        token_type, parser.peek_token.token_type
    );

    parser.errors.push(message);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        ast::{Node, Statement},
        lexer,
    };

    #[test]
    fn test_let_statements() {
        let input = "
			let x 5;
			let y = 10;
			let 123456;
		";

        let mut lexer = lexer::new(input);
        let mut parser = new(&mut lexer);
        let program = parse_program(&mut parser);

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements, got: {}",
            program.statements.len()
        );

        ["x", "y", "foobar"]
            .iter()
            .enumerate()
            .for_each(|(i, name)| {
                let statement = &program.statements[i];
                test_let_statement(statement, name.to_string());
            });
    }

    fn test_let_statement(statement: &Statement, expected_identifier: String) {
        assert_eq!(
            statement.token_literal(),
            "let",
            "statement is not a let statement, got {}",
            statement.token_literal()
        );

        let statement = match statement {
            Statement::Let(let_statement) => let_statement,
            _ => panic!("Only expecting let statements"),
        };

        assert_eq!(
            statement.name.token_literal(),
            expected_identifier,
            "name is not {}, got {}",
            expected_identifier,
            statement.name.token_literal()
        );
    }

    #[test]
    fn test_return_statements() {
        let input = "
			return 5;
			return 10;
			return 123456;
		";

        let mut lexer = lexer::new(input);
        let mut parser = new(&mut lexer);
        let program = parse_program(&mut parser);

        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements, got: {}",
            program.statements.len()
        );

        ["5", "10", "123456"]
            .iter()
            .enumerate()
            .for_each(|(i, value)| {
                let statement = &program.statements[i];
                test_return_statement(statement, value.to_string());
            });
    }

    fn test_return_statement(statement: &Statement, expected_value: String) {
        assert_eq!(
            statement.token_literal(),
            "return",
            "statement is not a return statement, got {}",
            statement.token_literal()
        );

        match statement {
            Statement::Return(return_statement) => return_statement,
            _ => panic!("Only expecting return statements"),
        };
    }

    fn check_parser_errors(parser: &Parser) {
        assert_eq!(
            parser.errors.len(),
            0,
            "\n\nparser errors: \n\t-> {}",
            parser.errors.join("\n\t-> ")
        );
    }
}
