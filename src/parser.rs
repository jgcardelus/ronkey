use crate::{
    ast::{self, Expression, Identifier, IntegerLiteral},
    lexer::{self, Lexer},
    tokens::{Token, TokenType},
};

enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    fn value(&self) -> i32 {
        match self {
            Precedence::Lowest => 0,
            Precedence::Equals => 1,
            Precedence::LessGreater => 2,
            Precedence::Sum => 3,
            Precedence::Product => 4,
            Precedence::Prefix => 5,
            Precedence::Call => 6,
        }
    }
}

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
        _ => parse_expression_statement(parser).map(ast::Statement::Expression),
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

pub fn parse_expression_statement(parser: &mut Parser) -> Option<ast::ExpressionStatement> {
    let token = parser.current_token.clone();
    let expression = parse_expression(parser, Precedence::Lowest);

    if parser.peek_token.token_type == TokenType::Semicolon {
        // NOTE: Should we expect a semicolon here?
        next_token(parser);
    }

    return Some(ast::ExpressionStatement { token, expression });
}

fn parse_expression(parser: &mut Parser, precedence: Precedence) -> Option<Expression> {
    // Check if it the current token has a prefix parser function
    let mut left_expression = match parse_prefix(parser) {
        Some(expression) => expression,
        None => {
            no_prefix_parsing_function_error(parser);
            return None;
        }
    };

    while parser.peek_token.token_type != TokenType::Semicolon
        && precedence.value() < peek_precedence(parser).value()
    {
        if !has_parse_infix_function(&parser.peek_token.token_type) {
            return Some(left_expression);
        }

        next_token(parser);
        let infix_expression = parse_infix_expression(parser, left_expression);
        left_expression = infix_expression;
    }

    return Some(left_expression);
}

fn parse_prefix(parser: &mut Parser) -> Option<Expression> {
    let current_token = &parser.current_token.token_type;
    match current_token {
        TokenType::Ident => Some(parse_identifier(parser)),
        TokenType::Int => Some(parse_integer_literal(parser)),
        TokenType::Bang => Some(parse_prefix_expression(parser)),
        TokenType::Minus => Some(parse_prefix_expression(parser)),
        _ => None,
    }
}

fn has_parse_infix_function(token_type: &TokenType) -> bool {
    match token_type {
        TokenType::Plus
        | TokenType::Minus
        | TokenType::Asterisk
        | TokenType::Slash
        | TokenType::Less
        | TokenType::Greater
        | TokenType::Equals
        | TokenType::NotEquals => true,
        _ => false,
    }
}

fn parse_infix_expression(parser: &mut Parser, left: Expression) -> Expression {
    let precendence = current_precedence(parser);
    let token = parser.current_token.clone();
    let operator = parser.current_token.literal.clone();
    next_token(parser);
    let right = parse_expression(parser, precendence);

    Expression::Infix(ast::InfixExpression {
        token,
        left: Box::new(left),
        operator,
        right: Box::new(right),
    })
}

fn parse_prefix_expression(parser: &mut Parser) -> Expression {
    let token = parser.current_token.clone();
    let operator = parser.current_token.literal.clone();

    next_token(parser);

    let right = parse_expression(parser, Precedence::Prefix);

    Expression::Prefix(ast::PrefixExpression {
        token,
        operator,
        right: Box::new(right),
    })
}

fn parse_identifier(parser: &mut Parser) -> Expression {
    return Expression::Identifier(Identifier {
        token: parser.current_token.clone(),
        value: parser.current_token.literal.clone(),
    });
}

fn parse_integer_literal(parser: &mut Parser) -> Expression {
    return Expression::IntegerLiteral(IntegerLiteral {
        token: parser.current_token.clone(),
        value: match parser.current_token.literal.parse::<i64>() {
            Ok(value) => value,
            Err(_) => panic!(
                "Could not parse {} as integer literal",
                parser.current_token.literal
            ),
        },
    });
}

fn peek_precedence(parser: &mut Parser) -> Precedence {
    precedences_lookup(&parser.peek_token.token_type)
}

fn current_precedence(parser: &mut Parser) -> Precedence {
    precedences_lookup(&parser.current_token.token_type)
}

fn precedences_lookup(token_type: &TokenType) -> Precedence {
    match token_type {
        TokenType::Equals => Precedence::Equals,
        TokenType::NotEquals => Precedence::Equals,
        TokenType::Less => Precedence::LessGreater,
        TokenType::Greater => Precedence::LessGreater,
        TokenType::Plus => Precedence::Sum,
        TokenType::Minus => Precedence::Sum,
        TokenType::Asterisk => Precedence::Product,
        TokenType::Slash => Precedence::Product,
        _ => Precedence::Lowest,
    }
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

fn no_prefix_parsing_function_error(parser: &mut Parser) {
    let message = format!(
        "no prefix parsing function for token type {}",
        parser.current_token.token_type
    );

    parser.errors.push(message);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        ast::{Expression, Node, Statement},
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
            .for_each(|(i, _value)| {
                let statement = &program.statements[i];
                test_return_statement(statement);
            });
    }

    fn test_return_statement(statement: &Statement) {
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

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let mut lexer = lexer::new(input);
        let mut parser = new(&mut lexer);

        let program = parse_program(&mut parser);
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statement, got: {}",
                program.statements.len()
            );
        }

        let statement = match &program.statements[0] {
            Statement::Expression(expression_statement) => expression_statement,
            _ => panic!("Only expecting expression statements"),
        };

        if let None = statement.expression {
            panic!("expression is None");
        }

        let expression = statement.expression.as_ref().unwrap();

        let identifier = match &expression {
            Expression::Identifier(identifier) => identifier,
            _ => panic!("Only expecting identifier expressions"),
        };

        assert_eq!(
            identifier.value, "foobar",
            "identifier is not foobar, got {}",
            identifier.value
        );

        assert_eq!(
            identifier.token_literal(),
            "foobar",
            "identifier is not foobar, got {}",
            identifier.token_literal()
        );
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "1;";

        let mut lexer = lexer::new(input);
        let mut parser = new(&mut lexer);

        let program = parse_program(&mut parser);
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statement, got: {}",
                program.statements.len()
            );
        }

        let statement = match &program.statements[0] {
            Statement::Expression(expression_statement) => expression_statement,
            _ => panic!("Only expecting expression statements"),
        };

        if let None = statement.expression {
            panic!("expression is None");
        }

        let expression = statement.expression.as_ref().unwrap();

        let integer = match &expression {
            Expression::IntegerLiteral(integer) => integer,
            _ => panic!("Only expecting integer expressions"),
        };

        assert_eq!(integer.value, 1, "integer is not 1, got {}", integer.value);

        assert_eq!(
            integer.token_literal(),
            "1",
            "integer is not 1, got {}",
            integer.token_literal()
        );
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = [("!5;", "!", 5), ("-15;", "-", 15)];

        prefix_tests
            .iter()
            .for_each(|(input, operator, expected_value)| {
                let mut lexer = lexer::new(input);
                let mut parser = new(&mut lexer);

                let program = parse_program(&mut parser);
                check_parser_errors(&parser);

                if program.statements.len() != 1 {
                    panic!(
                        "program.statements does not contain 1 statement, got: {}",
                        program.statements.len()
                    );
                }

                let statement = match &program.statements[0] {
                    Statement::Expression(expression_statement) => expression_statement,
                    other => panic!(
                        "Only expecting expression statements. got: {:?}",
                        other.token_literal()
                    ),
                };

                let prefix = match &statement.expression {
                    Some(Expression::Prefix(prefix)) => prefix,
                    None => panic!("Expected prefix expression, got: None"),
                    Some(expression) => panic!(
                        "Expected prefix expression, got: {:?}",
                        expression.token_literal()
                    ),
                };

                assert_eq!(
                    prefix.operator, *operator,
                    "Expected operator to be {}, got {}",
                    operator, prefix.operator
                );

                let right_expression = prefix
                    .right
                    .as_ref()
                    .as_ref()
                    .expect("Expected right to be Some");

                assert_eq!(
                    right_expression.token_literal(),
                    expected_value.to_string(),
                    "Expected right to be {}, got {}",
                    expected_value,
                    right_expression.token_literal()
                );

                test_integer_literal(&right_expression, *expected_value);
            });
    }

    #[test]
    fn test_infix_expressions() {
        let infix_tests = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        infix_tests
            .iter()
            .for_each(|(input, left_value, operator, right_value)| {
                let mut lexer = lexer::new(input);
                let mut parser = new(&mut lexer);

                let program = parse_program(&mut parser);
                check_parser_errors(&parser);

                if program.statements.len() != 1 {
                    panic!(
                        "program.statements does not contain 1 statement, got: {}",
                        program.statements.len()
                    );
                }

                let statement = match &program.statements[0] {
                    Statement::Expression(expression_statement) => expression_statement,
                    other => panic!(
                        "Only expecting expression statements. got: {:?}",
                        other.token_literal()
                    ),
                };

                let infix = match &statement.expression {
                    Some(Expression::Infix(infix)) => infix,
                    None => panic!("Expected infix expression, got: None"),
                    Some(expression) => panic!(
                        "Expected infix expression, got: {:?}",
                        expression.token_literal()
                    ),
                };

                assert_eq!(
                    infix.operator, *operator,
                    "Expected operator to be {}, got {}",
                    operator, infix.operator
                );

                let left_expression = infix.left.as_ref();

                assert_eq!(
                    left_expression.token_literal(),
                    left_value.to_string(),
                    "Expected left to be {}, got {}",
                    left_value,
                    left_expression.token_literal()
                );

                test_integer_literal(&left_expression, *left_value);

                let right_expression = infix
                    .right
                    .as_ref()
                    .as_ref()
                    .expect("Expected right to be Some");

                assert_eq!(
                    right_expression.token_literal(),
                    right_value.to_string(),
                    "Expected right to be {}, got {}",
                    right_value,
                    right_expression.token_literal()
                );

                test_integer_literal(&right_expression, *right_value);
            });
    }

    fn test_integer_literal(expression: &Expression, expected_value: i64) {
        let integer_literal = match expression {
            Expression::IntegerLiteral(integer_literal) => integer_literal,
            _ => panic!(
                "Expected integer literal, got: {:?}",
                expression.token_literal()
            ),
        };

        assert_eq!(
            integer_literal.value, expected_value,
            "Expected value to be {}, got {}",
            expected_value, integer_literal.value
        );
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
