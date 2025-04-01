use crate::{
    ast::{self, BlockStatement, BooleanLiteral, Expression, Identifier, IntegerLiteral},
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

enum InfixOption {
    Some(Expression),
    None(Expression),
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

    if !assert_peek_token_and_next(parser, TokenType::Ident) {
        return None;
    }

    let name = ast::Identifier {
        token: parser.current_token.clone(),
        value: parser.current_token.literal.clone(),
    };

    if !assert_peek_token_and_next(parser, TokenType::Assign) {
        return None;
    }

    next_token(parser);

    let expression = match parse_expression(parser, Precedence::Lowest) {
        Some(expression) => expression,
        None => return None,
    };

    if !assert_peek_token_and_next(parser, TokenType::Semicolon) {
        // WARN: In the book this if branch is different. Why should it be?
        return None;
    }

    Some(ast::LetStatement {
        token,
        name,
        value: expression,
    })
}

pub fn parse_return_statement(parser: &mut Parser) -> Option<ast::ReturnStatement> {
    let token = parser.current_token.clone();

    next_token(parser);

    let expression = match parse_expression(parser, Precedence::Lowest) {
        Some(expression) => expression,
        None => return None,
    };

    if !assert_peek_token_and_next(parser, TokenType::Semicolon) {
        // WARN: In the book this if branch is different. Why should it be?
        return None;
    }

    Some(ast::ReturnStatement {
        token,
        value: expression,
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
        next_token(parser);

        let infix_expression = match parse_infix(parser, left_expression) {
            InfixOption::Some(infix_expression) => infix_expression,
            InfixOption::None(left_expression) => return Some(left_expression),
        };

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
        TokenType::True => Some(parse_boolean_literal(parser)),
        TokenType::False => Some(parse_boolean_literal(parser)),
        TokenType::Lparen => parse_grouped_expression(parser),
        TokenType::If => parse_if_expression(parser),
        TokenType::Function => parse_function_literal(parser),
        _ => None,
    }
}

fn parse_infix(parser: &mut Parser, left: Expression) -> InfixOption {
    match parser.current_token.token_type {
        TokenType::Plus
        | TokenType::Minus
        | TokenType::Asterisk
        | TokenType::Slash
        | TokenType::Less
        | TokenType::Greater
        | TokenType::Equals
        | TokenType::NotEquals => InfixOption::Some(parse_infix_expression(parser, left)),
        TokenType::Lparen => parse_call_expression(parser, left),
        _ => InfixOption::None(left),
    }
}

fn parse_call_expression(parser: &mut Parser, left: Expression) -> InfixOption {
    let token = parser.current_token.clone();

    let arguments = match parse_call_arguments(parser) {
        Some(arguments) => arguments,
        None => return InfixOption::None(left),
    };

    InfixOption::Some(Expression::Call(ast::CallExpression {
        token,
        function: Box::new(left),
        arguments,
    }))
}

fn parse_call_arguments(parser: &mut Parser) -> Option<Vec<Expression>> {
    let mut arguments = vec![];

    if parser.peek_token.token_type == TokenType::Rparen {
        next_token(parser);
        return Some(arguments);
    }

    next_token(parser);

    match parse_expression(parser, Precedence::Lowest) {
        Some(expression) => arguments.push(expression),
        None => return None,
    }

    while parser.peek_token.token_type == TokenType::Comma {
        next_token(parser);
        next_token(parser);

        match parse_expression(parser, Precedence::Lowest) {
            Some(expression) => arguments.push(expression),
            None => return None,
        }
    }

    if !assert_peek_token_and_next(parser, TokenType::Rparen) {
        return None;
    }

    return Some(arguments);
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

fn parse_boolean_literal(parser: &mut Parser) -> Expression {
    return Expression::BooleanLiteral(BooleanLiteral {
        token: parser.current_token.clone(),
        value: match parser.current_token.token_type {
            TokenType::True => true,
            TokenType::False => false,
            _ => panic!(
                "Expected boolean literal, got: {}",
                parser.current_token.token_type
            ),
        },
    });
}

fn parse_grouped_expression(parser: &mut Parser) -> Option<Expression> {
    next_token(parser);

    let expression = parse_expression(parser, Precedence::Lowest);

    if !assert_peek_token_and_next(parser, TokenType::Rparen) {
        missing_right_paren_error(parser);
        return None;
    }

    expression
}

fn parse_if_expression(parser: &mut Parser) -> Option<Expression> {
    let current_token = parser.current_token.clone();

    if !assert_peek_token_and_next(parser, TokenType::Lparen) {
        return None;
    }

    next_token(parser);

    let condition = match parse_expression(parser, Precedence::Lowest) {
        Some(condition) => condition,
        None => {
            let message = format!(
                "expected expression, got: {}",
                parser.current_token.token_type
            );
            parser.errors.push(message);
            return None;
        }
    };

    if !assert_peek_token_and_next(parser, TokenType::Rparen) {
        return None;
    }

    if !assert_peek_token_and_next(parser, TokenType::Lbrace) {
        return None;
    }

    let consequence = parse_block_statement(parser);

    let alternative = if parser.peek_token.token_type == TokenType::Else {
        next_token(parser);

        if !assert_peek_token_and_next(parser, TokenType::Lbrace) {
            return None;
        }

        Some(parse_block_statement(parser))
    } else {
        None
    };

    Some(Expression::If(ast::IfExpression {
        token: current_token,
        condition: Box::new(condition),
        consequence: Box::new(consequence),
        alternative: Box::new(alternative),
    }))
}

fn parse_function_literal(parser: &mut Parser) -> Option<Expression> {
    let current_token = parser.current_token.clone();

    if !assert_peek_token_and_next(parser, TokenType::Lparen) {
        return None;
    }

    next_token(parser);

    let parameters = match parse_function_parameters(parser) {
        Some(parameters) => parameters,
        None => return None,
    };

    if !assert_peek_token_and_next(parser, TokenType::Lbrace) {
        return None;
    }

    let body = parse_block_statement(parser);

    Some(Expression::Function(ast::FunctionLiteral {
        token: current_token,
        parameters,
        body,
    }))
}

fn parse_function_parameters(parser: &mut Parser) -> Option<Vec<ast::Identifier>> {
    let mut parameters = vec![];

    if parser.current_token.token_type == TokenType::Rparen {
        return Some(parameters);
    }

    if parser.current_token.token_type == TokenType::Ident {
        parameters.push(Identifier {
            token: parser.current_token.clone(),
            value: parser.current_token.literal.clone(),
        });
    }

    while parser.peek_token.token_type == TokenType::Comma {
        next_token(parser);
        next_token(parser);

        if parser.current_token.token_type != TokenType::Ident {
            return None;
        }

        parameters.push(Identifier {
            token: parser.current_token.clone(),
            value: parser.current_token.literal.clone(),
        });
    }

    if !assert_peek_token_and_next(parser, TokenType::Rparen) {
        return None;
    }

    return Some(parameters);
}

fn parse_block_statement(parser: &mut Parser) -> BlockStatement {
    let current_token = parser.current_token.clone();
    let mut statements = vec![];

    next_token(parser);

    while parser.current_token.token_type != TokenType::Rbrace
        && parser.current_token.token_type != TokenType::Eof
    {
        if let Some(statement) = parse_statement(parser) {
            statements.push(statement);
        }

        next_token(parser);
    }

    if parser.current_token.token_type != TokenType::Rbrace {
        let message = format!(
            "expected right brace, got: {}. Did you forget to add '}}'?",
            parser.current_token.token_type
        );

        parser.errors.push(message);
    }

    BlockStatement {
        token: current_token,
        statements,
    }
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
        TokenType::Lparen => Precedence::Call,
        _ => Precedence::Lowest,
    }
}

fn assert_peek_token_and_next(parser: &mut Parser, token_type: TokenType) -> bool {
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

fn missing_right_paren_error(parser: &mut Parser) {
    let message = format!(
        "missing right paren, got: {}",
        parser.current_token.token_type
    );

    parser.errors.push(message);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        ast::{BlockStatement, Expression, Node, Statement},
        lexer,
    };

    #[test]
    fn test_let_statements() {
        let input = [
            ("let x = 5;", "x", 5),
            ("let y = 10;", "y", 10),
            ("let foobar = 123456;", "foobar", 123456),
        ];

        input
            .iter()
            .for_each(|(input, expected_identifier, expected_value)| {
                let mut lexer = lexer::new(input);
                let mut parser = new(&mut lexer);
                let program = parse_program(&mut parser);

                check_parser_errors(&parser);

                assert_eq!(
                    program.statements.len(),
                    1,
                    "program.statements does not contain 1 statements, got: {}",
                    program.statements.len()
                );

                let statement = &program.statements[0];

                test_let_statement(statement, expected_identifier, *expected_value);
            });
    }

    fn test_let_statement(statement: &Statement, expected_identifier: &str, expected_value: i64) {
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

        let value = match &statement.value {
            Expression::IntegerLiteral(integer_literal) => integer_literal.value,
            _ => panic!("Only expecting integer literals"),
        };

        assert_eq!(
            value, expected_value,
            "value is not {}, got {}",
            expected_value, value
        )
    }

    #[test]
    fn test_return_statements() {
        let input = [
            ("return 5;", 5),
            ("return 10;", 10),
            ("return 123456;", 123456),
        ];

        input.iter().for_each(|(input, expected_value)| {
            let mut lexer = lexer::new(input);
            let mut parser = new(&mut lexer);
            let program = parse_program(&mut parser);

            check_parser_errors(&parser);

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain 1 statements, got: {}",
                program.statements.len()
            );

            let statement = &program.statements[0];

            test_return_statement(statement, *expected_value);
        });
    }

    fn test_return_statement(statement: &Statement, expected_value: i64) {
        assert_eq!(
            statement.token_literal(),
            "return",
            "statement is not a return statement, got {}",
            statement.token_literal()
        );

        let return_statement = match statement {
            Statement::Return(return_statement) => return_statement,
            _ => panic!("Only expecting return statements"),
        };

        let value = match &return_statement.value {
            Expression::IntegerLiteral(integer_literal) => integer_literal.value,
            _ => panic!("Only expecting integer literals"),
        };

        assert_eq!(
            value, expected_value,
            "value is not {}, got {}",
            expected_value, value
        );
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
    fn test_boolean_literal_expression() {
        let input = [("true;", "true", true), ("false;", "false", false)];

        input
            .iter()
            .for_each(|(input, expected_token_literal, expected_value)| {
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

                let boolean = match statement.expression.as_ref() {
                    Some(Expression::BooleanLiteral(boolean)) => boolean,
                    _ => panic!("Only expecting boolean expressions"),
                };

                assert_eq!(
                    boolean.value, *expected_value,
                    "boolean is not {}, got {}",
                    expected_value, boolean.value
                );

                assert_eq!(
                    boolean.token_literal(),
                    *expected_token_literal,
                    "boolean is not {}, got {}",
                    expected_token_literal,
                    boolean.token_literal()
                );
            });
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

    #[test]
    fn test_operator_precedences() {
        let tests = [
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        tests.iter().for_each(|(test, expected)| {
            let mut lexer = lexer::new(test);
            let mut parser = new(&mut lexer);

            let program = parse_program(&mut parser);
            check_parser_errors(&parser);

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statement, got: {}",
                    program.statements.len()
                );
            }

            assert_eq!(
                program.statements[0].to_string(),
                *expected,
                "Expected: {}, got: {}",
                expected,
                program.statements[0]
            );
        });
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

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

        let if_expression = match statement.expression.as_ref() {
            Some(Expression::If(if_expression)) => if_expression,
            _ => panic!("Only expecting if expressions"),
        };

        test_infix_expression(&if_expression.condition, "x", "<", "y");

        let consequence = if_expression.consequence.as_ref();
        test_branch(consequence, "x");

        if let Some(expression) = &*if_expression.alternative {
            panic!(
                "Expected alternative to be None, got: {:?}",
                expression.token_literal()
            );
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

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

        let if_expression = match statement.expression.as_ref() {
            Some(Expression::If(if_expression)) => if_expression,
            _ => panic!("Only expecting if expressions"),
        };

        test_infix_expression(&if_expression.condition, "x", "<", "y");

        let consequence = if_expression.consequence.as_ref();
        test_branch(consequence, "x");

        let alternative = match if_expression.alternative.as_ref() {
            Some(alternative) => alternative,
            None => panic!("Expected alternative to be Some"),
        };
        test_branch(alternative, "y");
    }

    fn test_branch(expression: &BlockStatement, expected: &str) {
        if expression.statements.len() != 1 {
            panic!(
                "branch.statements does not contain 1 statement, got: {}",
                expression.statements.len()
            );
        }

        let branch_expression = match &expression.statements[0] {
            Statement::Expression(expression) => expression,
            _ => panic!("Only expecting expression statements"),
        };

        test_identifier_from_expression(
            &branch_expression
                .expression
                .as_ref()
                .expect("Expected expression to be Some"),
            expected,
        );
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }";

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

        let function = match statement.expression.as_ref() {
            Some(Expression::Function(function)) => function,
            _ => panic!("Only expecting function expressions"),
        };

        assert_eq!(
            function.parameters.len(),
            2,
            "function.parameters does not contain 2 parameters, got: {}",
            function.parameters.len()
        );

        test_identifier(&function.parameters[0], "x");
        test_identifier(&function.parameters[1], "y");

        assert_eq!(
            function.body.statements.len(),
            1,
            "function.body.statements does not contain 1 statement, got: {}",
            function.body.statements.len()
        );

        let body_expression = match &function.body.statements[0] {
            Statement::Expression(expression) => expression,
            _ => panic!("Only expecting expression statements"),
        };

        let body_expression = match &body_expression.expression {
            Some(body_expression) => body_expression,
            _ => panic!("Only expecting infix expressions"),
        };

        test_infix_expression(body_expression, "x", "+", "y");
    }

    #[test]
    fn test_function_parameters() {
        let input = [
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y) {};", vec!["x", "y"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        input.iter().for_each(|(input, expected)| {
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

            let function = match statement.expression.as_ref() {
                Some(Expression::Function(function)) => function,
                _ => panic!("Only expecting function expressions"),
            };

            assert_eq!(
                function.parameters.len(),
                expected.len(),
                "function.parameters does not contain {} parameters, got: {}",
                expected.len(),
                function.parameters.len()
            );

            expected.iter().enumerate().for_each(|(i, expected)| {
                test_identifier(&function.parameters[i], expected);
            });
        });
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, x * y, a + b);";

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

        let call = match statement.expression.as_ref() {
            Some(Expression::Call(call)) => call,
            _ => panic!("Only expecting call expressions"),
        };

        let call_function = match &*call.function {
            Expression::Identifier(identifier) => identifier,
            _ => panic!(
                "Expected identifier, got: {:?}",
                call.function.token_literal()
            ),
        };

        test_identifier(&call_function, "add");

        assert_eq!(
            call.arguments.len(),
            3,
            "call.arguments does not contain 3 arguments, got: {}",
            call.arguments.len()
        );

        test_integer_literal(&call.arguments[0], 1);
        test_infix_expression(&call.arguments[1], "x", "*", "y");
        test_infix_expression(&call.arguments[2], "a", "+", "b");
    }

    fn test_infix_expression(expression: &Expression, left: &str, operator: &str, right: &str) {
        let operator_expression = match expression {
            Expression::Infix(infix) => infix,
            _ => panic!(
                "Expected infix expression, got: {:?}",
                expression.token_literal()
            ),
        };

        assert_eq!(
            operator_expression.operator, *operator,
            "expected operator {}, got: {}",
            operator_expression.operator, operator
        );

        match operator_expression.left.as_ref() {
            Expression::Identifier(identifier) => test_identifier(&identifier, left),
            _ => panic!("Expected left to be identifier"),
        }

        match &*operator_expression.right {
            Some(Expression::Identifier(identifier)) => test_identifier(&identifier, right),
            None => panic!("Expected right to be Some"),
            _ => panic!("Expected right to be identifier"),
        }
    }

    fn test_identifier_from_expression(expression: &Expression, expected: &str) {
        let identifier = match expression {
            Expression::Identifier(identifier) => identifier,
            _ => panic!("Expected identifier, got: {:?}", expression.token_literal()),
        };

        test_identifier(identifier, expected);
    }

    fn test_identifier(identifier: &Identifier, expected: &str) {
        assert_eq!(
            identifier.value, expected,
            "Expected value to be {}, got {}",
            expected, identifier.value
        );

        assert_eq!(
            identifier.token_literal(),
            expected,
            "Expected token_literal to be {}, got {}",
            expected,
            identifier.token_literal()
        );
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
