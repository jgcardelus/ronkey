use crate::{
    ast::{self, Expression, Program, Statement},
    object::{self, Jar},
    tokens,
};

const EVAL_NULL: object::NullObject = object::NullObject {};
const EVAL_TRUE: object::BooleanObject = object::BooleanObject { value: true };
const EVAL_FALSE: object::BooleanObject = object::BooleanObject { value: false };

pub fn eval_program(program: &Program) -> Jar {
    let mut jar = bind_null();

    for statement in &program.statements {
        jar = eval_statement(statement);

        match statement {
            Statement::Return(_) => return jar,
            _ => {}
        }
    }

    jar
}

fn eval_statement(statement: &Statement) -> Jar {
    match statement {
        Statement::Let(_let_stament) => todo!(),
        Statement::Return(_return_statement) => todo!(),
        Statement::Expression(expression) => match expression.expression.as_ref() {
            Some(expression) => eval_expression(expression),
            None => bind_null(),
        },
    }
}

fn eval_expression(expression: &Expression) -> Jar {
    match expression {
        Expression::IntegerLiteral(integer) => Jar::Integer(object::IntegerObject {
            value: integer.value,
        }),
        Expression::BooleanLiteral(boolean) => bind_boolean(boolean),
        Expression::Prefix(prefix_expression) => {
            let right = match prefix_expression.right.as_ref() {
                Some(right) => eval_expression(right),
                None => return bind_null(),
            };

            eval_prefix_expression(prefix_expression, &right)
        }
        _ => todo!(),
    }
}

fn eval_prefix_expression(prefix_expression: &ast::PrefixExpression, right: &Jar) -> Jar {
    match prefix_expression.token.token_type {
        tokens::TokenType::Bang => eval_bang_expression(right),
        _ => todo!(),
    }
}

fn eval_bang_expression(jar: &Jar) -> Jar {
    let boolean_object = match jar {
        Jar::Boolean(object) => object,
        _ => return Jar::Boolean(&EVAL_FALSE),
    };

    match boolean_object.value {
        true => Jar::Boolean(&EVAL_FALSE),
        false => Jar::Boolean(&EVAL_TRUE),
    }
}

fn bind_boolean(boolean: &ast::BooleanLiteral) -> Jar {
    match boolean.value {
        true => Jar::Boolean(&EVAL_TRUE),
        false => Jar::Boolean(&EVAL_FALSE),
    }
}

fn bind_null() -> Jar {
    Jar::Null(&EVAL_NULL)
}

#[cfg(test)]
mod test {
    use crate::{
        lexer,
        parser::{self, parse_program},
    };

    use super::*;

    #[test]
    fn test_eval_intenger_expression() {
        let inputs = [("5", 5), ("10", 10)];

        inputs.iter().for_each(|(input, expected)| {
            let jar = test_eval(input);
            test_integer_object(&jar, *expected);
        });
    }

    #[test]
    fn test_eval_boolean_expression() {
        let inputs = [("true", true), ("false", false)];

        inputs.iter().for_each(|(input, expected)| {
            let jar = test_eval(input);
            test_boolean_object(&jar, *expected);
        });
    }

    #[test]
    fn test_bang_operator() {
        let inputs = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        inputs.iter().for_each(|(input, expected)| {
            let jar = test_eval(input);
            test_boolean_object(&jar, *expected);
        });
    }

    fn test_eval(input: &str) -> Jar {
        let mut lexer = lexer::new(input);
        let mut parser = parser::new(&mut lexer);
        let program = parse_program(&mut parser);
        eval_program(&program)
    }

    fn test_integer_object(jar: &Jar, expected: i64) {
        let integer_object = match jar {
            Jar::Integer(object) => object,
            _ => panic!("Expected Jar::Integer"),
        };

        assert_eq!(
            integer_object.value, expected,
            "Expected {}, got {}",
            expected, integer_object.value
        );
    }

    fn test_boolean_object(jar: &Jar, expected: bool) {
        let boolean_object = match jar {
            Jar::Boolean(object) => object,
            _ => panic!("Expected Jar::Boolean"),
        };

        assert_eq!(
            boolean_object.value, expected,
            "Expected {}, got {}",
            expected, boolean_object.value
        );
    }
}
