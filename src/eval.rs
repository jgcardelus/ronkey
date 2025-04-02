use crate::{
    ast::{self, BlockStatement, Expression, Program, Statement},
    environment::EnvironmentRef,
    jar::{
        self, BooleanObject, FunctionObject, IntegerObject, Jar, Object, PanicObject, RuntimeError,
    },
    tokens,
};

const EVAL_NULL: jar::NullObject = jar::NullObject {};
const EVAL_TRUE: jar::BooleanObject = jar::BooleanObject { value: true };
const EVAL_FALSE: jar::BooleanObject = jar::BooleanObject { value: false };

pub fn eval_program(program: &Program, environment: EnvironmentRef) -> Jar {
    let mut jar = bind_null();

    for statement in &program.statements {
        jar = eval_statement(statement, environment.clone());

        match jar {
            Jar::Return(value) => return *value,
            Jar::Panic(_) => return jar,
            _ => {}
        }
    }

    jar
}

fn eval_statement(statement: &Statement, environment: EnvironmentRef) -> Jar {
    match statement {
        Statement::Let(let_statement) => {
            let value = eval_expression(&let_statement.value, environment.clone());

            if is_panic(&value) {
                return value;
            }

            environment
                .lock()
                .unwrap()
                .set(&let_statement.name.value, value);

            bind_null()
        }
        Statement::Return(return_statement) => {
            let return_value = eval_expression(&return_statement.value, environment);
            Jar::Return(Box::new(return_value))
        }
        Statement::Expression(expression) => match expression.expression.as_ref() {
            Some(expression) => eval_expression(expression, environment),
            None => bind_null(),
        },
    }
}

fn eval_expression(expression: &Expression, environment: EnvironmentRef) -> Jar {
    match expression {
        Expression::IntegerLiteral(integer) => Jar::Integer(jar::IntegerObject {
            value: integer.value,
        }),
        Expression::BooleanLiteral(boolean) => bind_boolean(boolean),
        Expression::Prefix(prefix_expression) => {
            let right = match prefix_expression.right.as_ref() {
                Some(right) => eval_expression(right, environment),
                None => return bind_null(),
            };
            if is_panic(&right) {
                return right;
            }

            eval_prefix_expression(prefix_expression, &right)
        }
        Expression::Infix(infix_expression) => {
            let right = match infix_expression.right.as_ref() {
                Some(right) => right,
                None => return bind_null(),
            };

            let right = eval_expression(right, environment.clone());
            if is_panic(&right) {
                return right;
            }

            let left = eval_expression(infix_expression.left.as_ref(), environment.clone());
            if is_panic(&left) {
                return left;
            }

            eval_infix_expression(&infix_expression.operator, &left, &right)
        }
        Expression::If(if_expression) => eval_if_expression(if_expression, environment),
        Expression::Identifier(identifier) => eval_identifier_expression(identifier, environment),
        Expression::Function(function_expression) => Jar::Function(FunctionObject {
            parameters: function_expression.parameters.clone(),
            body: function_expression.body.clone(),
            environment,
        }),
        Expression::Call(call_expression) => {
            let function = eval_expression(&call_expression.function, environment.clone());
            if is_panic(&function) {
                return function;
            }

            let arguments = eval_expressions(&call_expression.arguments, environment.clone());
            eval_apply_function(&function, arguments)
        }
    }
}

fn eval_expressions(expressions: &[Expression], environment: EnvironmentRef) -> Vec<Jar> {
    let mut jars = Vec::new();

    for expression in expressions {
        let value = eval_expression(expression, environment.clone());

        if is_panic(&value) {
            return vec![value];
        }

        jars.push(value);
    }

    jars
}

fn eval_apply_function(function: &Jar, arguments: Vec<Jar>) -> Jar {
    let function = match function {
        Jar::Function(object) => object,
        jar => return panic_program(RuntimeError::NotAFunction(jar.to_string())),
    };

    let extended_environment = extend_function_environment(function, arguments);
    let return_value = eval_block_statement(&function.body, extended_environment);

    return unwrap_return_value(return_value);
}

fn extend_function_environment(function: &FunctionObject, arguments: Vec<Jar>) -> EnvironmentRef {
    let environment = function.environment.clone();

    for (i, argument) in arguments.iter().enumerate() {
        environment
            .lock()
            .unwrap()
            .set(&function.parameters[i].value, argument.clone());
    }

    environment
}

fn eval_block_statement(block_statement: &BlockStatement, environment: EnvironmentRef) -> Jar {
    let mut jar = bind_null();

    for statement in &block_statement.statements {
        jar = eval_statement(statement, environment.clone());

        match jar {
            Jar::Return(_) => return jar,
            Jar::Panic(_) => return jar,
            _ => {}
        };
    }

    jar
}

fn eval_identifier_expression(identifier: &ast::Identifier, environment: EnvironmentRef) -> Jar {
    match environment.lock().unwrap().get(&identifier.value) {
        Some(jar) => jar.as_ref().clone(),
        None => panic_program(RuntimeError::IdentifierNotFound(identifier.value.clone())),
    }
}

fn eval_if_expression(if_expression: &ast::IfExpression, environment: EnvironmentRef) -> Jar {
    let condition = eval_expression(&if_expression.condition, environment.clone());
    if is_panic(&condition) {
        return condition;
    }

    if is_condition_truthy(&condition) {
        return eval_block_statement(&if_expression.consequence, environment);
    }

    if let Some(alternative) = if_expression.alternative.as_ref().as_ref() {
        return eval_block_statement(alternative, environment);
    }

    bind_null()
}

fn eval_infix_expression(operator: &String, left: &Jar, right: &Jar) -> Jar {
    if left.object_type() != right.object_type() {
        return panic_program(RuntimeError::TypeMismatch(
            left.object_type().to_string(),
            right.object_type().to_string(),
        ));
    }

    match (left, right) {
        (Jar::Integer(left), Jar::Integer(right)) => {
            eval_integer_infix_expression(operator, left, right)
        }
        (Jar::Boolean(left), Jar::Boolean(right)) => {
            eval_boolean_infix_expression(operator, left, right)
        }
        _ => panic_program(RuntimeError::InfixUnknownOperator(
            left.object_type().to_string(),
            operator.to_string(),
            right.object_type().to_string(),
        )),
    }
}

fn eval_boolean_infix_expression(
    operator: &String,
    left: &BooleanObject,
    right: &BooleanObject,
) -> Jar {
    match operator.as_ref() {
        "==" => bind_wrap_bool(left.value == right.value),
        "!=" => bind_wrap_bool(left.value != right.value),
        _ => panic_program(RuntimeError::InfixUnknownOperator(
            left.object_type().to_string(),
            operator.clone(),
            right.object_type().to_string(),
        )),
    }
}

fn eval_integer_infix_expression(
    operator: &String,
    left: &IntegerObject,
    right: &IntegerObject,
) -> Jar {
    match operator.as_ref() {
        "+" => Jar::Integer(IntegerObject {
            value: left.value + right.value,
        }),
        "-" => Jar::Integer(IntegerObject {
            value: left.value - right.value,
        }),
        "*" => Jar::Integer(IntegerObject {
            value: left.value * right.value,
        }),
        "/" => Jar::Integer(IntegerObject {
            value: left.value / right.value,
        }),
        "<" => bind_wrap_bool(left.value < right.value),
        ">" => bind_wrap_bool(left.value > right.value),
        ">=" => bind_wrap_bool(left.value >= right.value),
        "<=" => bind_wrap_bool(left.value <= right.value),
        "==" => bind_wrap_bool(left.value == right.value),
        "!=" => bind_wrap_bool(left.value != right.value),
        _ => panic_program(RuntimeError::InfixUnknownOperator(
            left.object_type().to_string(),
            operator.clone(),
            right.object_type().to_string(),
        )),
    }
}

fn eval_prefix_expression(prefix_expression: &ast::PrefixExpression, right: &Jar) -> Jar {
    match prefix_expression.token.token_type {
        tokens::TokenType::Bang => eval_bang_expression(right),
        tokens::TokenType::Minus => eval_minus_expression(right),
        _ => panic_program(RuntimeError::PrefixUnknownOperator(
            prefix_expression.operator.clone(),
            right.object_type().to_string(),
        )),
    }
}

fn eval_minus_expression(right: &Jar) -> Jar {
    match right {
        Jar::Integer(integer) => Jar::Integer(jar::IntegerObject {
            value: -integer.value,
        }),
        _ => panic_program(RuntimeError::PrefixUnknownOperator(
            "-".to_string(),
            right.object_type().to_string(),
        )),
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

fn is_condition_truthy(condition: &Jar) -> bool {
    match condition {
        Jar::Boolean(boolean) => boolean.value == true,
        Jar::Null(_) => false,
        _ => true,
    }
}

fn bind_wrap_bool(boolean: bool) -> Jar {
    if boolean {
        Jar::Boolean(&EVAL_TRUE)
    } else {
        Jar::Boolean(&EVAL_FALSE)
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

fn panic_program(error: RuntimeError) -> Jar {
    Jar::Panic(PanicObject { error })
}

fn is_panic(jar: &Jar) -> bool {
    match jar {
        Jar::Panic(_) => true,
        _ => false,
    }
}

fn unwrap_return_value(jar: Jar) -> Jar {
    match jar {
        Jar::Return(value) => *value,
        jar => jar,
    }
}

#[cfg(test)]
mod test {
    use std::sync;

    use crate::{
        environment, lexer,
        parser::{self, parse_program},
    };

    use super::*;

    #[test]
    fn test_eval_intenger_expression() {
        let inputs = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        inputs.iter().for_each(|(input, expected)| {
            let jar = test_eval(input);
            test_integer_object(&jar, *expected);
        });
    }

    #[test]
    fn test_eval_boolean_expression() {
        let inputs = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 <= 1", true),
            ("1 >= 1", true),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        inputs.iter().for_each(|(input, expected)| {
            let jar = test_eval(input);
            test_boolean_object(&jar, *expected);
        });
    }

    #[test]
    fn test_eval_if_expression() {
        let inputs = [
            (
                "if (true) { 10 }",
                Jar::Integer(IntegerObject { value: 10 }),
            ),
            ("if (false) { 10 }", Jar::Null(&EVAL_NULL)),
            ("if (1) { 10 }", Jar::Integer(IntegerObject { value: 10 })),
            (
                "if (1 < 2) { 10 }",
                Jar::Integer(IntegerObject { value: 10 }),
            ),
            ("if (1 > 2) { 10 }", Jar::Null(&EVAL_NULL)),
            (
                "if (1 > 2) { 10 } else { 20 }",
                Jar::Integer(IntegerObject { value: 20 }),
            ),
            (
                "if (1 < 2) { 10 } else { 20 }",
                Jar::Integer(IntegerObject { value: 10 }),
            ),
        ];

        inputs.iter().for_each(|(input, expected)| {
            let jar = test_eval(input);
            test_jar(&jar, expected);
        });
    }

    #[test]
    fn test_eval_return_statement() {
        let inputs = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "
                if (10 > 1) {
                	if (10 > 1) {
                 		if (1 > 1) {
                   			return 4*2;
                   		} else {
                     		return 4*5;
                     	}
                  		return 10;
                   	}
                    return 1;
                }
                return 1;
                ",
                20,
            ),
        ];

        inputs.iter().for_each(|(input, expected)| {
            let jar = test_eval(input);
            test_integer_object(&jar, *expected);
        });
    }

    #[test]
    fn test_eval_panics() {
        let inputs = [
            ("5 + true;", "type mismatch: expected Integer, got Boolean"),
            (
                "5 + true; 5;",
                "type mismatch: expected Integer, got Boolean",
            ),
            ("-true;", "unknown operator: -Boolean"),
            ("true + false;", "unknown operator: Boolean + Boolean"),
            ("5; true + false; 5;", "unknown operator: Boolean + Boolean"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: Boolean + Boolean",
            ),
            (
                "
             	if (10 > 1) {
	              	if (10 > 1) {
	               		return true + false;
	               	}

					return 1;
	            }
             	",
                "unknown operator: Boolean + Boolean",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        inputs.iter().for_each(|(input, expected)| {
            let jar = test_eval(input);
            test_panic_object(&jar, *expected);
        });
    }

    #[test]
    fn test_eval_let_statements() {
        let inputs = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        inputs.iter().for_each(|(input, expected)| {
            let jar = test_eval(input);
            test_integer_object(&jar, *expected);
        });
    }

    #[test]
    fn test_eval_function_object() {
        let input = "fn(x) { x + 2; }; ";
        let evaluation = test_eval(input);
        let function_object = match evaluation {
            Jar::Function(object) => object,
            _ => panic!("Expected Jar::Function, got {:?}", evaluation),
        };

        assert_eq!(function_object.parameters.len(), 1);
        assert_eq!(function_object.parameters[0].value, "x");

        let expected_body = "(x + 2)";

        assert_eq!(function_object.body.to_string(), expected_body);
    }

    #[test]
    fn test_eval_function_application() {
        let inputs = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        inputs.iter().for_each(|(input, expected)| {
            let jar = test_eval(input);
            test_integer_object(&jar, *expected);
        });
    }

    fn test_panic_object(jar: &Jar, expected: &str) {
        let panic_object = match jar {
            Jar::Panic(object) => object,
            _ => panic!("Expected Jar::Panic, got {:?}\n\t{}", jar, expected),
        };

        let expected = format!("PANIC: {}", expected);

        assert_eq!(
            panic_object.to_string(),
            expected,
            "Expected {}, got {}",
            expected,
            panic_object.to_string()
        );
    }

    fn test_jar(jar: &Jar, expected: &Jar) {
        match (jar, expected) {
            (Jar::Integer(left), Jar::Integer(right)) => {
                assert_eq!(
                    left.value, right.value,
                    "Expected {}, got {}",
                    expected, jar
                );
            }
            (Jar::Boolean(left), Jar::Boolean(right)) => {
                assert_eq!(
                    left.value, right.value,
                    "Expected {}, got {}",
                    expected, jar
                );
            }
            (Jar::Null(_left), Jar::Null(_right)) => {
                assert!(true);
            }
            _ => panic!("Expected {}, got {}", expected, jar),
        }
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
        let environment = sync::Arc::new(sync::Mutex::new(environment::Environment::new()));
        eval_program(&program, environment)
    }

    fn test_integer_object(jar: &Jar, expected: i64) {
        let integer_object = match jar {
            Jar::Integer(object) => object,
            jar => panic!("Expected Jar::Integer, got {:?}", jar),
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
