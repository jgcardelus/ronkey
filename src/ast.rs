use crate::tokens::Token;
use std::fmt;

pub trait Node: fmt::Display {
    fn token_literal(&self) -> String;
}

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Empty,
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.token_literal(),
            Expression::IntegerLiteral(integer_literal) => integer_literal.token_literal(),
            Expression::Prefix(prefix_expression) => prefix_expression.token_literal(),
            Expression::Infix(infix_expression) => infix_expression.token_literal(),
            Expression::Empty => panic!("Empty expression should not be used"),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(identifier) => identifier.fmt(f),
            Expression::IntegerLiteral(integer_literal) => integer_literal.fmt(f),
            Expression::Prefix(prefix_expression) => prefix_expression.fmt(f),
            Expression::Infix(infix_expression) => infix_expression.fmt(f),
            Expression::Empty => panic!("Empty expression should not be used"),
        }
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let(let_statement) => let_statement.token_literal(),
            Statement::Return(return_statement) => return_statement.token_literal(),
            Statement::Expression(expression_statement) => expression_statement.token_literal(),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(let_statement) => let_statement.fmt(f),
            Statement::Return(return_statement) => return_statement.fmt(f),
            Statement::Expression(expression_statement) => expression_statement.fmt(f),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            return self.statements[0].token_literal();
        } else {
            return "".to_string();
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.statements
            .iter()
            .fold(fmt::Result::Ok(()), |acc, statement| {
                match statement.fmt(f) {
                    fmt::Result::Ok(()) => acc,
                    fmt::Result::Err(err) => fmt::Result::Err(err),
                }
            })
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let ")?;
        self.name.fmt(f)?;
        write!(f, " = ")?;
        self.value.fmt(f)?;
        write!(f, ";")?;
        return fmt::Result::Ok(());
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub value: Expression,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return ")?;
        self.value.fmt(f)?;
        write!(f, ";")?;
        return fmt::Result::Ok(());
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(expression) = &self.expression {
            expression.fmt(f)
        } else {
            write!(f, "")
        }
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Option<Expression>>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.right.as_ref() {
            Some(expression) => write!(f, "({}{})", self.operator, expression),
            None => write!(f, "({}, error: expected expression)", self.operator),
        }
    }
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Option<Expression>>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.right.as_ref() {
            Some(right) => write!(f, "({} {} {})", self.left, self.operator, right),
            None => {
                write!(
                    f,
                    "({} {} error: expected right expression)",
                    self.left, self.operator
                )
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::tokens::TokenType;

    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Token {
                    token_type: TokenType::Let,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "x".to_string(),
                    },
                    value: "x".to_string(),
                },
                value: Expression::Identifier(Identifier {
                    token: Token {
                        token_type: TokenType::Ident,
                        literal: "y".to_string(),
                    },
                    value: "y".to_string(),
                }),
            })],
        };

        assert_eq!(
            program.to_string(),
            "let x = y;",
            "program.to_string() does not match expected output, expected: {}, got: {}",
            "let x = y;",
            program.to_string()
        );
    }
}
