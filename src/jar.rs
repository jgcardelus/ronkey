use std::{cell, fmt, rc, sync};

use crate::{ast, environment};

#[derive(PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    Panic,
    Function,
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectType::Integer => write!(f, "Integer"),
            ObjectType::Boolean => write!(f, "Boolean"),
            ObjectType::Null => write!(f, "Null"),
            ObjectType::Panic => write!(f, "Panic"),
            ObjectType::Function => write!(f, "Function"),
        }
    }
}

pub trait Object: fmt::Display {
    fn object_type(&self) -> ObjectType;
}

#[derive(Debug, Clone)]
pub enum Jar {
    Integer(IntegerObject),
    Boolean(&'static BooleanObject),
    Null(&'static NullObject),
    Return(Box<Jar>),
    Panic(PanicObject),
    Function(FunctionObject),
}

impl Object for Jar {
    fn object_type(&self) -> ObjectType {
        match self {
            Jar::Integer(_) => ObjectType::Integer,
            Jar::Boolean(_) => ObjectType::Boolean,
            Jar::Null(_) => ObjectType::Null,
            Jar::Return(_) => ObjectType::Null,
            Jar::Panic(_) => ObjectType::Panic,
            Jar::Function(_) => ObjectType::Function,
        }
    }
}

impl fmt::Display for Jar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Jar::Integer(object) => write!(f, "{}", object),
            Jar::Boolean(object) => write!(f, "{}", object),
            Jar::Null(object) => write!(f, "{}", object),
            Jar::Return(object) => write!(f, "{}", object),
            Jar::Panic(object) => write!(f, "{}", object),
            Jar::Function(object) => write!(f, "{}", object),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IntegerObject {
    pub value: i64,
}

impl Object for IntegerObject {
    fn object_type(&self) -> ObjectType {
        ObjectType::Integer
    }
}

impl fmt::Display for IntegerObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct BooleanObject {
    pub value: bool,
}

impl Object for BooleanObject {
    fn object_type(&self) -> ObjectType {
        ObjectType::Boolean
    }
}

impl fmt::Display for BooleanObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct NullObject {}

impl Object for NullObject {
    fn object_type(&self) -> ObjectType {
        ObjectType::Null
    }
}

impl fmt::Display for NullObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "null")
    }
}

#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub parameters: Vec<ast::Identifier>,
    pub body: ast::BlockStatement,
    pub environment: sync::Arc<sync::Mutex<environment::Environment>>,
}

impl Object for FunctionObject {
    fn object_type(&self) -> ObjectType {
        ObjectType::Function
    }
}

impl fmt::Display for FunctionObject {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "fn")?;
        write!(f, "(")?;
        for (i, parameter) in self.parameters.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", parameter)?;
        }
        write!(f, ") {{ \n")?;
        for statement in &self.body.statements {
            write!(f, "{}\n", statement)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct PanicObject {
    pub error: RuntimeError,
}

impl Object for PanicObject {
    fn object_type(&self) -> ObjectType {
        ObjectType::Panic
    }
}

impl fmt::Display for PanicObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "PANIC: {}", self.error)
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeError {
    PrefixUnknownOperator(String, String),
    InfixUnknownOperator(String, String, String),
    TypeMismatch(String, String),
    IdentifierNotFound(String),
    NotAFunction(String),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::PrefixUnknownOperator(left, operator) => {
                write!(f, "unknown operator: {}{}", left, operator)
            }
            RuntimeError::InfixUnknownOperator(left, operator, right) => {
                write!(f, "unknown operator: {} {} {}", left, operator, right)
            }
            RuntimeError::TypeMismatch(left, right) => {
                write!(f, "type mismatch: expected {}, got {}", left, right)
            }
            RuntimeError::IdentifierNotFound(identifier) => {
                write!(f, "identifier not found: {}", identifier)
            }
            RuntimeError::NotAFunction(identifier) => {
                write!(f, "{} is not a function", identifier)
            }
        }
    }
}
