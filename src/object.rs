use std::{fmt, path::Display};

pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

pub trait Object: fmt::Display {
    fn object_type() -> ObjectType;
}

#[derive(Debug)]
pub enum Jar {
    Integer(IntegerObject),
    Boolean(&'static BooleanObject),
    Null(&'static NullObject),
}

impl fmt::Display for Jar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Jar::Integer(object) => write!(f, "{}", object),
            Jar::Boolean(object) => write!(f, "{}", object),
            Jar::Null(object) => write!(f, "{}", object),
        }
    }
}

#[derive(Debug)]
pub struct IntegerObject {
    pub value: i64,
}

impl Object for IntegerObject {
    fn object_type() -> ObjectType {
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
    fn object_type() -> ObjectType {
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
    fn object_type() -> ObjectType {
        ObjectType::Null
    }
}

impl fmt::Display for NullObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "null")
    }
}
