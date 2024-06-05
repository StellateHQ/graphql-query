use std::{error::Error, fmt::Display};

// Todo: Maybe reuse top level GraphQL/Syntax error struct? Would that be suitable?
#[derive(Debug)]
pub enum SchemaError {
    SyntaxError(String),
    ValidationError(String),
}

impl Display for SchemaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SchemaError::SyntaxError(s) => write!(f, "{}", s),
            SchemaError::ValidationError(s) => write!(f, "Validation error: {}", s),
        }
    }
}

impl Error for SchemaError {}

macro_rules! syntax_err {
    ($msg:literal, $($arg:tt)*) => {
        Err(syntax!($msg, $($arg)*))
    };

    ($msg:literal) => {
        Err(syntax!($msg))
    };
}

macro_rules! syntax {
    ($msg:literal, $($arg:tt)*) => {
        SchemaError::SyntaxError(format!($msg, $($arg)*))
    };

    ($msg:literal) => {
        SchemaError::SyntaxError(format!($msg))
    };
}

macro_rules! validation {
    ($msg:literal, $($arg:tt)*) => {
        SchemaError::ValidationError(format!($msg, $($arg)*))
    };

    ($msg:literal) => {
        SchemaError::ValidationError(format!($msg))
    };
}

// Required for macro visibility.
pub(crate) use syntax;
pub(crate) use syntax_err;
pub(crate) use validation;
