//! # Error and Result for this crate
//!
//! This crate defines a common [Error] structure that's used across this trait, or that certain
//! utilities convert their errors to.

use logos::Span;
use std::{error, fmt, result};

/// This crate's result type using the [Error] structure.
pub type Result<T> = result::Result<T, Error>;

/// This crate's error structure which internal errors are converted into.
///
/// The error is split into a general message and a context string. For parsing, for instance, the
/// context string is populated with a snippet of the source text, while for validation the context
/// is populated with a list of errors.
///
/// The Error implements both the [`fmt::Display`] and [`fmt::Debug`] traits. It also implements
// [`error::Error`] so that it can be used with existing patterns for error handling.
#[derive(PartialEq, Eq, Clone)]
pub struct Error {
    pub(crate) message: String,
    pub(crate) location: Option<Location>,
    pub(crate) context: Option<String>,
    pub(crate) error_type: ErrorType,
}

#[derive(PartialEq, Eq, Clone)]
pub enum ErrorType {
    GraphQL,
    Syntax,
}

impl Error {
    /// Create a new Error with only a main message from an input string.
    pub fn new<S: Into<String>>(message: S, error_type: Option<ErrorType>) -> Self {
        Self {
            message: message.into(),
            location: None,
            context: None,
            error_type: error_type.unwrap_or(ErrorType::GraphQL),
        }
    }

    /// Create a new Error with a main message and a context string from two input strings.
    pub fn new_with_context<S: Into<String>>(
        message: S,
        location: Option<Location>,
        context: S,
        error_type: Option<ErrorType>,
    ) -> Self {
        Self {
            message: message.into(),
            location,
            context: Some(context.into()),
            error_type: error_type.unwrap_or(ErrorType::GraphQL),
        }
    }

    /// Returns the message of the current error. The context is discarded.
    pub fn message(&self) -> &str {
        self.message.as_ref()
    }

    /// Returns the location of the current error.
    pub fn location(&self) -> &Option<Location> {
        &self.location
    }

    /// Formats this error, with the option to include the context information as well,
    /// which will cause the string to be multi-line.
    pub fn print(&self, include_ctx: bool) -> String {
        let formatted = match self.error_type {
            ErrorType::GraphQL => {
                format!("GraphQL Error: {}", self.message)
            }
            ErrorType::Syntax => {
                format!("Syntax Error: {}", self.message)
            }
        };

        match self.context {
            Some(ref context) if include_ctx => format!("{}\n{}", formatted, context),
            _ => formatted,
        }
    }
}

pub(crate) fn print_span(source: &str, span: Span) -> String {
    let mut out = String::new();
    let start_line = source[..span.start].lines().count();

    let start = source[..span.start]
        .rfind('\n')
        .and_then(|start| source[..start].rfind('\n'))
        .map_or(0, |idx| idx + 1);

    let end = source[span.end..]
        .find('\n')
        .map_or(source.len(), |idx| idx + span.end);

    let snippet = &source[start..end];
    let line_num_pad = (start_line + snippet.lines().count() - 1).to_string().len();
    for (index, line) in snippet.lines().enumerate() {
        if index > 0 {
            out.push('\n');
        }
        let line_num = (start_line + index).to_string();
        out.push_str(&" ".repeat(line_num_pad - line_num.len() + 1));
        out.push_str(&line_num);
        out.push_str(" | ");
        out.push_str(line);
    }
    if source[span.start..span.end].find('\n').is_none() {
        let start = source[..span.start].rfind('\n').map_or(0, |idx| idx + 1);
        out.push('\n');
        out.push_str(&" ".repeat(line_num_pad + 1));
        out.push_str(" | ");
        out.push_str(&" ".repeat(span.start - start));
        out.push_str(&"^".repeat(span.end - span.start));
    };

    out
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

pub(crate) fn get_location(source: &str, span: Span) -> Location {
    let line = source[..span.start].lines().count();
    let col = source[..span.start]
        .lines()
        .last()
        .map_or(span.start, |x| x.len());

    Location { line, column: col }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.print(true))
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\n{}\n", self)
    }
}

impl error::Error for Error {}
