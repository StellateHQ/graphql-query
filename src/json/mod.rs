//! # JSON Conversion
//!
//! The `graphql_query::json` module contains utilities to convert from and to `serde_json` values.
//! Any values that are converted to `graphql_query_rs`' structures are represented as AST values.
//! introspection data. This information may then be used to validate and introspect a schema as to
//! possible queries that may be written against it.
//!
//! The [ValueFromNode] trait allows conversion to `serde_json` values using a `to_json` method on
//! any given value. This methods converts without using any type information.
//!
//! The module otherwise only contains a handful of utility functions:
//!
//! - [ast_variables_from_value] is used to create a `Variables` map for a given JSON value.
//! - [ast_from_value] is used to convert any given JSON value to AST values while casting it.
//! - [ast_from_value_untyped] is used to convert any given JSON value to AST values without casting.
//! - [value_from_ast_variables] is used to convert AST `Variables` back to a JSON value.
//! - [value_from_ast] is used to convert a given AST value to a JSON value while filling in variables.

#[cfg(feature = "json")]
extern crate serde_json;

#[cfg(feature = "json")]
extern crate serde;

mod conversion;
mod values;

pub use conversion::*;
pub use values::*;
