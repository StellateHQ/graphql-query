//! # GraphQL Query Language AST
//!
//! The `graphql_query::ast` module contains the GraphQL query language AST and traits to parse and
//! print the AST. The AST implemented in this crate is specialized to only implement the
//! client-side GraphQL query language that clients use to make requests to a GraphQL service.
//! [Reference](https://spec.graphql.org/October2021/#sec-Language)
//!
//! It's easiest to use this module by importing all of it, however, its three main parts are:
//! - [`ASTContext`], a context containing an arena that defines the lifetime for an AST
//! - [`ParseNode`], a trait using which AST Nodes are parsed from source text
//! - [`PrintNode`], a trait using which AST Nodes are printed into source text
//!
//! The following workflow describes the minimum that's done using this module and while an AST
//! Context is active in the given scope.
//!
//! ```
//! use graphql_query::ast::*;
//!
//! // Create an AST Context for a document
//! let ctx = ASTContext::new();
//!
//! // Parse a source text into a Document AST root node
//! let ast = Document::parse(&ctx, "{ field }").unwrap();
//!
//! // Print the Document node to an output String
//! let output = ast.print();
//! ```

#[allow(clippy::module_inception)]
mod ast;

mod ast_conversion;
mod ast_kind;
mod ast_util;
mod lexer;
mod parser;
mod printer;

pub use ast::*;
pub use ast_kind::ASTKind;
pub use parser::ParseNode;
pub use printer::PrintNode;
