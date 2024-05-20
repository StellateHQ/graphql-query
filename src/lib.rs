//! `graphql_query`
//! =========
//!
//! _Stupendously fast and easy GraphQL Query Language handling._
//!
//! The **`graphql_query`** library follows two goals:
//!
//! - To support a pleasant-to-use API for the GraphQL Query Language
//! - To be stupendously fast at processing GraphQL Query Language ASTs
//!
//! In short, _surprise!_ The `graphql_query` crate while handling a part of GraphQL does
//! not aim to support full, server-side GraphQL execution or the GraphQL Schema Language.
//! Many parts of the server-side execution of requests are one-off operations. Parsing a schema
//! and using it are operations that could even be preprocessed using the reference GraphQL.js
//! implementation.
//!
//! A harder focus is to optimize how individual GraphQL requests are handled and making it easier
//! to write complex code on top of an easy to use AST.
//! GraphQL throughput is highly important in ensuring that GraphQL doesn't fall behind any other
//! solutions, which don't feature a rich query language.
//! On top, having an AST and library that's sufficiently easy to use and contains enough primitives
//! and utilities at the same time as valuing performance is something that's harder to do when
//! focusing on building a full GraphQL server.
//!
//! As such, this library focuses on just processing GraphQL queries for the purpose of
//! intermediary GraphQL layers, which operate inbetween GraphQL clients and GraphQL servers.
//!
//! [A good place to start learning more about this crate is the `ast` module...](ast)

pub mod ast;
pub mod error;
pub mod schema;
pub mod validate;
pub mod visit;

pub use bumpalo;

#[cfg(feature = "json")]
pub mod json;
