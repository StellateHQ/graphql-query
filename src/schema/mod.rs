//! # Using Schema Definitions
//!
//! The `graphql_query::schema` module contains utilities to create a GraphQL Schema or parse it from
//! introspection data. This information may then be used to validate and introspect a schema as to
//! possible queries that may be written against it.
//!
//! The [BuildClientSchema] trait may be used to convert introspection data into a usable [Schema],
//! which may then be used to inspect GraphQL defnitions:
//!
//! ```
//! use graphql_query::{ast::ASTContext, schema::*};
//!
//! fn inspect() {
//!     let ctx = ASTContext::new();
//!
//!     let introspection_json = include_str!("../../fixture/introspection_query.json");
//!     let introspection: IntrospectionQuery = serde_json::from_str(introspection_json).unwrap();
//!     let _schema = introspection.build_client_schema(&ctx);
//! }
//! ```
//!
//! [More information on the Schema struct.](Schema)

pub mod build_client_schema;
pub mod introspection;
#[allow(clippy::module_inception)]
pub mod schema;
mod schema_reference;

pub use build_client_schema::BuildClientSchema;
pub use introspection::{IntrospectionQuery, IntrospectionSchema};
pub use schema::*;
pub use schema_reference::*;
