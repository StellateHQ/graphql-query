//! # Validation Rules for GraphQL ASTs
//!
//! This module contains logic to run validation rules on GraphQL Query Language documents.
//! It provides rules that have already been implemented to validate a document as much as it can
//! without using any schema information, which are rules grouped into this module's [`DefaultRules`](rules::DefaultRules)
//! and utilities to create your own [`ValidationRules`](ValidationRule).
//!
//! The rules this module already comes with are:
//!
//! - [`rules::KnownFragmentNames`]: validates that all spread fragments are defined
//! - [`rules::LoneAnonymousOperation`]: validates that a document only contains a single anonymous operation
//! - [`rules::NoFragmentCycles`]: validates that no fragment is spread in on itself to avoid looping
//! - [`rules::NoUndefinedVariables`]: checks that all used variables are defined per operation
//! - [`rules::NoUnusedFragments`]: validates that all fragments in a document are used at least once
//! - [`rules::UniqueArgumentNames`]: checks for arguments that are used to not contain duplicates
//! - [`rules::UniqueFragmentNames`]: checks that no fragments share the same name
//! - [`rules::UniqueOperationNames`]: checks that no operations share the same name
//! - [`rules::UniqueVariableNames`]: checks that no variables share the same name
//!
//! The [visit](crate::visit) module is used to actually execute validation rules.
//! The [`ValidationRule`] trait is simply defined to implement the [Visitor](crate::visit::Visitor) trait
//! and to accept the [`ValidationContext`], which is used to keep track of validation errors.
//!
//! As such, the [`DefaultRules`](rules::DefaultRules) rule is a [`ValidationRule`] itself that's
//! composed using the [`ComposedVisitor`](crate::visit::ComposedVisitor) utility.
//!
//! All rules must implement the `Default` trait, which makes it easier to quickly run a validation
//! rule and isolates them from external state, since no validation requires any external state.
//!
//! For example, this is one way to run a validation rule, in this case `DefaultRules`:
//!
//! ```
//! use graphql_query::{ast::*, validate::*};
//!
//! let ctx = ASTContext::new();
//! let document = Document::parse(&ctx, "{ field }").unwrap();
//!
//! DefaultRules::validate(&ctx, &document).unwrap()
//! ```
//!
//! Another way is to utilize the [`ValidateNode`] trait instead to run validation starting from an
//! AST Node rather from the rule itself:
//!
//! ```
//! use graphql_query::{ast::*, validate::*};
//!
//! let ctx = ASTContext::new();
//! let document = Document::parse(&ctx, "{ field }").unwrap();

//! document.validate::<DefaultRules>(&ctx).unwrap()
//! ```

#[allow(clippy::module_inception)]
mod validate;

mod context;

pub mod rules;
pub use context::ValidationContext;
pub use rules::DefaultRules;
pub use validate::*;
