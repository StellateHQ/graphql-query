//! # Visiting and Transforming GraphQL ASTs
//!
//! The `graphql_query::visit` module contains utilities to traverse and transform GraphQL ASTs.
//! Mainly, this module exposes two traits relevant to this task:
//!
//! - The [Visitor] trait can be used to implement a visitor.
//! - The [Folder] trait can be used to implement a folder to transform an AST.
//!
//! This works via the [`VisitNode`] trait and [`FoldNode`] trait that most AST nodes implement and
//! where visiting and folding can start.
//!
//! Typically, a visitor is used in GraphQL to gain information about the AST and inspect it for
//! certain features. It may also be used to collect information about the AST before it's
//! transformed in a second folder step, if a single pass over the AST isn't enough.
//!
//! In this example we'll define a visitor that counts all operations in a document:
//!
//! ```
//! use graphql_query::{ast::*, visit::*};
//!
//! #[derive(Default)]
//! struct CountOperations {
//!    operations: usize,
//! }
//!
//! impl<'a> Visitor<'a> for CountOperations {
//!     fn enter_fragment(
//!         &mut self,
//!         _ctx: &mut (),
//!         _fragment: &'a FragmentDefinition<'a>,
//!         _info: &VisitInfo
//!     ) -> VisitFlow {
//!         // We can skip over fragment nodes and never traverse its children,
//!         // since we're only interested in counting operations
//!         VisitFlow::Skip
//!     }
//!
//!     fn enter_operation(
//!         &mut self,
//!         _ctx: &mut (),
//!         operation: &'a OperationDefinition<'a>,
//!         _info: &VisitInfo
//!     ) -> VisitFlow {
//!         self.operations += 1;
//!         VisitFlow::Next
//!     }
//! }
//! ```
//!
//! We may then execute this visitor using `Document::visit`,
//! e.g. `document.visit(&ctx, &mut CountOperations::default())`.
//!
//! Of course, it's often necessary to ensure that while the context is mutated inside the visitor,
//! its results should still later on be made accessible.
//!
//! [More information on the Visitor trait](Visitor)
//!
//! A folder is very similar but instead receives and returns AST nodes to create a new copy of an
//! AST while transforming it. A simple folder that changes all names to `"oomph"` may look like
//! such:
//!
//! ```
//! use graphql_query::{ast::*, visit::*};
//!
//! #[derive(Default)]
//! struct OomphRename {}
//!
//! impl<'a> SimpleFolder<'a> for OomphRename {
//!     fn named_type(&mut self, _name: NamedType<'a>) -> NamedType<'a> {
//!       NamedType { name: "oomph" }
//!     }
//! }
//! ```
//!
//! Which we may then execute using `Document::fold`,
//! e.g. `document.fold(&ctx, &mut OomphRename::default()).unwrap()`.
//!
//! Notably, we've used the [`SimpleFolder`] trait in this example, since it allows us to write less
//! code than with the [Folder] trait when a folder is really straightforward.
//!
//! [More information on the Folder trait](Folder)

mod compose;
mod folder;
mod folder_simple;
mod path;
mod visitor;

pub use compose::ComposedVisitor;
pub use folder::*;
pub use path::*;
pub use visitor::*;
