use super::context::ValidationContext;
use crate::ast::{ASTContext, Document};
use crate::error::Result;
use crate::visit::{ComposedVisitor, VisitNode, Visitor};
use std::borrow::Borrow;

/// Trait for a `ValidationRule` that checks a given `GraphQl` document against its rules using a
/// visitor.
///
/// A rule always implements a visitor and accepts the [`ValidationContext`] structure as
/// its passed context.
///
/// Rules implement the `Default` trait, which allows them to be instantiated easily.
/// The intention of using `Default` is for rules to not carry any external
/// state as for GraphQL validation no external state is needed.
pub trait ValidationRule<'a>: Visitor<'a, ValidationContext<'a>> + Default {
    /// Run this `ValidationRule` against the given document and return a result which errors if
    /// the rule fails on the document.
    #[inline]
    fn validate(ctx: &'a ASTContext, document: &'a Document<'a>) -> Result<()> {
        let mut validation = ValidationContext::new(ctx);
        let mut visitor = Self::default();
        document.visit(&mut validation, &mut visitor);
        validation.to_result()
    }
}

impl<'a, A, B> Default for ComposedVisitor<'a, ValidationContext<'a>, A, B>
where
    A: ValidationRule<'a>,
    B: ValidationRule<'a>,
{
    #[inline]
    fn default() -> Self {
        ComposedVisitor::new(A::default(), B::default())
    }
}

impl<'a, A, B> ValidationRule<'a> for ComposedVisitor<'a, ValidationContext<'a>, A, B>
where
    A: ValidationRule<'a>,
    B: ValidationRule<'a>,
{
}

/// Trait to run a [`ValidationRule`] on a given GraphQL Document node.
pub trait ValidateNode<'a>
where
    Self: Borrow<Document<'a>>,
{
    /// Run the generic validation rule on the document node and return a result which errors if
    /// the validation rule fails on the current document.
    ///
    /// `document.validate::<YourValidationRule>(&ctx)`
    #[inline]
    fn validate<Rule: ValidationRule<'a>>(&'a self, ctx: &'a ASTContext) -> Result<()> {
        Rule::validate(ctx, self.borrow())
    }
}

impl<'a> ValidateNode<'a> for Document<'a> {}
