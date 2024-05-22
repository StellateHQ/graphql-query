use super::super::context::ValidationContext;
use super::super::validate::ValidationRule;
use crate::{ast::*, visit::*};

/// Validate a document to only contain one anonymous operation or multiple named operations.
///
/// See [`ValidationRule`]
/// [Reference](https://spec.graphql.org/October2021/#sec-Lone-Anonymous-Operation)
#[derive(Default)]
pub struct LoneAnonymousOperation {
    operations: usize,
    has_anonymous: bool,
}

impl<'a> ValidationRule<'a> for LoneAnonymousOperation {}

impl<'a> Visitor<'a, ValidationContext<'a>> for LoneAnonymousOperation {
    fn enter_fragment(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        _operation: &'a FragmentDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Skip
    }

    fn enter_operation(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        operation: &'a OperationDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        self.has_anonymous |= operation.name.is_none();
        self.operations += 1;
        VisitFlow::Skip
    }

    fn leave_document(
        &mut self,
        ctx: &mut ValidationContext<'a>,
        _document: &'a Document<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        if self.has_anonymous && self.operations > 1 {
            ctx.add_error("Anonymous operation must be the only defined operation.");
        }
        VisitFlow::Next
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lone_operation() {
        let ctx = ASTContext::new();
        let document = Document::parse(&ctx, "query { __typename }").unwrap();
        LoneAnonymousOperation::validate(&ctx, document).unwrap();
    }

    #[test]
    fn two_named() {
        let ctx = ASTContext::new();
        let document =
            Document::parse(&ctx, "query A { __typename } query B { __typename }").unwrap();
        LoneAnonymousOperation::validate(&ctx, document).unwrap();
    }

    #[test]
    fn two_anonymous() {
        let ctx = ASTContext::new();
        let document = Document::parse(&ctx, "{ __typename } { __typename }").unwrap();
        LoneAnonymousOperation::validate(&ctx, document).unwrap_err();
    }
}
