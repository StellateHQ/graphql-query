use super::super::{ValidationContext, ValidationRule};
use crate::{ast::*, visit::*};

/// Validates that no operation the document defines have duplicate names.
/// Note: Operations and Fragments are allowed to share names.
///
/// See [`ValidationRule`]
/// [Reference](https://spec.graphql.org/October2021/#sec-Operation-Name-Uniqueness)
#[derive(Default)]
pub struct UniqueOperationNames<'a> {
    used_operation_names: Vec<&'a str>,
}

impl<'a> ValidationRule<'a> for UniqueOperationNames<'a> {}

impl<'a> Visitor<'a, ValidationContext<'a>> for UniqueOperationNames<'a> {
    fn enter_operation(
        &mut self,
        ctx: &mut ValidationContext<'a>,
        operation: &'a OperationDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        if let Some(name) = operation.name {
            if self.used_operation_names.contains(&name.name) {
                ctx.add_error("All defined operations must have unique names");
                VisitFlow::Break
            } else {
                self.used_operation_names.push(name.name);
                VisitFlow::Skip
            }
        } else {
            VisitFlow::Skip
        }
    }

    fn enter_fragment(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        _fragment: &'a FragmentDefinition,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Skip
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_operation_names() {
        let ctx = ASTContext::new();
        let document = Document::parse(&ctx, "query Root { __typename }").unwrap();
        UniqueOperationNames::validate(&ctx, document).unwrap();
    }

    #[test]
    fn overlapping_fragment_names() {
        let ctx = ASTContext::new();
        let document = Document::parse(
            &ctx,
            "query Root { __typename } mutation Root { __typename }",
        )
        .unwrap();
        UniqueOperationNames::validate(&ctx, document).unwrap_err();
    }
}
