use super::super::{ValidationContext, ValidationRule};
use crate::{ast::*, visit::*};

/// Validate that a document uses all the fragments it defines at least once.
///
/// See [`ValidationRule`]
/// [Reference](https://spec.graphql.org/October2021/#sec-Fragments-Must-Be-Used)
#[derive(Default)]
pub struct NoUnusedFragments<'a> {
    fragment_names: Vec<&'a str>,
    fragment_spreads: Vec<&'a str>,
}

impl<'a> ValidationRule<'a> for NoUnusedFragments<'a> {}

impl<'a> Visitor<'a, ValidationContext<'a>> for NoUnusedFragments<'a> {
    fn enter_fragment(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        fragment: &'a FragmentDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        self.fragment_names.push(fragment.name.name);
        VisitFlow::Next
    }

    fn enter_fragment_spread(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        spread: &'a FragmentSpread<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        self.fragment_spreads.push(spread.name.name);
        VisitFlow::Skip
    }

    fn leave_document(
        &mut self,
        ctx: &mut ValidationContext<'a>,
        _document: &'a Document<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        for name in self.fragment_names.iter() {
            if !self.fragment_spreads.contains(name) {
                ctx.add_error("All defined fragments must be at least spread once");
            }
        }
        VisitFlow::Next
    }

    fn enter_variable_definition(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        _var_def: &'a VariableDefinition,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Skip
    }

    fn enter_argument(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        _argument: &'a Argument,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Skip
    }

    fn enter_directive(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        _directive: &'a Directive,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Skip
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_spread() {
        let ctx = ASTContext::new();
        let document = Document::parse(
            &ctx,
            "query { ...Root } fragment Root on Query { __typename }",
        )
        .unwrap();
        NoUnusedFragments::validate(&ctx, document).unwrap();
    }

    #[test]
    fn missing_spread() {
        let ctx = ASTContext::new();
        let document = Document::parse(
            &ctx,
            "query { __typename } fragment Root on Query { __typename }",
        )
        .unwrap();
        NoUnusedFragments::validate(&ctx, document).unwrap_err();
    }
}
