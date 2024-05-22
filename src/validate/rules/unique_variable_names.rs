use bumpalo::collections::Vec;

use super::super::{ValidationContext, ValidationRule};
use crate::{ast::*, visit::*};

/// Validates that no operation the document defines has duplicate variable names in its variable
/// definitions.
///
/// See [`ValidationRule`]
/// [Reference](https://spec.graphql.org/October2021/#sec-Variable-Uniqueness)
pub struct UniqueVariableNames<'a> {
    used_variable_names: Vec<'a, &'a str>,
}

impl<'a> DefaultIn<'a> for UniqueVariableNames<'a> {
    fn default_in(arena: &'a bumpalo::Bump) -> Self {
        Self {
            used_variable_names: Vec::new_in(arena),
        }
    }
}

impl<'a> ValidationRule<'a> for UniqueVariableNames<'a> {}

impl<'a> Visitor<'a, ValidationContext<'a>> for UniqueVariableNames<'a> {
    fn enter_operation(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        _operation: &'a OperationDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        self.used_variable_names.clear();
        VisitFlow::Next
    }

    fn enter_variable_definition(
        &mut self,
        ctx: &mut ValidationContext<'a>,
        var_def: &'a VariableDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        if self.used_variable_names.contains(&var_def.variable.name) {
            ctx.add_error("All defined variables per operation must have unique names");
            VisitFlow::Break
        } else {
            self.used_variable_names.push(var_def.variable.name);
            VisitFlow::Skip
        }
    }

    fn enter_selection_set(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        _selection_set: &'a SelectionSet,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Skip
    }

    fn enter_directive(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        _fragment: &'a Directive,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Skip
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
    fn valid_variables() {
        let ctx = ASTContext::new();
        let document = Document::parse(&ctx, "query ($a: Int, $b: Int) { __typename }").unwrap();
        UniqueVariableNames::validate(&ctx, document).unwrap();
    }

    #[test]
    fn overlapping_variables() {
        let ctx = ASTContext::new();
        let document = Document::parse(&ctx, "query ($a: Int, $a: Int) { __typename }").unwrap();
        UniqueVariableNames::validate(&ctx, document).unwrap_err();
    }
}
