use bumpalo::collections::Vec;

use super::super::{ValidationContext, ValidationRule};
use crate::{ast::*, visit::*};

/// Validate that a document uses all the variables it defines at least once.
///
/// See [`ValidationRule`]
/// [Reference](https://spec.graphql.org/draft/#sec-All-Variables-Used)
pub struct NoUnusedVariables<'a> {
    variables: Vec<'a, &'a str>,
    used_variables: Vec<'a, &'a str>,
}

impl<'a> DefaultIn<'a> for NoUnusedVariables<'a> {
    fn default_in(arena: &'a bumpalo::Bump) -> Self {
        Self {
            variables: Vec::new_in(arena),
            used_variables: Vec::new_in(arena),
        }
    }
}

impl<'a> ValidationRule<'a> for NoUnusedVariables<'a> {}

impl<'a> Visitor<'a, ValidationContext<'a>> for NoUnusedVariables<'a> {
    fn enter_operation(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        operation: &'a OperationDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        operation
            .variable_definitions
            .children
            .iter()
            .for_each(|def| {
                self.variables.push(def.variable.name);
            });
        VisitFlow::Next
    }

    fn enter_field(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        field: &'a Field<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        field.arguments.children.iter().for_each(|arg| {
            if let Value::Variable(var) = arg.value {
                self.used_variables.push(var.name);
            }
        });
        VisitFlow::Next
    }

    fn leave_document(
        &mut self,
        ctx: &mut ValidationContext<'a>,
        _document: &'a Document<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        self.variables.iter().for_each(|defined_variable| {
            if !self.used_variables.contains(defined_variable) {
                ctx.add_error("All defined variables must be at least used once");
            }
        });
        VisitFlow::Next
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn used_all_variables() {
        let ctx = ASTContext::new();
        let document =
            Document::parse(&ctx, "query ($x: Int!) { todos(from: $x) { id } }").unwrap();
        NoUnusedVariables::validate(&ctx, document).unwrap();
    }

    #[test]
    fn has_unused_variable() {
        let ctx = ASTContext::new();
        let document = Document::parse(
            &ctx,
            "query ($x: Int!, $unused: String!) { todos(from: $x) { id } }",
        )
        .unwrap();
        NoUnusedVariables::validate(&ctx, document).unwrap_err();
    }
}
