use bumpalo::{collections::Vec, Bump};
use hashbrown::{hash_map::DefaultHashBuilder, HashMap};

use super::super::{ValidationContext, ValidationRule};
use crate::{ast::*, visit::*};

#[derive(Clone)]
struct OperationEdge<'a> {
    defined_vars: Vec<'a, &'a str>,
    used_fragments: Vec<'a, &'a str>,
}

#[derive(Clone)]
struct FragmentEdge<'a> {
    used_vars: Vec<'a, &'a str>,
    used_fragments: Vec<'a, &'a str>,
}

/// Validate that a document defines all the variables it uses per operation
///
/// See [`ValidationRule`]
/// [Reference](https://spec.graphql.org/October2021/#sec-All-Variable-Uses-Defined)
pub struct NoUndefinedVariables<'a> {
    used_vars: Vec<'a, &'a str>,
    defined_vars: Vec<'a, &'a str>,
    used_fragments: Vec<'a, &'a str>,
    operation_edges: Vec<'a, OperationEdge<'a>>,
    fragment_edges: HashMap<&'a str, FragmentEdge<'a>, DefaultHashBuilder, &'a Bump>,
}

impl<'a> DefaultIn<'a> for NoUndefinedVariables<'a> {
    fn default_in(arena: &'a bumpalo::Bump) -> Self {
        Self {
            used_vars: Vec::new_in(arena),
            defined_vars: Vec::new_in(arena),
            used_fragments: Vec::new_in(arena),
            operation_edges: Vec::new_in(arena),
            fragment_edges: HashMap::new_in(arena),
        }
    }
}

impl<'a> ValidationRule<'a> for NoUndefinedVariables<'a> {}

impl<'a> Visitor<'a, ValidationContext<'a>> for NoUndefinedVariables<'a> {
    fn enter_variable_definition(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        var_def: &'a VariableDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        self.defined_vars.push(var_def.variable.name);
        VisitFlow::Skip
    }

    fn enter_argument(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        argument: &'a Argument,
        _info: &VisitInfo,
    ) -> VisitFlow {
        if let Value::Variable(var) = argument.value {
            self.used_vars.push(var.name);
        }
        VisitFlow::Skip
    }

    fn leave_operation(
        &mut self,
        ctx: &mut ValidationContext<'a>,
        _operation: &'a OperationDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        for var in self.used_vars.iter() {
            if !self.defined_vars.contains(var) {
                ctx.add_error(
                    "All variables used within operations must be defined on the operation",
                );
                return VisitFlow::Break;
            }
        }
        self.operation_edges.push(OperationEdge {
            defined_vars: self.defined_vars.clone(),
            used_fragments: self.used_fragments.clone(),
        });
        self.used_fragments.clear();
        self.used_vars.clear();
        self.defined_vars.clear();
        VisitFlow::Next
    }

    fn leave_fragment(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        fragment: &'a FragmentDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        let name = fragment.name.name;
        self.fragment_edges.insert(
            name,
            FragmentEdge {
                used_vars: self.used_vars.clone(),
                used_fragments: self.used_fragments.clone(),
            },
        );
        self.used_fragments.clear();
        self.used_vars.clear();
        VisitFlow::Next
    }

    fn enter_fragment_spread(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        spread: &'a FragmentSpread<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        self.used_fragments.push(spread.name.name);
        VisitFlow::Skip
    }

    fn leave_document(
        &mut self,
        ctx: &mut ValidationContext<'a>,
        _document: &'a Document<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        let mut visited: Vec<&'a str> = Vec::new_in(ctx.arena);
        for operation_edge in self.operation_edges.iter() {
            if references_undefined_var(
                &mut visited,
                &self.fragment_edges,
                &operation_edge.defined_vars,
                &operation_edge.used_fragments,
            ) {
                ctx.add_error("All variables within fragments must be defined on the operation they're used in");
                return VisitFlow::Break;
            }
            visited.clear();
        }
        VisitFlow::Next
    }
}

fn references_undefined_var<'a>(
    visited: &mut Vec<&'a str>,
    fragment_edges: &HashMap<&'a str, FragmentEdge<'a>, DefaultHashBuilder, &'a Bump>,
    defined_vars: &Vec<&'a str>,
    used_fragments: &Vec<&'a str>,
) -> bool {
    for fragment_name in used_fragments {
        if !visited.contains(fragment_name) {
            visited.push(fragment_name);
            if let Some(edge) = fragment_edges.get(fragment_name) {
                for var in edge.used_vars.iter() {
                    if !defined_vars.contains(var) {
                        return true;
                    }
                }
                if references_undefined_var(
                    visited,
                    fragment_edges,
                    defined_vars,
                    &edge.used_fragments,
                ) {
                    return true;
                }
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defined_vars() {
        let ctx = ASTContext::new();
        let document = Document::parse(&ctx, "query($var: Int) { field(x: $var), ...Frag } fragment Frag on Query { field(x: $var) } ").unwrap();
        NoUndefinedVariables::validate(&ctx, document).unwrap();
    }

    #[test]
    fn undefined_vars_on_operation() {
        let ctx = ASTContext::new();
        let document = Document::parse(&ctx, "query { field(x: $var) }").unwrap();
        NoUndefinedVariables::validate(&ctx, document).unwrap_err();
    }

    #[test]
    fn undefined_vars_on_fragments() {
        let ctx = ASTContext::new();
        let document = Document::parse(
            &ctx,
            "query { ...Frag } fragment Frag on Query { field(x: $var) } ",
        )
        .unwrap();
        NoUndefinedVariables::validate(&ctx, document).unwrap_err();
        let document = Document::parse(
            &ctx,
            "query { ...A } fragment A on A { ...B } fragment B on B { field(x: $var) } ",
        )
        .unwrap();
        NoUndefinedVariables::validate(&ctx, document).unwrap_err();
    }
}
