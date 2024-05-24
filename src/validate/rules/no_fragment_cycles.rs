use bumpalo::{collections::Vec, Bump};
use hashbrown::{hash_map::DefaultHashBuilder, HashMap};

use super::super::{ValidationContext, ValidationRule};
use crate::{ast::*, visit::*};

/// Validate that a document does not contain fragments that are spread within themselves, creating a loop.
///
/// See [`ValidationRule`]
/// [Reference](https://spec.graphql.org/October2021/#sec-Fragment-spreads-must-not-form-cycles)
pub struct NoFragmentCycles<'a> {
    fragment_edges: HashMap<&'a str, Vec<'a, &'a str>, DefaultHashBuilder, &'a Bump>,
    used_fragments: Vec<'a, &'a str>,
}

impl<'a> DefaultIn<'a> for NoFragmentCycles<'a> {
    fn default_in(arena: &'a bumpalo::Bump) -> Self {
        Self {
            fragment_edges: HashMap::new_in(arena),
            used_fragments: Vec::new_in(arena),
        }
    }
}

impl<'a> ValidationRule<'a> for NoFragmentCycles<'a> {}

impl<'a> Visitor<'a, ValidationContext<'a>> for NoFragmentCycles<'a> {
    fn enter_operation(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        _operation: &'a OperationDefinition<'a>,
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
        self.used_fragments.clear();
        VisitFlow::Next
    }

    fn leave_fragment(
        &mut self,
        _ctx: &mut ValidationContext<'a>,
        fragment: &'a FragmentDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        let name = fragment.name.name;
        self.fragment_edges
            .insert(name, self.used_fragments.clone());
        self.used_fragments.clear();
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
        for (name, _) in self.fragment_edges.iter() {
            if contains_edge(&mut visited, name, name, &self.fragment_edges) {
                ctx.add_error("Cannot spread fragments within themselves");
                return VisitFlow::Break;
            }
            visited.clear();
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

fn contains_edge<'a>(
    visited: &mut Vec<&'a str>,
    toplevel_name: &'a str,
    current_name: &'a str,
    fragment_edges: &HashMap<&'a str, Vec<&'a str>, DefaultHashBuilder, &'a Bump>,
) -> bool {
    if visited.contains(&current_name) {
        true
    } else if let Some(edges) = fragment_edges.get(current_name) {
        visited.push(current_name);
        if edges.contains(&toplevel_name) {
            true
        } else {
            for next_name in edges {
                if contains_edge(visited, toplevel_name, next_name, fragment_edges) {
                    return true;
                }
            }
            false
        }
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_fragment_spreads() {
        let ctx = ASTContext::new();
        let document = Document::parse(
            &ctx,
            "fragment A on A { ...B } fragment B on B { __typename }",
        )
        .unwrap();
        NoFragmentCycles::validate(&ctx, document).unwrap();
    }

    #[test]
    fn cycling_fragment_spreads() {
        let ctx = ASTContext::new();
        let document =
            Document::parse(&ctx, "fragment A on A { ...B } fragment B on B { ...A }").unwrap();
        NoFragmentCycles::validate(&ctx, document).unwrap_err();
        let document = Document::parse(
            &ctx,
            "fragment A on A { ...B } fragment B on B { ...C } fragment C on C { ...A }",
        )
        .unwrap();
        NoFragmentCycles::validate(&ctx, document).unwrap_err();
        let document = Document::parse(&ctx, "fragment D on D { ...C } fragment A on A { ...B } fragment B on B { ...C } fragment C on C { ...A }").unwrap();
        NoFragmentCycles::validate(&ctx, document).unwrap_err();
        let document = Document::parse(&ctx, "fragment D on D { ...E } fragment A on A { ...B } fragment B on B { ...C } fragment C on C { ...A } fragment E on E { __typename }").unwrap();
        NoFragmentCycles::validate(&ctx, document).unwrap_err();
    }
}
