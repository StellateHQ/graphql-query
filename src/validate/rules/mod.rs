use crate::visit::ComposedVisitor;

mod known_fragment_names;
mod lone_anonymous_operation;
mod no_fragment_cycles;
mod no_undefined_variables;
mod no_unused_fragments;
mod unique_argument_names;
mod unique_fragment_names;
mod unique_operation_names;
mod unique_variable_names;

pub use super::context::ValidationContext;
pub use known_fragment_names::*;
pub use lone_anonymous_operation::*;
pub use no_fragment_cycles::*;
pub use no_undefined_variables::*;
pub use no_unused_fragments::*;
pub use unique_argument_names::*;
pub use unique_fragment_names::*;
pub use unique_operation_names::*;
pub use unique_variable_names::*;

/// All of GraphQL's validation rules which don't require a schema to be run combined into one
/// `ValidationRule`.
///
/// See: [`ComposedVisitor`]
pub type DefaultRules<'a> = ComposedVisitor<
    'a,
    ValidationContext<'a>,
    ComposedVisitor<
        'a,
        ValidationContext<'a>,
        ComposedVisitor<'a, ValidationContext<'a>, LoneAnonymousOperation, UniqueVariableNames<'a>>,
        ComposedVisitor<
            'a,
            ValidationContext<'a>,
            UniqueFragmentNames<'a>,
            UniqueOperationNames<'a>,
        >,
    >,
    ComposedVisitor<
        'a,
        ValidationContext<'a>,
        ComposedVisitor<'a, ValidationContext<'a>, NoUnusedFragments<'a>, UniqueArgumentNames<'a>>,
        ComposedVisitor<
            'a,
            ValidationContext<'a>,
            KnownFragmentNames<'a>,
            ComposedVisitor<
                'a,
                ValidationContext<'a>,
                NoFragmentCycles<'a>,
                NoUndefinedVariables<'a>,
            >,
        >,
    >,
>;

#[cfg(test)]
mod tests {
    use super::super::*;
    use super::*;
    use crate::ast::*;

    #[test]
    fn passes() {
        let ctx = ASTContext::new();
        let document = Document::parse(
            &ctx,
            "query A { __typename } query B { ...Test } fragment Test on Query { __typename }",
        )
        .unwrap();
        DefaultRules::validate(&ctx, document).unwrap();
    }

    #[test]
    fn fails() {
        let ctx = ASTContext::new();

        let document = Document::parse(
            &ctx,
            "query A { __typename } query B { __typename } fragment Test on Query { __typename }",
        )
        .unwrap();
        DefaultRules::validate(&ctx, document).unwrap_err();

        let document = Document::parse(
            &ctx,
            "query { __typename } query { ...Test } fragment Test on Query { __typename }",
        )
        .unwrap();
        DefaultRules::validate(&ctx, document).unwrap_err();
    }
}
