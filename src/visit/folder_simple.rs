use super::folder::Folder;
use super::VisitInfo;
use crate::ast::*;
use crate::error::Result;

/// This trait is a simplified version of the [Folder] trait for ease of use.
///
/// All structures implementing `SimpleFolder` automatically implement the `Folder` trait.
///
/// Its callbacks don't receive the AST Context and only return the AST Nodes themselves a `Result`
/// having to wrap them.
pub trait SimpleFolder<'a> {
    /// Folds an [`OperationDefinition`] into a new node as part of a new, transformed AST.
    #[inline]
    fn operation(&mut self, operation: OperationDefinition<'a>) -> OperationDefinition<'a> {
        operation
    }

    /// Folds a [`FragmentDefinition`] into a new node as part of a new, transformed AST.
    #[inline]
    fn fragment(&mut self, fragment: FragmentDefinition<'a>) -> FragmentDefinition<'a> {
        fragment
    }

    /// Folds a [`VariableDefinitions`] node into a new node as part of a new, transformed AST.
    #[inline]
    fn variable_definitions(
        &mut self,
        var_defs: VariableDefinitions<'a>,
    ) -> VariableDefinitions<'a> {
        var_defs
    }

    /// Folds a [`VariableDefinition`] into a new node as part of a new, transformed AST.
    #[inline]
    fn variable_definition(&mut self, var_def: VariableDefinition<'a>) -> VariableDefinition<'a> {
        var_def
    }

    /// Folds a [`SelectionSet`] into a new node as part of a new, transformed AST.
    #[inline]
    fn selection_set(&mut self, selection_set: SelectionSet<'a>) -> SelectionSet<'a> {
        selection_set
    }

    /// Folds a [`FragmentSpread`] node into a new node as part of a new, transformed AST.
    #[inline]
    fn fragment_spread(&mut self, fragment_spread: FragmentSpread<'a>) -> FragmentSpread<'a> {
        fragment_spread
    }

    /// Folds an [`InlineFragment`] into a new node as part of a new, transformed AST.
    #[inline]
    fn inline_fragment(&mut self, inline_fragment: InlineFragment<'a>) -> InlineFragment<'a> {
        inline_fragment
    }

    /// Folds a [Field] into a new node as part of a new, transformed AST.
    #[inline]
    fn field(&mut self, field: Field<'a>) -> Field<'a> {
        field
    }

    /// Folds a [Directives] node into a new node as part of a new, transformed AST.
    #[inline]
    fn directives(&mut self, directives: Directives<'a>) -> Directives<'a> {
        directives
    }

    /// Folds a [Directive] into a new node as part of a new, transformed AST.
    #[inline]
    fn directive(&mut self, directive: Directive<'a>) -> Directive<'a> {
        directive
    }

    /// Folds a [Arguments] node into a new node as part of a new, transformed AST.
    #[inline]
    fn arguments(&mut self, arguments: Arguments<'a>) -> Arguments<'a> {
        arguments
    }

    /// Folds an [Argument] into a new node as part of a new, transformed AST.
    #[inline]
    fn argument(&mut self, argument: Argument<'a>) -> Argument<'a> {
        argument
    }

    /// Folds a [Value] node into a new node as part of a new, transformed AST.
    #[inline]
    fn value(&mut self, value: Value<'a>) -> Value<'a> {
        value
    }

    /// Folds a [Type] node into a new node as part of a new, transformed AST.
    #[inline]
    fn of_type(&mut self, of_type: Type<'a>) -> Type<'a> {
        of_type
    }

    /// Folds a [Variable] node into a new node as part of a new, transformed AST.
    #[inline]
    fn variable(&mut self, var: Variable<'a>) -> Variable<'a> {
        var
    }

    /// Folds a [`NamedType`] node into a new node as part of a new, transformed AST.
    #[inline]
    fn named_type(&mut self, name: NamedType<'a>) -> NamedType<'a> {
        name
    }
}

impl<'a, F: SimpleFolder<'a>> Folder<'a> for F {
    #[inline]
    fn enter_operation(
        &mut self,
        _ctx: &'a ASTContext,
        operation: OperationDefinition<'a>,
        _info: &VisitInfo,
    ) -> Result<OperationDefinition<'a>> {
        Ok(SimpleFolder::operation(self, operation))
    }

    #[inline]
    fn leave_operation(
        &mut self,
        _ctx: &'a ASTContext,
        operation: OperationDefinition<'a>,
        _info: &VisitInfo,
    ) -> Result<OperationDefinition<'a>> {
        Ok(operation)
    }

    #[inline]
    fn enter_fragment(
        &mut self,
        _ctx: &'a ASTContext,
        fragment: FragmentDefinition<'a>,
        _info: &VisitInfo,
    ) -> Result<FragmentDefinition<'a>> {
        Ok(SimpleFolder::fragment(self, fragment))
    }

    #[inline]
    fn leave_fragment(
        &mut self,
        _ctx: &'a ASTContext,
        fragment: FragmentDefinition<'a>,
        _info: &VisitInfo,
    ) -> Result<FragmentDefinition<'a>> {
        Ok(fragment)
    }

    #[inline]
    fn variable_definitions(
        &mut self,
        _ctx: &'a ASTContext,
        var_defs: VariableDefinitions<'a>,
        _info: &VisitInfo,
    ) -> Result<VariableDefinitions<'a>> {
        Ok(SimpleFolder::variable_definitions(self, var_defs))
    }

    #[inline]
    fn variable_definition(
        &mut self,
        _ctx: &'a ASTContext,
        var_def: VariableDefinition<'a>,
        _info: &VisitInfo,
    ) -> Result<VariableDefinition<'a>> {
        Ok(SimpleFolder::variable_definition(self, var_def))
    }

    #[inline]
    fn selection_set(
        &mut self,
        _ctx: &'a ASTContext,
        selection_set: SelectionSet<'a>,
        _info: &VisitInfo,
    ) -> Result<SelectionSet<'a>> {
        Ok(SimpleFolder::selection_set(self, selection_set))
    }

    #[inline]
    fn enter_fragment_spread(
        &mut self,
        _ctx: &'a ASTContext,
        fragment_spread: FragmentSpread<'a>,
        _info: &VisitInfo,
    ) -> Result<FragmentSpread<'a>> {
        Ok(SimpleFolder::fragment_spread(self, fragment_spread))
    }

    #[inline]
    fn leave_fragment_spread(
        &mut self,
        _ctx: &'a ASTContext,
        fragment_spread: FragmentSpread<'a>,
        _info: &VisitInfo,
    ) -> Result<FragmentSpread<'a>> {
        Ok(fragment_spread)
    }

    #[inline]
    fn enter_inline_fragment(
        &mut self,
        _ctx: &'a ASTContext,
        inline_fragment: InlineFragment<'a>,
        _info: &VisitInfo,
    ) -> Result<InlineFragment<'a>> {
        Ok(SimpleFolder::inline_fragment(self, inline_fragment))
    }

    #[inline]
    fn leave_inline_fragment(
        &mut self,
        _ctx: &'a ASTContext,
        inline_fragment: InlineFragment<'a>,
        _info: &VisitInfo,
    ) -> Result<InlineFragment<'a>> {
        Ok(inline_fragment)
    }

    #[inline]
    fn enter_field(
        &mut self,
        _ctx: &'a ASTContext,
        field: Field<'a>,
        _info: &VisitInfo,
    ) -> Result<Field<'a>> {
        Ok(SimpleFolder::field(self, field))
    }

    #[inline]
    fn leave_field(
        &mut self,
        _ctx: &'a ASTContext,
        field: Field<'a>,
        _info: &VisitInfo,
    ) -> Result<Field<'a>> {
        Ok(field)
    }

    #[inline]
    fn directives(
        &mut self,
        _ctx: &'a ASTContext,
        directives: Directives<'a>,
        _info: &VisitInfo,
    ) -> Result<Directives<'a>> {
        Ok(SimpleFolder::directives(self, directives))
    }

    #[inline]
    fn enter_directive(
        &mut self,
        _ctx: &'a ASTContext,
        directive: Directive<'a>,
        _info: &VisitInfo,
    ) -> Result<Directive<'a>> {
        Ok(SimpleFolder::directive(self, directive))
    }

    #[inline]
    fn leave_directive(
        &mut self,
        _ctx: &'a ASTContext,
        directive: Directive<'a>,
        _info: &VisitInfo,
    ) -> Result<Directive<'a>> {
        Ok(directive)
    }

    #[inline]
    fn arguments(
        &mut self,
        _ctx: &'a ASTContext,
        arguments: Arguments<'a>,
        _info: &VisitInfo,
    ) -> Result<Arguments<'a>> {
        Ok(SimpleFolder::arguments(self, arguments))
    }

    #[inline]
    fn argument(
        &mut self,
        _ctx: &'a ASTContext,
        argument: Argument<'a>,
        _info: &VisitInfo,
    ) -> Result<Argument<'a>> {
        Ok(SimpleFolder::argument(self, argument))
    }

    #[inline]
    fn value(
        &mut self,
        _ctx: &'a ASTContext,
        value: Value<'a>,
        _info: &VisitInfo,
    ) -> Result<Value<'a>> {
        Ok(SimpleFolder::value(self, value))
    }

    #[inline]
    fn of_type(
        &mut self,
        _ctx: &'a ASTContext,
        of_type: Type<'a>,
        _info: &VisitInfo,
    ) -> Result<Type<'a>> {
        Ok(SimpleFolder::of_type(self, of_type))
    }

    #[inline]
    fn variable(
        &mut self,
        _ctx: &'a ASTContext,
        var: Variable<'a>,
        _info: &VisitInfo,
    ) -> Result<Variable<'a>> {
        Ok(SimpleFolder::variable(self, var))
    }

    #[inline]
    fn named_type(
        &mut self,
        _ctx: &'a ASTContext,
        name: NamedType<'a>,
        _info: &VisitInfo,
    ) -> Result<NamedType<'a>> {
        Ok(SimpleFolder::named_type(self, name))
    }
}
