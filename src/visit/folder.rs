use crate::ast::*;
use bumpalo::collections::Vec;

pub use super::{folder_simple::SimpleFolder, Path, PathSegment, VisitInfo};
pub use crate::error::{Error, Result};

pub(crate) mod private {
    use hashbrown::{hash_map::DefaultHashBuilder, HashMap};
    use std::cell::RefCell;

    use super::{ASTContext, Definition, Document, Folder, Result, Vec};
    use crate::{ast::FragmentDefinitionWithIndex, visit::VisitInfo};

    /// Private Folder context state that's kept to keep track of the current folding progress and
    /// state. This contains the AST context and optional records on fragments and the new document's
    /// definition if the `Folder` is being traversed by operation.
    pub struct FolderContext<'a> {
        pub(crate) ctx: &'a ASTContext,
        pub(crate) definitions: RefCell<Vec<'a, Definition<'a>>>,
        pub(crate) fragments: RefCell<
            HashMap<
                &'a str,
                FragmentDefinitionWithIndex<'a>,
                DefaultHashBuilder,
                &'a bumpalo::Bump,
            >,
        >,
        pub(crate) fragment_names:
            RefCell<HashMap<&'a str, &'a str, DefaultHashBuilder, &'a bumpalo::Bump>>,
        pub(crate) recurse: bool,
    }

    impl<'a> FolderContext<'a> {
        pub(crate) fn empty(ctx: &'a ASTContext) -> Self {
            FolderContext {
                ctx,
                fragments: RefCell::new(HashMap::new_in(&ctx.arena)),
                fragment_names: RefCell::new(HashMap::new_in(&ctx.arena)),
                definitions: RefCell::new(Vec::new_in(&ctx.arena)),
                recurse: false,
            }
        }

        pub(crate) fn new_vec<T>(&self) -> Vec<T> {
            Vec::new_in(&self.ctx.arena)
        }

        pub(crate) fn with_document(ctx: &'a ASTContext, document: &'a Document<'a>) -> Self {
            FolderContext {
                ctx,
                fragments: RefCell::new(document.fragments_with_index(ctx)),
                fragment_names: RefCell::new(HashMap::new_in(&ctx.arena)),
                definitions: RefCell::new(Vec::new_in(&ctx.arena)),
                recurse: true,
            }
        }
    }

    pub trait FoldNode<'a>: Sized {
        fn fold_with_ctx<'b, F: Folder<'a>>(
            &self,
            info: &mut VisitInfo,
            ctx: &'a FolderContext<'a>,
            folder: &'b mut F,
        ) -> Result<Self>;
    }
}

/// Trait for a folder that carries methods that are called as callback while AST nodes
/// implementing the folder pattern are traversed and edited.
///
/// A Folder is used to traverse an GraphQL AST top-to-bottom, depth-first and to replace the AST's
/// nodes by calling the Folder's callbacks and replacing the AST nodes one by one. After an AST
/// Node is folded it's an entirely new copy, separate from the input AST, which remains untouched.
///
/// All callbacks have a default no-op implementation that return the input AST Node and hence only
/// create an unchanged copy of the AST.
/// The callbacks receive a reference to the [`ASTContext`] and must return the altered (or
/// unchanged) node that's placed into the new AST using a [Result]. This can be used to also
/// return an error and stop the folding.
///
/// This pattern is applicable to any AST node that implements the [`FoldNode`] trait.
pub trait Folder<'a> {
    /// Folds an [`OperationDefinition`] into a new node as part of a new, transformed AST, before
    /// the Operation is folded recursively
    #[inline]
    fn enter_operation(
        &mut self,
        _ctx: &'a ASTContext,
        operation: OperationDefinition<'a>,
        _info: &VisitInfo,
    ) -> Result<OperationDefinition<'a>> {
        Ok(operation)
    }

    /// Folds an [`OperationDefinition`] into a new node as part of a new, transformed AST, after
    /// the Operation has been folded.
    #[inline]
    fn leave_operation(
        &mut self,
        _ctx: &'a ASTContext,
        operation: OperationDefinition<'a>,
        _info: &VisitInfo,
    ) -> Result<OperationDefinition<'a>> {
        Ok(operation)
    }

    /// Folds a [`FragmentDefinition`] into a new node as part of a new, transformed AST, before the
    /// FragmentDefinition is folded recursively.
    #[inline]
    fn enter_fragment(
        &mut self,
        _ctx: &'a ASTContext,
        fragment: FragmentDefinition<'a>,
        _info: &VisitInfo,
    ) -> Result<FragmentDefinition<'a>> {
        Ok(fragment)
    }

    /// Folds a [`FragmentDefinition`] into a new node as part of a new, transformed AST, after the
    /// FragmentDefinition has been folded.
    #[inline]
    fn leave_fragment(
        &mut self,
        _ctx: &'a ASTContext,
        fragment: FragmentDefinition<'a>,
        _info: &VisitInfo,
    ) -> Result<FragmentDefinition<'a>> {
        Ok(fragment)
    }

    /// Folds a [`VariableDefinitions`] node into a new node as part of a new, transformed AST.
    #[inline]
    fn variable_definitions(
        &mut self,
        _ctx: &'a ASTContext,
        var_defs: VariableDefinitions<'a>,
        _info: &VisitInfo,
    ) -> Result<VariableDefinitions<'a>> {
        Ok(var_defs)
    }

    /// Folds a [`VariableDefinition`] into a new node as part of a new, transformed AST.
    #[inline]
    fn variable_definition(
        &mut self,
        _ctx: &'a ASTContext,
        var_def: VariableDefinition<'a>,
        _info: &VisitInfo,
    ) -> Result<VariableDefinition<'a>> {
        Ok(var_def)
    }

    /// Folds a [`SelectionSet`] into a new node as part of a new, transformed AST.
    #[inline]
    fn selection_set(
        &mut self,
        _ctx: &'a ASTContext,
        selection_set: SelectionSet<'a>,
        _info: &VisitInfo,
    ) -> Result<SelectionSet<'a>> {
        Ok(selection_set)
    }

    /// Folds a [`FragmentSpread`] node into a new node as part of a new, transformed AST, before
    /// the FragmentSpread is folded recursively.
    #[inline]
    fn enter_fragment_spread(
        &mut self,
        _ctx: &'a ASTContext,
        fragment_spread: FragmentSpread<'a>,
        _info: &VisitInfo,
    ) -> Result<FragmentSpread<'a>> {
        Ok(fragment_spread)
    }

    /// Folds a [`FragmentSpread`] node into a new node as part of a new, transformed AST, after
    /// the FragmentSpread has been folded.
    #[inline]
    fn leave_fragment_spread(
        &mut self,
        _ctx: &'a ASTContext,
        fragment_spread: FragmentSpread<'a>,
        _info: &VisitInfo,
    ) -> Result<FragmentSpread<'a>> {
        Ok(fragment_spread)
    }

    /// Folds an [`InlineFragment`] into a new node as part of a new, transformed AST, before the
    /// InlineFragment is folded recursively.
    #[inline]
    fn enter_inline_fragment(
        &mut self,
        _ctx: &'a ASTContext,
        inline_fragment: InlineFragment<'a>,
        _info: &VisitInfo,
    ) -> Result<InlineFragment<'a>> {
        Ok(inline_fragment)
    }

    /// Folds an [`InlineFragment`] into a new node as part of a new, transformed AST, after the
    /// InlineFragment has been folded.
    #[inline]
    fn leave_inline_fragment(
        &mut self,
        _ctx: &'a ASTContext,
        inline_fragment: InlineFragment<'a>,
        _info: &VisitInfo,
    ) -> Result<InlineFragment<'a>> {
        Ok(inline_fragment)
    }

    /// Folds a [Field] into a new node as part of a new, transformed AST, before the Field is
    /// folded recursively.
    #[inline]
    fn enter_field(
        &mut self,
        _ctx: &'a ASTContext,
        field: Field<'a>,
        _info: &VisitInfo,
    ) -> Result<Field<'a>> {
        Ok(field)
    }

    /// Folds a [Field] into a new node as part of a new, transformed AST, after the field has been
    /// folded.
    #[inline]
    fn leave_field(
        &mut self,
        _ctx: &'a ASTContext,
        field: Field<'a>,
        _info: &VisitInfo,
    ) -> Result<Field<'a>> {
        Ok(field)
    }

    /// Folds a [Directives] node into a new node as part of a new, transformed AST.
    #[inline]
    fn directives(
        &mut self,
        _ctx: &'a ASTContext,
        directives: Directives<'a>,
        _info: &VisitInfo,
    ) -> Result<Directives<'a>> {
        Ok(directives)
    }

    /// Folds a [Directive] into a new node as part of a new, transformed AST, before the Directive
    /// is folded recursively.
    #[inline]
    fn enter_directive(
        &mut self,
        _ctx: &'a ASTContext,
        directive: Directive<'a>,
        _info: &VisitInfo,
    ) -> Result<Directive<'a>> {
        Ok(directive)
    }

    /// Folds a [Directive] into a new node as part of a new, transformed AST, after the Directive
    /// has been folded.
    #[inline]
    fn leave_directive(
        &mut self,
        _ctx: &'a ASTContext,
        directive: Directive<'a>,
        _info: &VisitInfo,
    ) -> Result<Directive<'a>> {
        Ok(directive)
    }

    /// Folds a [Arguments] node into a new node as part of a new, transformed AST.
    #[inline]
    fn arguments(
        &mut self,
        _ctx: &'a ASTContext,
        arguments: Arguments<'a>,
        _info: &VisitInfo,
    ) -> Result<Arguments<'a>> {
        Ok(arguments)
    }

    /// Folds an [Argument] into a new node as part of a new, transformed AST.
    #[inline]
    fn argument(
        &mut self,
        _ctx: &'a ASTContext,
        argument: Argument<'a>,
        _info: &VisitInfo,
    ) -> Result<Argument<'a>> {
        Ok(argument)
    }

    /// Folds a [Value] node into a new node as part of a new, transformed AST.
    #[inline]
    fn value(
        &mut self,
        _ctx: &'a ASTContext,
        value: Value<'a>,
        _info: &VisitInfo,
    ) -> Result<Value<'a>> {
        Ok(value)
    }

    /// Folds a [Type] node into a new node as part of a new, transformed AST.
    #[inline]
    fn of_type(
        &mut self,
        _ctx: &'a ASTContext,
        of_type: Type<'a>,
        _info: &VisitInfo,
    ) -> Result<Type<'a>> {
        Ok(of_type)
    }

    /// Folds a [Variable] node into a new node as part of a new, transformed AST.
    #[inline]
    fn variable(
        &mut self,
        _ctx: &'a ASTContext,
        var: Variable<'a>,
        _info: &VisitInfo,
    ) -> Result<Variable<'a>> {
        Ok(var)
    }

    /// Folds a [`NamedType`] node into a new node as part of a new, transformed AST.
    #[inline]
    fn named_type(
        &mut self,
        _ctx: &'a ASTContext,
        name: NamedType<'a>,
        _info: &VisitInfo,
    ) -> Result<NamedType<'a>> {
        Ok(name)
    }
}

/// Trait for folding AST Nodes of a GraphQL language document in depth-first order using a
/// custom folder. This transforms the AST while creating a new copy of it.
///
/// The folder must implement the [Folder] trait.
pub trait FoldNode<'a>: private::FoldNode<'a> {
    /// Visit the GraphQL AST node tree recursively in depth-first order and create a transformed
    /// copy of it using the given folder. The folder must implement the [Folder] trait.
    ///
    /// This will return a [Result] containing a reference to the new copied AST Node allocated on
    /// the current AST Context's arena or an error.
    fn fold<'b, F: Folder<'a>>(
        &'a self,
        ctx: &'a ASTContext,
        folder: &'b mut F,
    ) -> Result<&'a Self> {
        let mut info = VisitInfo::default();
        let folder_ctx = ctx.alloc(private::FolderContext::empty(ctx));
        Ok(ctx.alloc(self.fold_with_ctx(&mut info, folder_ctx, folder)?))
    }
}

/// Trait for folding a GraphQL Document AST Node by traversing an operation instead of the entire
/// AST tree. This method alters the traversal of the folder and traverses starting from an operation
/// instead; folding the fragment definitions only as they're used and refered to using
/// `FragmentSpread` nodes in the operation.
///
/// If a Document should instead be transformed and copied in its entirety then `Document::fold` is
/// a better choice.
pub trait FoldDocument<'a>: private::FoldNode<'a> {
    /// Folds a GraphQL document by a given operation instead. Instead of transforming the given
    /// document in its entirety `fold_operation` will start at the defined operation instead,
    /// transforming fragments only as they're referred to via `FragmentSpread` nodes. This will
    /// create a new document that only refers to and contains the specified `operation`.
    fn fold_operation<'b, F: Folder<'a>>(
        &'a self,
        ctx: &'a ASTContext,
        operation: Option<&'a str>,
        folder: &'b mut F,
    ) -> Result<&'a Self>;
}

impl<'a, T: private::FoldNode<'a>> FoldNode<'a> for T {}

impl<'a> private::FoldNode<'a> for Type<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'b private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        folder.of_type(ctx.ctx, *self, info)
    }
}

impl<'a> private::FoldNode<'a> for Value<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        folder.value(ctx.ctx, self.clone(), info)
    }
}

impl<'a> private::FoldNode<'a> for NamedType<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        folder.named_type(ctx.ctx, *self, info)
    }
}

impl<'a> private::FoldNode<'a> for Variable<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        folder.variable(ctx.ctx, *self, info)
    }
}

impl<'a> private::FoldNode<'a> for Argument<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        let argument = folder.argument(ctx.ctx, self.clone(), info)?;

        info.path.push(PathSegment::Value);
        let value = argument.value.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        Ok(Argument {
            name: argument.name,
            value,
        })
    }
}

impl<'a> private::FoldNode<'a> for Arguments<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        let new_arguments_iter = {
            folder
                .arguments(ctx.ctx, self.clone(), info)?
                .into_iter()
                .enumerate()
                .map(|(index, argument)| {
                    info.path.push(PathSegment::Index(index));
                    let folded = argument.fold_with_ctx(info, ctx, folder);
                    info.path.pop();
                    folded
                })
        };

        let new_arguments = {
            let mut new_arguments = ctx.new_vec();
            for item in new_arguments_iter {
                new_arguments.push(item?);
            }

            new_arguments
        };

        Ok(Arguments {
            children: new_arguments,
        })
    }
}

impl<'a> private::FoldNode<'a> for Directive<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        let directive = &folder.enter_directive(ctx.ctx, self.clone(), info)?;

        info.path.push(PathSegment::Arguments);
        let arguments = directive.arguments.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        let directive = Directive {
            name: directive.name,
            arguments,
        };
        folder.leave_directive(ctx.ctx, directive, info)
    }
}

impl<'a> private::FoldNode<'a> for Directives<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        // TODO: how to make this Result dissapear
        let new_directives_iter = folder
            .directives(ctx.ctx, self.clone(), info)?
            .into_iter()
            .enumerate()
            .map(|(index, directive)| {
                info.path.push(PathSegment::Index(index));
                let folded = directive.fold_with_ctx(info, ctx, folder);
                info.path.pop();
                folded
            });

        let mut new_directives = Vec::new_in(&ctx.ctx.arena);
        for item in new_directives_iter {
            new_directives.push(item?);
        }
        Ok(Directives {
            children: new_directives,
        })
    }
}

impl<'a> private::FoldNode<'a> for VariableDefinition<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        let var_def = folder.variable_definition(ctx.ctx, self.clone(), info)?;

        info.path.push(PathSegment::Variable);
        let variable = var_def.variable.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        info.path.push(PathSegment::Type);
        let of_type = var_def.of_type.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        info.path.push(PathSegment::Value);
        let default_value = var_def.default_value.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        info.path.push(PathSegment::Directives);
        let directives = var_def.directives.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        Ok(VariableDefinition {
            variable,
            of_type,
            default_value,
            directives,
        })
    }
}

impl<'a> private::FoldNode<'a> for VariableDefinitions<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        let new_variables_iter = folder
            .variable_definitions(ctx.ctx, self.clone(), info)?
            .into_iter()
            .enumerate()
            .map(|(index, var_def)| {
                info.path.push(PathSegment::Index(index));
                let folded = var_def.fold_with_ctx(info, ctx, folder);
                info.path.pop();
                folded
            });

        let mut new_variables = Vec::new_in(&ctx.ctx.arena);
        for item in new_variables_iter {
            new_variables.push(item?);
        }
        Ok(VariableDefinitions {
            children: new_variables,
        })
    }
}

impl<'a> private::FoldNode<'a> for Field<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        let field = &folder.enter_field(ctx.ctx, self.clone(), info)?;

        info.path.push(PathSegment::Arguments);
        let arguments = field.arguments.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        info.path.push(PathSegment::Directives);
        let directives = field.directives.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        info.path.push(PathSegment::SelectionSet);
        let selection_set = field.selection_set.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        let field = Field {
            alias: field.alias,
            name: field.name,
            arguments,
            directives,
            selection_set,
        };
        folder.leave_field(ctx.ctx, field, info)
    }
}

impl<'a> private::FoldNode<'a> for FragmentSpread<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        let spread = &folder.enter_fragment_spread(ctx.ctx, self.clone(), info)?;
        let spread = if ctx.recurse {
            let fragment_name = spread.name.name;
            let mut borrowed_fragment_names = ctx.fragment_names.borrow_mut();
            let fragment_name =
                if let Some(fragment_name) = borrowed_fragment_names.get(fragment_name) {
                    fragment_name
                } else if let Some(FragmentDefinitionWithIndex { fragment, index }) =
                    ctx.fragments.borrow().get(spread.name.name)
                {
                    let path = info.path.clone();
                    info.path = Path::default();
                    info.path.push(PathSegment::Index(index.to_owned()));
                    let fragment = fragment.fold_with_ctx(info, ctx, folder)?;
                    info.path = path;

                    let fragment_name = fragment.name.name;
                    // TODO: was prepend before
                    ctx.definitions
                        .borrow_mut()
                        .push(Definition::Fragment(fragment));
                    borrowed_fragment_names.insert(fragment_name, fragment_name);
                    fragment_name
                } else {
                    return Err(Error::new(
                        format!("The fragment '{}' does not exist", fragment_name),
                        None,
                    ));
                };

            info.path.push(PathSegment::Directives);
            let directives = spread.directives.fold_with_ctx(info, ctx, folder)?;
            info.path.pop();

            FragmentSpread {
                name: fragment_name.into(),
                directives,
            }
        } else {
            info.path.push(PathSegment::Name);
            let name = spread.name.fold_with_ctx(info, ctx, folder)?;
            info.path.pop();

            info.path.push(PathSegment::Directives);
            let directives = spread.directives.fold_with_ctx(info, ctx, folder)?;
            info.path.pop();

            FragmentSpread { name, directives }
        };
        folder.leave_fragment_spread(ctx.ctx, spread, info)
    }
}

impl<'a> private::FoldNode<'a> for InlineFragment<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        let fragment = folder.enter_inline_fragment(ctx.ctx, self.clone(), info)?;

        let type_condition = match fragment.type_condition {
            Some(condition) => {
                info.path.push(PathSegment::Name);
                let folded = condition.fold_with_ctx(info, ctx, folder)?;
                info.path.pop();
                Some(folded)
            }
            None => None,
        };

        info.path.push(PathSegment::Directives);
        let directives = fragment.directives.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        info.path.push(PathSegment::SelectionSet);
        let selection_set = fragment.selection_set.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        let fragment = InlineFragment {
            type_condition,
            directives,
            selection_set,
        };
        folder.leave_inline_fragment(ctx.ctx, fragment, info)
    }
}

impl<'a> private::FoldNode<'a> for SelectionSet<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        let new_selections_iter = folder
            .selection_set(ctx.ctx, self.clone(), info)?
            .into_iter()
            .enumerate()
            .map(|(index, selection)| -> Result<Selection> {
                info.path.push(PathSegment::Index(index));
                let folded = match selection {
                    Selection::Field(field) => Ok(field.fold_with_ctx(info, ctx, folder)?.into()),
                    Selection::FragmentSpread(spread) => {
                        Ok(spread.fold_with_ctx(info, ctx, folder)?.into())
                    }
                    Selection::InlineFragment(fragment) => {
                        Ok(fragment.fold_with_ctx(info, ctx, folder)?.into())
                    }
                };
                info.path.pop();
                folded
            });

        let mut new_selections = Vec::new_in(&ctx.ctx.arena);
        for item in new_selections_iter {
            new_selections.push(item?);
        }
        Ok(SelectionSet {
            selections: new_selections,
        })
    }
}

impl<'a> private::FoldNode<'a> for FragmentDefinition<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        let fragment = &folder.enter_fragment(ctx.ctx, self.clone(), info)?;

        info.path.push(PathSegment::Name);
        let name = fragment.name.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        info.path.push(PathSegment::Type);
        let type_condition = fragment.type_condition.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        info.path.push(PathSegment::Directives);
        let directives = fragment.directives.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        info.path.push(PathSegment::SelectionSet);
        let selection_set = fragment.selection_set.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        let fragment = FragmentDefinition {
            name,
            type_condition,
            directives,
            selection_set,
        };
        folder.leave_fragment(ctx.ctx, fragment, info)
    }
}

impl<'a> private::FoldNode<'a> for OperationDefinition<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        let operation = &folder.enter_operation(ctx.ctx, self.clone(), info)?;

        info.path.push(PathSegment::VariableDefinitions);
        let variable_definitions = operation
            .variable_definitions
            .fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        info.path.push(PathSegment::Directives);
        let directives = operation.directives.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        info.path.push(PathSegment::SelectionSet);
        let selection_set = operation.selection_set.fold_with_ctx(info, ctx, folder)?;
        info.path.pop();

        let operation = OperationDefinition {
            operation: operation.operation,
            name: operation.name,
            variable_definitions,
            directives,
            selection_set,
        };
        folder.leave_operation(ctx.ctx, operation, info)
    }
}

impl<'a> private::FoldNode<'a> for Document<'a> {
    #[inline]
    fn fold_with_ctx<'b, F: Folder<'a>>(
        &self,
        info: &mut VisitInfo,
        ctx: &'a private::FolderContext<'a>,
        folder: &'b mut F,
    ) -> Result<Self> {
        let new_definitions_iter =
            self.definitions
                .iter()
                .enumerate()
                .map(|(index, selection)| -> Result<Definition> {
                    info.path.push(PathSegment::Index(index));
                    let folded = match selection {
                        Definition::Operation(operation) => {
                            Ok(operation.fold_with_ctx(info, ctx, folder)?.into())
                        }
                        Definition::Fragment(fragment) => {
                            Ok(fragment.fold_with_ctx(info, ctx, folder)?.into())
                        }
                    };
                    info.path.pop();
                    folded
                });
        let mut new_definitions = Vec::new_in(&ctx.ctx.arena);
        for item in new_definitions_iter.rev() {
            new_definitions.push(item?);
        }
        Ok(Document {
            definitions: new_definitions,
            size_hint: self.size_hint,
        })
    }
}

impl<'a> FoldDocument<'a> for Document<'a> {
    /// Folds a GraphQL document by a given operation instead. Instead of transforming the given
    /// document in its entirety `fold_operation` will start at the defined operation instead,
    /// transforming fragments only as they're referred to via `FragmentSpread` nodes. This will
    /// create a new document that only refers to and contains the specified `operation`.
    fn fold_operation<'b, F: Folder<'a>>(
        &'a self,
        ctx: &'a ASTContext,
        operation_name: Option<&'a str>,
        folder: &'b mut F,
    ) -> Result<&'a Self> {
        let (operation, index) = self.operation_with_index(operation_name)?;
        let folder_ctx = ctx.alloc(private::FolderContext::with_document(ctx, self));
        let mut info = VisitInfo::default();
        info.path.push(PathSegment::Index(index));
        let operation = private::FoldNode::fold_with_ctx(operation, &mut info, folder_ctx, folder)?;

        let mut borrowed_definitions = folder_ctx.definitions.clone().into_inner();

        borrowed_definitions.push(Definition::Operation(operation));
        Ok(ctx.alloc(Document {
            definitions: Vec::from_iter_in(borrowed_definitions.into_iter().rev(), &ctx.arena),
            size_hint: self.size_hint,
        }))
    }
}

#[cfg(test)]
mod tests {
    use bumpalo::collections::CollectIn;

    use super::{super::*, FoldNode, Folder, Result};
    use crate::{
        ast::*,
        visit::{folder::FoldDocument, folder_simple::SimpleFolder},
    };

    #[test]
    fn kitchen_sink() {
        #[derive(Default)]
        struct FoldNoop {}
        impl<'a> SimpleFolder<'a> for FoldNoop {}

        let ctx = ASTContext::new();
        let query = include_str!("../../fixture/kitchen_sink.graphql");
        let ast = Document::parse(&ctx, query).unwrap();

        let output = ast
            .fold_operation(&ctx, Some("queryName"), &mut FoldNoop::default())
            .unwrap();

        let actual = output.print();
        let expected = indoc::indoc! {r#"
            query queryName($foo: ComplexType, $site: Site = MOBILE) @onQuery {
              whoever123is: node(id: [123, 456]) {
                id
                ... on User @onInlineFragment {
                  field2 {
                    id
                    alias: field1(first: 10, after: $foo) @include(if: $foo) {
                      id
                      ...frag @onFragmentSpread
                    }
                  }
                }
                ... @skip(unless: $foo) {
                  id
                }
                ... {
                  id
                }
              }
            }

            fragment frag on Friend @onFragmentDefinition {
              foo(size: $site, bar: 12, obj: {key: "value", block: """
              block string uses \"""
              """})
            }"#};

        assert_eq!(actual, expected);
    }

    struct InfoFolder {}

    impl<'a> Folder<'a> for InfoFolder {
        fn enter_fragment_spread(
            &mut self,
            _ctx: &'a ASTContext,
            fragment_spread: FragmentSpread<'a>,
            info: &VisitInfo,
        ) -> Result<FragmentSpread<'a>> {
            // We run this folder on the kitchen sink query which contains
            // exactly one fragment spread at the following location
            let expected_path = Path::try_from(
                "0.selectionSet.0.selectionSet.1.selectionSet.0.selectionSet.1.selectionSet.1",
            )
            .unwrap();
            assert_eq!(info.path, expected_path);
            Ok(fragment_spread)
        }

        fn value(
            &mut self,
            _ctx: &'a ASTContext,
            value: Value<'a>,
            info: &VisitInfo,
        ) -> Result<Value<'a>> {
            if let Value::Object(_) = value {
                // We run this folder on the kitchen sink query which contains
                // exactly one object value at the following location
                let expected_path = Path::try_from("3.selectionSet.0.arguments.2.value").unwrap();
                assert_eq!(info.path, expected_path);
            }

            Ok(value)
        }
    }

    #[test]
    fn fold_info_path() {
        let ctx = ASTContext::new();
        let query = include_str!("../../fixture/kitchen_sink.graphql");
        let ast = Document::parse(&ctx, query).unwrap();

        let mut folder = InfoFolder {};
        let _ = ast.fold(&ctx, &mut folder).unwrap();
    }

    #[test]
    fn fold_operation_info_path() {
        let ctx = ASTContext::new();
        let query = include_str!("../../fixture/kitchen_sink.graphql");
        let ast = Document::parse(&ctx, query).unwrap();

        let mut folder = InfoFolder {};
        let _ = ast
            .fold_operation(&ctx, Some("queryName"), &mut folder)
            .unwrap();
    }

    struct AddTypenames {}

    use std::iter;

    impl<'arena> Folder<'arena> for AddTypenames {
        fn selection_set(
            &mut self,
            ctx: &'arena ASTContext,
            selection_set: SelectionSet<'arena>,
            _info: &VisitInfo,
        ) -> Result<SelectionSet<'arena>> {
            if selection_set.is_empty() {
                return Ok(selection_set);
            }

            let has_typename = selection_set.selections.iter().any(|selection| {
                selection
                    .field()
                    .map(|field| field.name == "__typename" && field.alias.is_none())
                    .unwrap_or(false)
            });

            if !has_typename {
                let typename_field = Selection::Field(Field {
                    alias: None,
                    name: "__typename",
                    arguments: Arguments::default_in(&ctx.arena),
                    directives: Directives::default_in(&ctx.arena),
                    selection_set: SelectionSet::default_in(&ctx.arena),
                });

                let new_selections =
                    selection_set
                        .into_iter()
                        .chain(iter::once(typename_field))
                        .collect_in::<bumpalo::collections::Vec<Selection>>(&ctx.arena);

                Ok(SelectionSet {
                    selections: new_selections,
                })
            } else {
                Ok(selection_set)
            }
        }
    }

    #[test]
    fn fold_typename() {
        let ctx = ASTContext::new();
        let query = "query { todo { id author { id } } }";
        let ast = Document::parse(&ctx, query).unwrap();

        let mut folder = AddTypenames {};
        let new_ast = ast.fold_operation(&ctx, None, &mut folder);
        assert_eq!(
            new_ast.unwrap().print(),
            "{\n  todo {\n    id\n    author {\n      id\n      __typename\n    }\n    __typename\n  }\n  __typename\n}"
        );
    }
}
