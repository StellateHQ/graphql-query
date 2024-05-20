use super::{compose::ComposedVisitor, path::Path, PathSegment};
use crate::ast::*;

pub(crate) mod private {
    use super::{VisitFlow, VisitInfo, Visitor};

    pub trait VisitNodeWithInfo<'a>: Sized {
        fn visit_with_info<'b, C, V: Visitor<'a, C>>(
            &'a self,
            ctx: &'b mut C,
            visitor: &'b mut V,
            info: &mut VisitInfo,
        ) -> VisitFlow;
    }
}

/// A visitor signal that is returned from [Visitor] callbacks to alter the flow of traversal.
///
/// The default callbacks all return `VisitFlow::Next`, which continues the depth-first traversal. The
/// other signals may be used to skip over a node in a `before_` callback or to abort traversal
/// entirely without visiting any more AST Nodes.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VisitFlow {
    /// Continue visiting nodes as usual.
    Next,
    /// Abort the traversal without performing any subsequent visits.
    Break,
    /// Skip over the current node without performing any deeper traversal.
    /// (Only applies to `enter_` callbacks)
    Skip,
}

#[derive(Debug, Default)]
pub struct VisitInfo {
    pub path: Path,
}

/// Trait for a visitor that carries methods that are called as callback while AST nodes
/// implementing the visitor pattern are traversed.
///
/// While the AST is traversed in depth-first order, callbacks that are prefixed with `enter_` are
/// called from top-to-bottom while the traversal is recursing, while callbacks that are prefixed
/// with `leave_` are called from bottom-to-top while the traversal is returning.
///
/// All callbacks have a default no-op implementation that returns `VisitFlow::Next`. The
/// [`VisitFlow`] signals are returned from callbacks to alter the traversal and either continue it
/// (`Next`), skip over a node during an `enter_` callback with (`Skip`), or abort traversal
/// entirely (`Break`).
///
/// The visitor must implement the [Visitor] trait which may also define a custom context structure
/// that can be passed to the `visit` method. By default the context is an empty unit `()`.
///
/// This pattern is applicable to any AST node that implements the [`VisitNode`] trait.
pub trait Visitor<'a, Context = ()>: Sized {
    /// Combines two visitors into one that will run both the original and passed visitor concurrently.
    ///
    /// Both visitors must accept the same `Context` type.
    #[inline]
    fn compose<V: Visitor<'a, Context>>(self, other: V) -> ComposedVisitor<'a, Context, Self, V> {
        ComposedVisitor::new(self, other)
    }

    /// Called when a [Document] is visited and before its child nodes are visited.
    fn enter_document(
        &mut self,
        _ctx: &mut Context,
        _document: &'a Document<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }
    /// Called after a [Document] and its child nodes were visited.
    fn leave_document(
        &mut self,
        _ctx: &mut Context,
        _document: &'a Document<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }

    /// Called when an [`OperationDefinition`] node is visited and before its child nodes are visited.
    fn enter_operation(
        &mut self,
        _ctx: &mut Context,
        _operation: &'a OperationDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }
    /// Called after an [`OperationDefinition`] and its child node were visited.
    fn leave_operation(
        &mut self,
        _ctx: &mut Context,
        _operation: &'a OperationDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }

    /// Called when a [`FragmentDefinition`] node is visited and before its child nodes are visited.
    fn enter_fragment(
        &mut self,
        _ctx: &mut Context,
        _fragment: &'a FragmentDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }
    /// Called after a [`FragmentDefinition`] node and its child nodes were visited.
    fn leave_fragment(
        &mut self,
        _ctx: &mut Context,
        _fragment: &'a FragmentDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }

    /// Called when a [`VariableDefinition`] node is visited and before its child nodes are visited.
    fn enter_variable_definition(
        &mut self,
        _ctx: &mut Context,
        _var_def: &'a VariableDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }
    /// Called after a [`VariableDefinition`] node and its child nodes were visited.
    fn leave_variable_definition(
        &mut self,
        _ctx: &mut Context,
        _var_def: &'a VariableDefinition<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }

    /// Called when a [`SelectionSet`] node is visited and before its child nodes are visited.
    fn enter_selection_set(
        &mut self,
        _ctx: &mut Context,
        _selection_set: &'a SelectionSet<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }
    /// Called after a [`SelectionSet`] node and its child nodes were visited.
    fn leave_selection_set(
        &mut self,
        _ctx: &mut Context,
        _selection_set: &'a SelectionSet<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }

    /// Called when a [`FragmentSpread`] node is visited and before its child nodes are visited.
    fn enter_fragment_spread(
        &mut self,
        _ctx: &mut Context,
        _fragment_spread: &'a FragmentSpread<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }
    /// Called after a [`FragmentSpread`] node and its child nodes were visited.
    fn leave_fragment_spread(
        &mut self,
        _ctx: &mut Context,
        _fragment_spread: &'a FragmentSpread<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }

    /// Called when an [`InlineFragment`] node is visited and before its child nodes are visited.
    fn enter_inline_fragment(
        &mut self,
        _ctx: &mut Context,
        _inline_fragment: &'a InlineFragment<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }
    /// Called after an [`InlineFragment`] node and its child nodes were visited.
    fn leave_inline_fragment(
        &mut self,
        _ctx: &mut Context,
        _inline_fragment: &'a InlineFragment<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }

    /// Called when a [Field] node is visited and before its child nodes are visited.
    fn enter_field(
        &mut self,
        _ctx: &mut Context,
        _field: &'a Field<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }
    /// Called after a [Field] node and its child nodes were visited.
    fn leave_field(
        &mut self,
        _ctx: &mut Context,
        _field: &'a Field<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }

    /// Called when a [Directive] node is visited and before its child nodes are visited.
    fn enter_directive(
        &mut self,
        _ctx: &mut Context,
        _directive: &'a Directive<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }
    /// Called after a [Directive] node and its child nodes were visited.
    fn leave_directive(
        &mut self,
        _ctx: &mut Context,
        _directive: &'a Directive<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }

    /// Called when an [Argument] node is visited and before its child nodes are visited.
    fn enter_argument(
        &mut self,
        _ctx: &mut Context,
        _argument: &'a Argument<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }
    /// Called after an [Argument] node and its child nodes were visited.
    fn leave_argument(
        &mut self,
        _ctx: &mut Context,
        _argument: &'a Argument<'a>,
        _info: &VisitInfo,
    ) -> VisitFlow {
        VisitFlow::Next
    }
}

/// Trait for visiting AST Nodes of a GraphQL language document in depth-first order using a
/// custom visitor.
///
/// The visitor must implement the [Visitor] trait which may also define a custom context structure
/// that can be passed to the `visit` method.
pub trait VisitNode<'a>: Sized + private::VisitNodeWithInfo<'a> {
    /// Visit a GraphQL AST node tree recursively in depth-first order with a given visitor.
    ///
    /// The visitor must implement the [Visitor] trait which may also define a custom context structure
    /// that can be passed to the `visit` method. By default the context is an empty unit `()`.
    fn visit<'b, C, V: Visitor<'a, C>>(&'a self, ctx: &'b mut C, visitor: &'b mut V) -> VisitFlow {
        let mut info = VisitInfo::default();
        self.visit_with_info(ctx, visitor, &mut info)
    }
}

impl<'a, T: private::VisitNodeWithInfo<'a>> VisitNode<'a> for T {}

impl<'a> private::VisitNodeWithInfo<'a> for Argument<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        let flow = visitor.enter_argument(ctx, self, info);
        if let VisitFlow::Next = flow {
            visitor.leave_argument(ctx, self, info)
        } else {
            flow
        }
    }
}

impl<'a> private::VisitNodeWithInfo<'a> for Arguments<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        for (index, argument) in self.children.iter().enumerate() {
            info.path.push(PathSegment::Index(index));
            if let VisitFlow::Break = argument.visit_with_info(ctx, visitor, info) {
                return VisitFlow::Break;
            }
            info.path.pop();
        }
        VisitFlow::Next
    }
}

impl<'a> private::VisitNodeWithInfo<'a> for Directive<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        let flow = visitor.enter_directive(ctx, self, info);
        if let VisitFlow::Next = flow {
            info.path.push(PathSegment::Arguments);
            if self.arguments.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();

            visitor.leave_directive(ctx, self, info)
        } else {
            flow
        }
    }
}

impl<'a> private::VisitNodeWithInfo<'a> for Directives<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        for (index, directive) in self.children.iter().enumerate() {
            info.path.push(PathSegment::Index(index));
            if directive.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();
        }
        VisitFlow::Next
    }
}

impl<'a> private::VisitNodeWithInfo<'a> for VariableDefinition<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        let flow = visitor.enter_variable_definition(ctx, self, info);
        if let VisitFlow::Next = flow {
            info.path.push(PathSegment::Directives);
            if self.directives.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();

            visitor.leave_variable_definition(ctx, self, info)
        } else {
            flow
        }
    }
}

impl<'a> private::VisitNodeWithInfo<'a> for VariableDefinitions<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        for (index, var_def) in self.children.iter().enumerate() {
            info.path.push(PathSegment::Index(index));
            if var_def.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();
        }
        VisitFlow::Next
    }
}

impl<'a> private::VisitNodeWithInfo<'a> for Field<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        let flow = visitor.enter_field(ctx, self, info);
        if let VisitFlow::Next = flow {
            info.path.push(PathSegment::Arguments);
            if self.arguments.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();

            info.path.push(PathSegment::Directives);
            if self.directives.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();

            info.path.push(PathSegment::SelectionSet);
            if self.selection_set.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();

            visitor.leave_field(ctx, self, info)
        } else {
            flow
        }
    }
}

impl<'a> private::VisitNodeWithInfo<'a> for FragmentSpread<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        let flow = visitor.enter_fragment_spread(ctx, self, info);
        if let VisitFlow::Next = flow {
            info.path.push(PathSegment::Directives);
            if self.directives.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();

            visitor.leave_fragment_spread(ctx, self, info)
        } else {
            flow
        }
    }
}

impl<'a> private::VisitNodeWithInfo<'a> for InlineFragment<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        let flow = visitor.enter_inline_fragment(ctx, self, info);
        if let VisitFlow::Next = flow {
            info.path.push(PathSegment::Directives);
            if self.directives.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();

            info.path.push(PathSegment::SelectionSet);
            if self.selection_set.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();

            visitor.leave_inline_fragment(ctx, self, info)
        } else {
            flow
        }
    }
}

impl<'a> private::VisitNodeWithInfo<'a> for SelectionSet<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        let flow = visitor.enter_selection_set(ctx, self, info);
        if let VisitFlow::Next = flow {
            for (index, selection) in self.selections.iter().enumerate() {
                info.path.push(PathSegment::Index(index));
                let flow = match selection {
                    Selection::Field(field) => field.visit_with_info(ctx, visitor, info),
                    Selection::FragmentSpread(spread) => spread.visit_with_info(ctx, visitor, info),
                    Selection::InlineFragment(fragment) => {
                        fragment.visit_with_info(ctx, visitor, info)
                    }
                };
                if flow == VisitFlow::Break {
                    return VisitFlow::Break;
                }
                info.path.pop();
            }
            visitor.leave_selection_set(ctx, self, info)
        } else {
            flow
        }
    }
}

impl<'a> private::VisitNodeWithInfo<'a> for FragmentDefinition<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        let flow = visitor.enter_fragment(ctx, self, info);
        if let VisitFlow::Next = flow {
            info.path.push(PathSegment::Directives);
            if self.directives.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();

            info.path.push(PathSegment::SelectionSet);
            if self.selection_set.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();

            visitor.leave_fragment(ctx, self, info)
        } else {
            flow
        }
    }
}

impl<'a> private::VisitNodeWithInfo<'a> for OperationDefinition<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        let flow = visitor.enter_operation(ctx, self, info);
        if let VisitFlow::Next = flow {
            info.path.push(PathSegment::VariableDefinitions);
            if self
                .variable_definitions
                .visit_with_info(ctx, visitor, info)
                == VisitFlow::Break
            {
                return VisitFlow::Break;
            }
            info.path.pop();

            info.path.push(PathSegment::Directives);
            if self.directives.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();

            info.path.push(PathSegment::SelectionSet);
            if self.selection_set.visit_with_info(ctx, visitor, info) == VisitFlow::Break {
                return VisitFlow::Break;
            }
            info.path.pop();

            visitor.leave_operation(ctx, self, info)
        } else {
            flow
        }
    }
}

impl<'a> private::VisitNodeWithInfo<'a> for Document<'a> {
    #[inline]
    fn visit_with_info<'b, C, V: Visitor<'a, C>>(
        &'a self,
        ctx: &'b mut C,
        visitor: &'b mut V,
        info: &mut VisitInfo,
    ) -> VisitFlow {
        let flow = visitor.enter_document(ctx, self, info);
        if let VisitFlow::Next = flow {
            for (index, selection) in self.definitions.iter().enumerate() {
                info.path.push(PathSegment::Index(index));
                let flow = match selection {
                    Definition::Operation(operation) => {
                        operation.visit_with_info(ctx, visitor, info)
                    }
                    Definition::Fragment(fragment) => fragment.visit_with_info(ctx, visitor, info),
                };
                if flow == VisitFlow::Break {
                    return VisitFlow::Break;
                }
                info.path.pop();
            }
            visitor.leave_document(ctx, self, info)
        } else {
            flow
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    #[derive(Debug, PartialEq, Default)]
    pub(crate) struct CountVisitor {
        pub(crate) in_document: usize,
        pub(crate) out_document: usize,
        pub(crate) in_operation: usize,
        pub(crate) out_operation: usize,
        pub(crate) in_fragment: usize,
        pub(crate) out_fragment: usize,
        pub(crate) in_variable_definition: usize,
        pub(crate) out_variable_definition: usize,
        pub(crate) in_selection_set: usize,
        pub(crate) out_selection_set: usize,
        pub(crate) in_fragment_spread: usize,
        pub(crate) out_fragment_spread: usize,
        pub(crate) in_inline_fragment: usize,
        pub(crate) out_inline_fragment: usize,
        pub(crate) in_field: usize,
        pub(crate) out_field: usize,
        pub(crate) in_directive: usize,
        pub(crate) out_directive: usize,
        pub(crate) in_argument: usize,
        pub(crate) out_argument: usize,
    }

    impl<'a> Visitor<'a, ()> for CountVisitor {
        fn enter_document(
            &mut self,
            _: &mut (),
            _document: &'a Document<'a>,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.in_document += 1;
            VisitFlow::Next
        }
        fn leave_document(
            &mut self,
            _: &mut (),
            _document: &Document,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.out_document += 1;
            VisitFlow::Next
        }

        fn enter_operation(
            &mut self,
            _: &mut (),
            _operation: &OperationDefinition,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.in_operation += 1;
            VisitFlow::Next
        }
        fn leave_operation(
            &mut self,
            _: &mut (),
            _operation: &OperationDefinition,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.out_operation += 1;
            VisitFlow::Next
        }

        fn enter_fragment(
            &mut self,
            _: &mut (),
            _fragment: &FragmentDefinition,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.in_fragment += 1;
            VisitFlow::Next
        }
        fn leave_fragment(
            &mut self,
            _ctx: &mut (),
            _fragment: &FragmentDefinition,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.out_fragment += 1;
            VisitFlow::Next
        }

        fn enter_variable_definition(
            &mut self,
            _: &mut (),
            _var_def: &VariableDefinition,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.in_variable_definition += 1;
            VisitFlow::Next
        }
        fn leave_variable_definition(
            &mut self,
            _: &mut (),
            _var_def: &VariableDefinition,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.out_variable_definition += 1;
            VisitFlow::Next
        }

        fn enter_selection_set(
            &mut self,
            _: &mut (),
            _selection_set: &SelectionSet,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.in_selection_set += 1;
            VisitFlow::Next
        }
        fn leave_selection_set(
            &mut self,
            _: &mut (),
            _selection_set: &SelectionSet,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.out_selection_set += 1;
            VisitFlow::Next
        }

        fn enter_fragment_spread(
            &mut self,
            _: &mut (),
            _fragment_spread: &FragmentSpread,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.in_fragment_spread += 1;
            VisitFlow::Next
        }
        fn leave_fragment_spread(
            &mut self,
            _: &mut (),
            _fragment_spread: &FragmentSpread,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.out_fragment_spread += 1;
            VisitFlow::Next
        }

        fn enter_inline_fragment(
            &mut self,
            _: &mut (),
            _inline_fragment: &InlineFragment,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.in_inline_fragment += 1;
            VisitFlow::Next
        }
        fn leave_inline_fragment(
            &mut self,
            _: &mut (),
            _inline_fragment: &InlineFragment,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.out_inline_fragment += 1;
            VisitFlow::Next
        }

        fn enter_field(&mut self, _: &mut (), _field: &Field, _info: &VisitInfo) -> VisitFlow {
            self.in_field += 1;
            VisitFlow::Next
        }
        fn leave_field(&mut self, _: &mut (), _field: &Field, _info: &VisitInfo) -> VisitFlow {
            self.out_field += 1;
            VisitFlow::Next
        }

        fn enter_directive(
            &mut self,
            _: &mut (),
            _directive: &Directive,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.in_directive += 1;
            VisitFlow::Next
        }
        fn leave_directive(
            &mut self,
            _: &mut (),
            _directive: &Directive,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.out_directive += 1;
            VisitFlow::Next
        }

        fn enter_argument(
            &mut self,
            _: &mut (),
            _argument: &Argument,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.in_argument += 1;
            VisitFlow::Next
        }
        fn leave_argument(
            &mut self,
            _: &mut (),
            _argument: &Argument,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.out_argument += 1;
            VisitFlow::Next
        }
    }

    #[test]
    fn kitchen_sink() {
        let ctx = ASTContext::new();
        let query = include_str!("../../fixture/kitchen_sink.graphql");
        let ast = Document::parse(&ctx, query).unwrap();

        let mut visitor = CountVisitor::default();
        ast.visit(&mut (), &mut visitor);

        assert_eq!(
            visitor,
            CountVisitor {
                in_document: 1,
                out_document: 1,
                in_operation: 5,
                out_operation: 5,
                in_fragment: 1,
                out_fragment: 1,
                in_variable_definition: 3,
                out_variable_definition: 3,
                in_selection_set: 30,
                out_selection_set: 30,
                in_fragment_spread: 1,
                out_fragment_spread: 1,
                in_inline_fragment: 3,
                out_inline_fragment: 3,
                in_field: 21,
                out_field: 21,
                in_directive: 10,
                out_directive: 10,
                in_argument: 13,
                out_argument: 13,
            }
        )
    }

    struct InfoVisitor {}

    impl<'a> Visitor<'a> for InfoVisitor {
        fn enter_fragment_spread(
            &mut self,
            _ctx: &mut (),
            _fragment_spread: &'a FragmentSpread<'a>,
            info: &VisitInfo,
        ) -> VisitFlow {
            // We run this visitor on the kitchen sink query which contains
            // exactly one fragment spread at the following location
            assert_eq!(
                info.path,
                Path::try_from(
                    "0.selectionSet.0.selectionSet.1.selectionSet.0.selectionSet.1.selectionSet.1"
                )
                .unwrap()
            );
            VisitFlow::Next
        }
    }

    #[test]
    fn visit_info_path() {
        let ctx = ASTContext::new();
        let query = include_str!("../../fixture/kitchen_sink.graphql");
        let ast = Document::parse(&ctx, query).unwrap();

        let mut visitor = InfoVisitor {};
        ast.visit(&mut (), &mut visitor);
    }
}
