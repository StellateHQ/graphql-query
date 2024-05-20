use super::visitor::*;
use crate::ast::*;

/// This structure implements the `Visitor` trait and runs two child Visitors in parallel,
/// executing and calling callbacks on them both.
///
/// During traversal the Composed Visitor will keep track of the visitor's [`VisitFlow`] signals and
/// will avoid calling callbacks on them appropriately, while letting the other visitor continue as
/// usual. In short, this visitor aims to minimize the work it does while preserving expected
/// behavior.
///
/// Visitors may be composed indefinitely since a Composed Visitor can be passed into another
/// Composed Visitor, as long as all visitors accept the same `Context` type.
pub struct ComposedVisitor<'a, Context, A: Visitor<'a, Context>, B: Visitor<'a, Context>> {
    _marker: std::marker::PhantomData<&'a Context>,
    depth: usize,
    skip_a: usize,
    skip_b: usize,
    pub a: A,
    pub b: B,
}

impl<'a, C, A: Visitor<'a, C>, B: Visitor<'a, C>> ComposedVisitor<'a, C, A, B> {
    /// Composes two input visitors into one Composed Visitor.
    #[inline]
    pub fn new(a: A, b: B) -> ComposedVisitor<'a, C, A, B> {
        ComposedVisitor {
            _marker: std::marker::PhantomData,
            depth: 1,
            skip_a: usize::MAX,
            skip_b: usize::MAX,
            a,
            b,
        }
    }

    #[inline(always)]
    fn compose_flow_enter<'b, Node: 'a>(
        &mut self,
        fn_a: fn(_self: &mut A, ctx: &mut C, node: &'a Node, info: &VisitInfo) -> VisitFlow,
        fn_b: fn(_self: &mut B, ctx: &mut C, node: &'a Node, info: &VisitInfo) -> VisitFlow,
        node: &'a Node,
        info: &VisitInfo,
        ctx: &mut C,
    ) -> VisitFlow {
        let mut all_skip = true;
        let mut all_break = true;

        if self.skip_a == usize::MAX || self.skip_a == self.depth {
            self.skip_a = usize::MAX;
            let flow = fn_a(&mut self.a, ctx, node, info);
            if flow == VisitFlow::Break {
                self.skip_a = 0;
                all_skip = false;
            } else if flow == VisitFlow::Skip {
                self.skip_a = self.depth;
                all_break = false;
            } else {
                all_break = false;
                all_skip = false;
            }
        } else if self.skip_b == 0 {
            all_skip = false;
        } else {
            all_break = false;
        }

        if self.skip_b == usize::MAX || self.skip_b == self.depth {
            self.skip_b = usize::MAX;
            let flow = fn_b(&mut self.b, ctx, node, info);
            if flow == VisitFlow::Break {
                self.skip_b = 0;
                all_skip = false;
            } else if flow == VisitFlow::Skip {
                self.skip_b = self.depth;
                all_break = false;
            } else {
                all_break = false;
                all_skip = false;
            }
        } else if self.skip_b == 0 {
            all_skip = false;
        } else {
            all_break = false;
        };

        if all_break {
            VisitFlow::Break
        } else if all_skip {
            if self.skip_a == self.depth {
                self.skip_a = usize::MAX;
            }
            if self.skip_b == self.depth {
                self.skip_b = usize::MAX;
            }
            VisitFlow::Skip
        } else {
            self.depth += 1;
            VisitFlow::Next
        }
    }

    #[inline(always)]
    fn compose_flow_leave<'b, Node: 'a>(
        &mut self,
        fn_a: fn(_self: &mut A, ctx: &mut C, node: &'a Node, info: &VisitInfo) -> VisitFlow,
        fn_b: fn(_self: &mut B, ctx: &mut C, node: &'a Node, info: &VisitInfo) -> VisitFlow,
        node: &'a Node,
        info: &VisitInfo,
        ctx: &'b mut C,
    ) -> VisitFlow {
        self.depth -= 1;
        let mut all_break = true;

        if self.skip_a == usize::MAX {
            let flow = fn_a(&mut self.a, ctx, node, info);
            if flow == VisitFlow::Break {
                self.skip_a = 0;
            } else {
                all_break = false;
            }
        } else if self.skip_a == self.depth {
            self.skip_a = usize::MAX;
            all_break = false;
        } else if self.skip_a != 0 {
            all_break = false;
        }

        if self.skip_b == usize::MAX {
            let flow = fn_b(&mut self.b, ctx, node, info);
            if flow == VisitFlow::Break {
                self.skip_b = 0;
            } else {
                all_break = false;
            }
        } else if self.skip_b == self.depth {
            self.skip_b = usize::MAX;
            all_break = false;
        } else if self.skip_b != 0 {
            all_break = false;
        }

        if all_break {
            VisitFlow::Break
        } else {
            VisitFlow::Next
        }
    }
}

impl<'a, C, A: Visitor<'a, C>, B: Visitor<'a, C>> Visitor<'a, C> for ComposedVisitor<'a, C, A, B> {
    #[inline]
    fn enter_document(
        &mut self,
        ctx: &mut C,
        document: &'a Document<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_enter(A::enter_document, B::enter_document, document, info, ctx)
    }

    #[inline]
    fn leave_document(
        &mut self,
        ctx: &mut C,
        document: &'a Document<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_leave(A::leave_document, B::leave_document, document, info, ctx)
    }

    #[inline]
    fn enter_operation(
        &mut self,
        ctx: &mut C,
        operation: &'a OperationDefinition<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_enter(A::enter_operation, B::enter_operation, operation, info, ctx)
    }

    #[inline]
    fn leave_operation(
        &mut self,
        ctx: &mut C,
        operation: &'a OperationDefinition<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_leave(A::leave_operation, B::leave_operation, operation, info, ctx)
    }

    #[inline]
    fn enter_fragment(
        &mut self,
        ctx: &mut C,
        fragment: &'a FragmentDefinition<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_enter(A::enter_fragment, B::enter_fragment, fragment, info, ctx)
    }

    #[inline]
    fn leave_fragment(
        &mut self,
        ctx: &mut C,
        fragment: &'a FragmentDefinition<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_leave(A::leave_fragment, B::leave_fragment, fragment, info, ctx)
    }

    #[inline]
    fn enter_variable_definition(
        &mut self,
        ctx: &mut C,
        var_def: &'a VariableDefinition<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_enter(
            A::enter_variable_definition,
            B::enter_variable_definition,
            var_def,
            info,
            ctx,
        )
    }

    #[inline]
    fn leave_variable_definition(
        &mut self,
        ctx: &mut C,
        var_def: &'a VariableDefinition<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_leave(
            A::leave_variable_definition,
            B::leave_variable_definition,
            var_def,
            info,
            ctx,
        )
    }

    #[inline]
    fn enter_selection_set(
        &mut self,
        ctx: &mut C,
        selection_set: &'a SelectionSet<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_enter(
            A::enter_selection_set,
            B::enter_selection_set,
            selection_set,
            info,
            ctx,
        )
    }

    #[inline]
    fn leave_selection_set(
        &mut self,
        ctx: &mut C,
        selection_set: &'a SelectionSet<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_leave(
            A::leave_selection_set,
            B::leave_selection_set,
            selection_set,
            info,
            ctx,
        )
    }

    #[inline]
    fn enter_fragment_spread(
        &mut self,
        ctx: &mut C,
        fragment_spread: &'a FragmentSpread<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_enter(
            A::enter_fragment_spread,
            B::enter_fragment_spread,
            fragment_spread,
            info,
            ctx,
        )
    }

    #[inline]
    fn leave_fragment_spread(
        &mut self,
        ctx: &mut C,
        fragment_spread: &'a FragmentSpread<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_leave(
            A::leave_fragment_spread,
            B::leave_fragment_spread,
            fragment_spread,
            info,
            ctx,
        )
    }

    #[inline]
    fn enter_inline_fragment(
        &mut self,
        ctx: &mut C,
        inline_fragment: &'a InlineFragment<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_enter(
            A::enter_inline_fragment,
            B::enter_inline_fragment,
            inline_fragment,
            info,
            ctx,
        )
    }

    #[inline]
    fn leave_inline_fragment(
        &mut self,
        ctx: &mut C,
        inline_fragment: &'a InlineFragment<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_leave(
            A::leave_inline_fragment,
            B::leave_inline_fragment,
            inline_fragment,
            info,
            ctx,
        )
    }

    #[inline]
    fn enter_field(&mut self, ctx: &mut C, field: &'a Field<'a>, info: &VisitInfo) -> VisitFlow {
        self.compose_flow_enter(A::enter_field, B::enter_field, field, info, ctx)
    }

    #[inline]
    fn leave_field(&mut self, ctx: &mut C, field: &'a Field<'a>, info: &VisitInfo) -> VisitFlow {
        self.compose_flow_leave(A::leave_field, B::leave_field, field, info, ctx)
    }

    #[inline]
    fn enter_directive(
        &mut self,
        ctx: &mut C,
        directive: &'a Directive<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_enter(A::enter_directive, B::enter_directive, directive, info, ctx)
    }

    #[inline]
    fn leave_directive(
        &mut self,
        ctx: &mut C,
        directive: &'a Directive<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_leave(A::leave_directive, B::leave_directive, directive, info, ctx)
    }

    #[inline]
    fn enter_argument(
        &mut self,
        ctx: &mut C,
        argument: &'a Argument<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_enter(A::enter_argument, B::enter_argument, argument, info, ctx)
    }

    #[inline]
    fn leave_argument(
        &mut self,
        ctx: &mut C,
        argument: &'a Argument<'a>,
        info: &VisitInfo,
    ) -> VisitFlow {
        self.compose_flow_leave(A::leave_argument, B::leave_argument, argument, info, ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::super::visitor::tests::*;
    use super::*;

    #[derive(Debug, PartialEq, Default)]
    struct SkipVisitor {
        pub(crate) enter: usize,
        pub(crate) leave: usize,
        pub(crate) enter_field: usize,
        pub(crate) leave_field: usize,
        pub(crate) enter_directive: usize,
        pub(crate) leave_directive: usize,
    }

    impl<'a> Visitor<'a, ()> for SkipVisitor {
        fn enter_selection_set(
            &mut self,
            _: &mut (),
            _selection_set: &SelectionSet,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.enter += 1;
            VisitFlow::Next
        }
        fn leave_selection_set(
            &mut self,
            _: &mut (),
            _selection_set: &SelectionSet,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.leave += 1;
            VisitFlow::Next
        }

        fn enter_field(&mut self, _: &mut (), _field: &Field, _info: &VisitInfo) -> VisitFlow {
            self.enter_field += 1;
            VisitFlow::Skip
        }
        fn leave_field(&mut self, _: &mut (), _field: &Field, _info: &VisitInfo) -> VisitFlow {
            self.leave_field += 1;
            VisitFlow::Skip
        }

        fn enter_directive(
            &mut self,
            _: &mut (),
            _field: &Directive,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.enter_directive += 1;
            VisitFlow::Skip
        }
        fn leave_directive(
            &mut self,
            _: &mut (),
            _field: &Directive,
            _info: &VisitInfo,
        ) -> VisitFlow {
            self.leave_directive += 1;
            VisitFlow::Skip
        }
    }

    #[test]
    fn kitchen_sink() {
        let ctx = ASTContext::new();
        let query = include_str!("../../fixture/kitchen_sink.graphql");
        let ast = Document::parse(&ctx, query).unwrap();
        let mut visitor = ComposedVisitor::new(CountVisitor::default(), SkipVisitor::default());
        ast.visit(&mut (), &mut visitor);

        assert_eq!(
            visitor.a,
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
        );

        assert_eq!(
            visitor.b,
            SkipVisitor {
                enter: 6,
                leave: 6,
                enter_field: 7,
                leave_field: 0,
                enter_directive: 4,
                leave_directive: 0,
            }
        );
    }
}
