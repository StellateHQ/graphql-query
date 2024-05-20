use super::ast::*;
use super::ast_kind::ASTKind;
use super::lexer::{Extras, Token};
use crate::error::{get_location, print_span, Error, ErrorType, Result};
use bumpalo::collections::Vec;
use logos::{Lexer, Logos, Span};

type ParseResult<T> = std::result::Result<T, ASTKind>;

pub(crate) mod private {
    use super::{ASTContext, Extras, Lexer, Logos, ParseResult, Span, Token};

    /// Private Parser context state that's kept to keep track of the current parser's progress and
    /// state. This contains the AST context's arena and a [Lexer].
    pub struct ParserContext<'a> {
        pub(crate) arena: &'a bumpalo::Bump,
        pub(crate) peek: Option<Token<'a>>,
        pub(crate) iter: Lexer<'a, Token<'a>>,
        pub(crate) in_var_def: bool,
    }

    impl<'a> ParserContext<'a> {
        /// Create a new Parser context for a given AST context and initialize it with an input source
        /// string to parse from.
        pub(crate) fn new(ctx: &'a ASTContext, source: &'a str) -> Self {
            let extras = Extras { arena: &ctx.arena };
            ParserContext {
                arena: &ctx.arena,
                peek: None,
                iter: Token::lexer_with_extras(source, extras),
                in_var_def: false,
            }
        }

        #[inline]
        pub(crate) fn next(&mut self) -> Token<'a> {
            match self.peek.take() {
                Some(token) => token,
                None => self.iter.next().unwrap_or(Token::End),
            }
        }

        #[inline]
        pub(crate) fn peek(&mut self) -> &Token<'a> {
            let iter = &mut self.iter;
            self.peek
                .get_or_insert_with(|| iter.next().unwrap_or(Token::End))
        }

        #[inline]
        pub(crate) fn source(&self) -> &str {
            self.iter.source()
        }

        #[inline]
        pub(crate) fn span(&self) -> Span {
            self.iter.span()
        }
    }

    /// (Private) Trait for parsing AST Nodes from a Parser Context.
    /// The [`super::ParseNode`] trait implements the public `parse` method instead.
    pub trait ParseNode<'a>: Sized {
        fn new_with_ctx(ctx: &mut ParserContext<'a>) -> ParseResult<Self>;
    }
}

/// Trait for parsing AST Nodes from source texts using recursive descent and a lexer.
///
/// This trait is implemented by all AST Nodes and can hence be used to granularly parse GraphQL language.
/// However, mostly this will be used via `Document::parse`.
pub trait ParseNode<'a>: private::ParseNode<'a> {
    /// Parse an input source text into the implementor's AST Node structure and allocate the
    /// resulting AST into the current AST Context's arena.
    fn parse<T: ToString>(ctx: &'a ASTContext, source: T) -> Result<&'a Self> {
        let source = ctx.alloc_string(source.to_string());
        let mut parser_ctx = private::ParserContext::new(ctx, source);
        match Self::new_with_ctx(&mut parser_ctx) {
            Ok(value) => Ok(ctx.alloc(value)),
            Err(error) => {
                let span = print_span(parser_ctx.source(), parser_ctx.span());
                let location = get_location(parser_ctx.source(), parser_ctx.span());
                let message = format!("Invalid {}", error);
                Err(Error::new_with_context(
                    message,
                    Some(location),
                    span,
                    Some(ErrorType::Syntax),
                ))
            }
        }
    }
}

impl<'a, T: private::ParseNode<'a>> ParseNode<'a> for T {}

impl<'a> private::ParseNode<'a> for BooleanValue {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<BooleanValue> {
        match ctx.next() {
            Token::Name("true") => Ok(BooleanValue { value: true }),
            Token::Name("false") => Ok(BooleanValue { value: false }),
            _ => Err(ASTKind::Boolean),
        }
    }
}

impl<'a> private::ParseNode<'a> for EnumValue<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<EnumValue<'a>> {
        match ctx.next() {
            Token::Name("true" | "false" | "null") => Err(ASTKind::Enum),
            Token::Name(value) => Ok(EnumValue { value }),
            _ => Err(ASTKind::Enum),
        }
    }
}

impl<'a> private::ParseNode<'a> for FloatValue<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<FloatValue<'a>> {
        if let Token::Float(value) = ctx.next() {
            Ok(FloatValue { value })
        } else {
            Err(ASTKind::Float)
        }
    }
}

impl<'a> private::ParseNode<'a> for IntValue<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<IntValue<'a>> {
        if let Token::Integer(value) = ctx.next() {
            Ok(IntValue { value })
        } else {
            Err(ASTKind::Int)
        }
    }
}

impl<'a> private::ParseNode<'a> for StringValue<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<StringValue<'a>> {
        if let Token::String(value) = ctx.next() {
            Ok(StringValue { value })
        } else {
            Err(ASTKind::String)
        }
    }
}

impl<'a> private::ParseNode<'a> for Variable<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Variable<'a>> {
        if let Token::VariableName(name) = ctx.next() {
            Ok(Variable { name })
        } else {
            Err(ASTKind::Variable)
        }
    }
}

impl<'a> private::ParseNode<'a> for Value<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Value<'a>> {
        let in_var_def = ctx.in_var_def;
        match ctx.peek() {
            Token::Name("null") => {
                ctx.next();
                Ok(Value::Null)
            }
            Token::VariableName(_) if in_var_def => Err(ASTKind::VariableDefinition),
            Token::VariableName(_) => Variable::new_with_ctx(ctx).map(Value::Variable),
            Token::Name("true" | "false") => BooleanValue::new_with_ctx(ctx).map(Value::Boolean),
            Token::Name(_) => EnumValue::new_with_ctx(ctx).map(Value::Enum),
            Token::Float(_) => FloatValue::new_with_ctx(ctx).map(Value::Float),
            Token::Integer(_) => IntValue::new_with_ctx(ctx).map(Value::Int),
            Token::String(_) => StringValue::new_with_ctx(ctx).map(Value::String),
            Token::BracketOpen => ListValue::new_with_ctx(ctx).map(Value::List),
            Token::BraceOpen => ObjectValue::new_with_ctx(ctx).map(Value::Object),
            _ => Err(ASTKind::Value),
        }
    }
}

impl<'a> private::ParseNode<'a> for ObjectField<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<ObjectField<'a>> {
        if let Token::Name(name) = ctx.next() {
            if let Token::Colon = ctx.next() {
                let value = Value::new_with_ctx(ctx)?;
                return Ok(ObjectField { name, value });
            }
        }
        Err(ASTKind::ObjectField)
    }
}

impl<'a> private::ParseNode<'a> for ObjectValue<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<ObjectValue<'a>> {
        if let Token::BraceOpen = ctx.next() {
            let children = if let Token::BraceClose = ctx.peek() {
                ctx.next();
                Vec::new_in(ctx.arena)
            } else {
                let mut builder = Vec::new_in(ctx.arena);
                loop {
                    builder.push(ObjectField::new_with_ctx(ctx)?);
                    if let Token::BraceClose = ctx.peek() {
                        ctx.next();
                        break;
                    }
                }
                builder
            };
            Ok(ObjectValue { children })
        } else {
            Err(ASTKind::Object)
        }
    }
}

impl<'a> private::ParseNode<'a> for ListValue<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<ListValue<'a>> {
        if let Token::BracketOpen = ctx.next() {
            let children = if let Token::BracketClose = ctx.peek() {
                ctx.next();
                Vec::new_in(ctx.arena)
            } else {
                let mut builder = Vec::new_in(ctx.arena);
                loop {
                    builder.push(Value::new_with_ctx(ctx)?);
                    if let Token::BracketClose = ctx.peek() {
                        ctx.next();
                        break;
                    }
                }
                builder
            };
            Ok(ListValue { children })
        } else {
            Err(ASTKind::List)
        }
    }
}

impl<'a> private::ParseNode<'a> for Argument<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Argument<'a>> {
        if let Token::Name(name) = ctx.next() {
            if let Token::Colon = ctx.next() {
                let value = Value::new_with_ctx(ctx)?;
                return Ok(Argument { name, value });
            }
        }
        Err(ASTKind::Argument)
    }
}

impl<'a> private::ParseNode<'a> for Arguments<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Arguments<'a>> {
        let children = if let Token::ParenOpen = ctx.peek() {
            ctx.next();
            if let Token::ParenClose = ctx.peek() {
                ctx.next();
                Vec::new_in(ctx.arena)
            } else {
                let mut builder = Vec::new_in(ctx.arena);
                loop {
                    builder.push(Argument::new_with_ctx(ctx)?);
                    if let Token::ParenClose = ctx.peek() {
                        ctx.next();
                        break;
                    }
                }
                builder
            }
        } else {
            Vec::new_in(ctx.arena)
        };
        Ok(Arguments { children })
    }
}

impl<'a> private::ParseNode<'a> for Directive<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Directive<'a>> {
        if let Token::DirectiveName(name) = ctx.next() {
            let arguments = Arguments::new_with_ctx(ctx)?;
            Ok(Directive { name, arguments })
        } else {
            Err(ASTKind::Directive)
        }
    }
}

impl<'a> private::ParseNode<'a> for Directives<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Directives<'a>> {
        let mut builder = Vec::new_in(ctx.arena);
        while let Token::DirectiveName(_) = ctx.peek() {
            builder.push(Directive::new_with_ctx(ctx)?);
        }
        Ok(Directives { children: builder })
    }
}

impl<'a> private::ParseNode<'a> for Field<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Field<'a>> {
        if let Token::Name(name_or_alias) = ctx.next() {
            let (alias, name) = if let Token::Colon = ctx.peek() {
                ctx.next();
                if let Token::Name(name) = ctx.next() {
                    (Some(name_or_alias), name)
                } else {
                    return Err(ASTKind::Field);
                }
            } else {
                (None, name_or_alias)
            };

            let arguments = Arguments::new_with_ctx(ctx)?;
            let directives = Directives::new_with_ctx(ctx)?;
            let selection_set = SelectionSet::new_with_ctx(ctx)?;

            Ok(Field {
                alias,
                name,
                arguments,
                directives,
                selection_set,
            })
        } else {
            Err(ASTKind::Field)
        }
    }
}

impl<'a> private::ParseNode<'a> for FragmentSpread<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<FragmentSpread<'a>> {
        if let Token::Ellipsis = ctx.peek() {
            ctx.next();
        };
        match ctx.peek() {
            Token::Name("on") => Err(ASTKind::FragmentSpread),
            Token::Name(_) => {
                let name = NamedType::new_with_ctx(ctx)?;
                let directives = Directives::new_with_ctx(ctx)?;
                Ok(FragmentSpread { name, directives })
            }
            _ => Err(ASTKind::FragmentSpread),
        }
    }
}

impl<'a> private::ParseNode<'a> for NamedType<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<NamedType<'a>> {
        if let Token::Name(name) = ctx.next() {
            Ok(NamedType { name })
        } else {
            Err(ASTKind::NamedType)
        }
    }
}

impl<'a> private::ParseNode<'a> for InlineFragment<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<InlineFragment<'a>> {
        if let Token::Ellipsis = ctx.peek() {
            ctx.next();
        };
        let type_condition = if let Token::Name("on") = ctx.peek() {
            ctx.next();
            Some(NamedType::new_with_ctx(ctx)?)
        } else {
            None
        };
        let directives = Directives::new_with_ctx(ctx)?;
        if let Token::BraceOpen = ctx.peek() {
            let selection_set = SelectionSet::new_with_ctx(ctx)?;
            Ok(InlineFragment {
                type_condition,
                directives,
                selection_set,
            })
        } else {
            Err(ASTKind::InlineFragment)
        }
    }
}

impl<'a> private::ParseNode<'a> for Selection<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Selection<'a>> {
        match ctx.peek() {
            Token::Name(_) => Field::new_with_ctx(ctx).map(Selection::Field),
            Token::Ellipsis => {
                ctx.next();
                match ctx.peek() {
                    Token::DirectiveName(_) | Token::BraceOpen | Token::Name("on") => {
                        InlineFragment::new_with_ctx(ctx).map(Selection::InlineFragment)
                    }
                    Token::Name(_) => {
                        FragmentSpread::new_with_ctx(ctx).map(Selection::FragmentSpread)
                    }
                    _ => Err(ASTKind::Selection),
                }
            }
            _ => Err(ASTKind::Selection),
        }
    }
}

impl<'a> private::ParseNode<'a> for SelectionSet<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<SelectionSet<'a>> {
        let selections = if let Token::BraceOpen = ctx.peek() {
            ctx.next();
            let mut builder = Vec::new_in(ctx.arena);
            loop {
                builder.push(Selection::new_with_ctx(ctx)?);
                if let Token::BraceClose = ctx.peek() {
                    ctx.next();
                    break;
                }
            }
            builder
        } else {
            Vec::new_in(ctx.arena)
        };
        Ok(SelectionSet { selections })
    }
}

impl<'a> private::ParseNode<'a> for Type<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Type<'a>> {
        let token = ctx.next();
        let of_type = if let Token::BracketOpen = token {
            let inner = Type::new_with_ctx(ctx)?;
            if let Token::BracketClose = ctx.next() {
                Type::ListType(ctx.arena.alloc(inner))
            } else {
                return Err(ASTKind::ListType);
            }
        } else if let Token::Name(name) = token {
            Type::NamedType(NamedType { name })
        } else {
            return Err(ASTKind::Type);
        };
        if let Token::Exclam = ctx.peek() {
            ctx.next();
            Ok(Type::NonNullType(ctx.arena.alloc(of_type)))
        } else {
            Ok(of_type)
        }
    }
}

impl<'a> private::ParseNode<'a> for VariableDefinition<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<VariableDefinition<'a>> {
        let variable = Variable::new_with_ctx(ctx)?;
        let of_type = if let Token::Colon = ctx.next() {
            Type::new_with_ctx(ctx)?
        } else {
            return Err(ASTKind::VariableDefinition);
        };
        let default_value = if let Token::Equal = ctx.peek() {
            ctx.next();
            ctx.in_var_def = true;
            let value = Value::new_with_ctx(ctx)?;
            ctx.in_var_def = false;
            value
        } else {
            Value::Null
        };
        let directives = Directives::new_with_ctx(ctx)?;
        Ok(VariableDefinition {
            variable,
            of_type,
            default_value,
            directives,
        })
    }
}

impl<'a> private::ParseNode<'a> for VariableDefinitions<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<VariableDefinitions<'a>> {
        let children = if let Token::ParenOpen = ctx.peek() {
            ctx.next();
            let mut builder = Vec::new_in(ctx.arena);
            loop {
                builder.push(VariableDefinition::new_with_ctx(ctx)?);
                if let Token::ParenClose = ctx.peek() {
                    ctx.next();
                    break;
                }
            }
            builder
        } else {
            Vec::new_in(ctx.arena)
        };
        Ok(VariableDefinitions { children })
    }
}

impl<'a> private::ParseNode<'a> for FragmentDefinition<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<FragmentDefinition<'a>> {
        if let Token::Name("fragment") = ctx.next() {
            let name = NamedType::new_with_ctx(ctx)?;
            let type_condition = if let Token::Name("on") = ctx.next() {
                NamedType::new_with_ctx(ctx)?
            } else {
                return Err(ASTKind::FragmentDefinition);
            };
            let directives = Directives::new_with_ctx(ctx)?;
            let selection_set = if let Token::BraceOpen = ctx.peek() {
                SelectionSet::new_with_ctx(ctx)?
            } else {
                return Err(ASTKind::FragmentDefinition);
            };
            Ok(FragmentDefinition {
                name,
                type_condition,
                directives,
                selection_set,
            })
        } else {
            Err(ASTKind::FragmentDefinition)
        }
    }
}

impl<'a> private::ParseNode<'a> for OperationKind {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<OperationKind> {
        match ctx.next() {
            Token::Name("query") => Ok(OperationKind::Query),
            Token::Name("mutation") => Ok(OperationKind::Mutation),
            Token::Name("subscription") => Ok(OperationKind::Subscription),
            _ => Err(ASTKind::OperationKind),
        }
    }
}

impl<'a> private::ParseNode<'a> for OperationDefinition<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<OperationDefinition<'a>> {
        let operation = match ctx.peek() {
            Token::BraceOpen => {
                let selection_set = SelectionSet::new_with_ctx(ctx)?;
                return Ok(OperationDefinition {
                    operation: OperationKind::Query,
                    name: None,
                    variable_definitions: VariableDefinitions::default_in(ctx.arena),
                    directives: Directives::default_in(ctx.arena),
                    selection_set,
                });
            }
            Token::Name("query") => OperationKind::Query,
            Token::Name("mutation") => OperationKind::Mutation,
            Token::Name("subscription") => OperationKind::Subscription,
            _ => return Err(ASTKind::OperationDefinition),
        };
        ctx.next();
        let name = if let Token::Name(_) = ctx.peek() {
            NamedType::new_with_ctx(ctx).ok()
        } else {
            None
        };
        let variable_definitions = VariableDefinitions::new_with_ctx(ctx)?;
        let directives = Directives::new_with_ctx(ctx)?;
        if let Token::BraceOpen = ctx.peek() {
            let selection_set = SelectionSet::new_with_ctx(ctx)?;
            Ok(OperationDefinition {
                operation,
                name,
                variable_definitions,
                directives,
                selection_set,
            })
        } else {
            Err(ASTKind::OperationDefinition)
        }
    }
}

impl<'a> private::ParseNode<'a> for Document<'a> {
    #[inline]
    fn new_with_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Document<'a>> {
        let mut definitions = Vec::new_in(ctx.arena);
        loop {
            let definition = match ctx.peek() {
                Token::BraceOpen | Token::Name("query" | "mutation" | "subscription") => {
                    OperationDefinition::new_with_ctx(ctx).map(Definition::Operation)
                }
                Token::Name("fragment") => {
                    FragmentDefinition::new_with_ctx(ctx).map(Definition::Fragment)
                }
                Token::End => break,
                _ => Err(ASTKind::Document),
            }?;
            definitions.push(definition);
        }
        Ok(Document {
            definitions,
            size_hint: ctx.iter.span().end,
        })
    }
}

#[cfg(test)]
mod tests {
    use bumpalo::collections::Vec;

    use crate::error::Location;

    use super::{super::ast::*, ParseNode};

    fn assert_parse<'a, T: 'a>(ctx: &'a ASTContext, source: &'a str, expected: T)
    where
        T: ParseNode<'a> + std::fmt::Debug + PartialEq,
    {
        assert_eq!(*T::parse(ctx, source).unwrap(), expected);
    }

    #[test]
    fn error() {
        let ctx = ASTContext::new();
        let result = Document::parse(&ctx, "query { document { $ }}");

        assert_eq!(
            result.err().unwrap().location,
            Some(Location {
                column: 19,
                line: 1
            })
        );

        let result = Document::parse(
            &ctx,
            "query {
            document {
                $
            }
        }",
        );
        assert_eq!(
            result.err().unwrap().location,
            Some(Location {
                column: 16,
                line: 3
            })
        );
    }

    #[test]
    fn named_type() {
        let ctx = ASTContext::new();
        assert_parse(&ctx, "TypeName", NamedType { name: "TypeName" });
    }

    #[test]
    fn variable() {
        let ctx = ASTContext::new();
        assert_parse(&ctx, "$test", Variable { name: "test" });
    }

    #[test]
    fn lists() {
        let ctx = ASTContext::new();
        assert_parse(
            &ctx,
            "[]",
            ListValue {
                children: Vec::new_in(&ctx.arena),
            },
        );

        let list_children = Vec::from_iter_in([Value::Null, Value::Null], &ctx.arena);
        assert_parse(
            &ctx,
            "[null, null]",
            ListValue {
                children: list_children,
            },
        );
    }

    #[test]
    fn objects() {
        let ctx = ASTContext::new();
        assert_parse(
            &ctx,
            "{}",
            ObjectValue {
                children: Vec::new_in(&ctx.arena),
            },
        );
        let list_children = Vec::from_iter_in(
            [ObjectField {
                name: "test",
                value: Value::Boolean(BooleanValue { value: true }),
            }],
            &ctx.arena,
        );
        assert_parse(
            &ctx,
            "{ test: true }",
            ObjectValue {
                children: list_children,
            },
        );
    }

    #[test]
    fn values() {
        let ctx = ASTContext::new();
        assert_parse(&ctx, "true", Value::Boolean(BooleanValue { value: true }));
        assert_parse(&ctx, "false", Value::Boolean(BooleanValue { value: false }));
        assert_parse(&ctx, "$var", Value::Variable(Variable { name: "var" }));
        assert_parse(&ctx, "Opt", Value::Enum(EnumValue { value: "Opt" }));
        assert_parse(&ctx, "123", Value::Int(IntValue { value: "123" }));
        assert_parse(&ctx, "0.0", Value::Float(FloatValue { value: "0.0" }));
        assert_parse(&ctx, "null", Value::Null);

        assert_parse(
            &ctx,
            "\"hello world\"",
            Value::String(StringValue::new(&ctx, "hello world")),
        );

        assert_parse(
            &ctx,
            "[]",
            Value::List(ListValue {
                children: Vec::new_in(&ctx.arena),
            }),
        );
        assert_parse(
            &ctx,
            "[null, null]",
            Value::List(ListValue {
                children: Vec::from_iter_in([Value::Null, Value::Null], &ctx.arena),
            }),
        );

        assert_parse(
            &ctx,
            "{}",
            Value::Object(ObjectValue {
                children: Vec::new_in(&ctx.arena),
            }),
        );
        assert_parse(
            &ctx,
            "{ test: true }",
            Value::Object(ObjectValue {
                children: Vec::from_iter_in(
                    [ObjectField {
                        name: "test",
                        value: Value::Boolean(BooleanValue { value: true }),
                    }],
                    &ctx.arena,
                ),
            }),
        );
    }

    #[test]
    fn arguments() {
        let ctx = ASTContext::new();
        assert_parse(
            &ctx,
            "()",
            Arguments {
                children: Vec::new_in(&ctx.arena),
            },
        );
        assert_parse(
            &ctx,
            "(a: 1, b: 2)",
            Arguments {
                children: Vec::from_iter_in(
                    [
                        Argument {
                            name: "a",
                            value: Value::Int(IntValue { value: "1" }),
                        },
                        Argument {
                            name: "b",
                            value: Value::Int(IntValue { value: "2" }),
                        },
                    ],
                    &ctx.arena,
                ),
            },
        );
    }

    #[test]
    fn directives() {
        let ctx = ASTContext::new();

        assert_parse(
            &ctx,
            "#",
            Directives {
                children: Vec::new_in(&ctx.arena),
            },
        );

        assert_parse(
            &ctx,
            "@defer",
            Directives {
                children: Vec::from_iter_in(
                    [Directive {
                        name: "defer",
                        arguments: Arguments {
                            children: Vec::new_in(&ctx.arena),
                        },
                    }],
                    &ctx.arena,
                ),
            },
        );

        assert_parse(
            &ctx,
            "@defer @defer",
            Directives {
                children: Vec::from_iter_in(
                    [
                        Directive {
                            name: "defer",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                        },
                        Directive {
                            name: "defer",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                        },
                    ],
                    &ctx.arena,
                ),
            },
        );

        assert_parse(
            &ctx,
            "@include(if: $hi)",
            Directive {
                name: "include",
                arguments: Arguments {
                    children: Vec::from_iter_in(
                        [Argument {
                            name: "if",
                            value: Value::Variable(Variable { name: "hi" }),
                        }],
                        &ctx.arena,
                    ),
                },
            },
        );
    }

    #[test]
    fn fields() {
        let ctx = ASTContext::new();

        assert_parse(
            &ctx,
            "name",
            Field {
                alias: None,
                name: "name",
                arguments: Arguments {
                    children: Vec::new_in(&ctx.arena),
                },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::new_in(&ctx.arena),
                },
            },
        );

        assert_parse(
            &ctx,
            "name: name",
            Field {
                alias: Some("name"),
                name: "name",
                arguments: Arguments {
                    children: Vec::new_in(&ctx.arena),
                },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::new_in(&ctx.arena),
                },
            },
        );

        assert_parse(
            &ctx,
            "alias: name",
            Field {
                alias: Some("alias"),
                name: "name",
                arguments: Arguments {
                    children: Vec::new_in(&ctx.arena),
                },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::new_in(&ctx.arena),
                },
            },
        );

        assert_parse(
            &ctx,
            "alias: name(x: null)",
            Field {
                alias: Some("alias"),
                name: "name",
                arguments: Arguments {
                    children: Vec::from_iter_in(
                        [Argument {
                            name: "x",
                            value: Value::Null,
                        }],
                        &ctx.arena,
                    ),
                },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::new_in(&ctx.arena),
                },
            },
        );

        assert_parse(
            &ctx,
            "alias: name(x: null) @skip(if: true)",
            Field {
                alias: Some("alias"),
                name: "name",
                arguments: Arguments {
                    children: Vec::from_iter_in(
                        [Argument {
                            name: "x",
                            value: Value::Null,
                        }],
                        &ctx.arena,
                    ),
                },
                directives: Directives {
                    children: Vec::from_iter_in(
                        [Directive {
                            name: "skip",
                            arguments: Arguments {
                                children: Vec::from_iter_in(
                                    [Argument {
                                        name: "if",
                                        value: Value::Boolean(BooleanValue { value: true }),
                                    }],
                                    &ctx.arena,
                                ),
                            },
                        }],
                        &ctx.arena,
                    ),
                },
                selection_set: SelectionSet {
                    selections: Vec::new_in(&ctx.arena),
                },
            },
        );

        assert_parse(
            &ctx,
            "alias: name(x: null) @skip(if: true) { child }",
            Field {
                alias: Some("alias"),
                name: "name",
                arguments: Arguments {
                    children: Vec::from_iter_in(
                        [Argument {
                            name: "x",
                            value: Value::Null,
                        }],
                        &ctx.arena,
                    ),
                },
                directives: Directives {
                    children: Vec::from_iter_in(
                        [Directive {
                            name: "skip",
                            arguments: Arguments {
                                children: Vec::from_iter_in(
                                    [Argument {
                                        name: "if",
                                        value: Value::Boolean(BooleanValue { value: true }),
                                    }],
                                    &ctx.arena,
                                ),
                            },
                        }],
                        &ctx.arena,
                    ),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "child",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );

        assert_parse(
            &ctx,
            "parent { child }",
            Field {
                alias: None,
                name: "parent",
                arguments: Arguments {
                    children: Vec::new_in(&ctx.arena),
                },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "child",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );
    }

    #[test]
    fn fragment_spread() {
        let ctx = ASTContext::new();

        assert_parse(
            &ctx,
            "... FragName",
            FragmentSpread {
                name: NamedType { name: "FragName" },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
            },
        );

        assert_parse(
            &ctx,
            "... FragName @skip(if: true)",
            FragmentSpread {
                name: NamedType { name: "FragName" },
                directives: Directives {
                    children: Vec::from_iter_in(
                        [Directive {
                            name: "skip",
                            arguments: Arguments {
                                children: Vec::from_iter_in(
                                    [Argument {
                                        name: "if",
                                        value: Value::Boolean(BooleanValue { value: true }),
                                    }],
                                    &ctx.arena,
                                ),
                            },
                        }],
                        &ctx.arena,
                    ),
                },
            },
        );
    }

    #[test]
    fn inline_fragment() {
        let ctx = ASTContext::new();

        assert_parse(
            &ctx,
            "... { __typename }",
            InlineFragment {
                type_condition: None,
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "__typename",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );

        assert_parse(
            &ctx,
            "... on Frag { __typename }",
            InlineFragment {
                type_condition: Some(NamedType { name: "Frag" }),
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "__typename",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );

        assert_parse(
            &ctx,
            "... @skip(if: true) { __typename }",
            InlineFragment {
                type_condition: None,
                directives: Directives {
                    children: Vec::from_iter_in(
                        [Directive {
                            name: "skip",
                            arguments: Arguments {
                                children: Vec::from_iter_in(
                                    [Argument {
                                        name: "if",
                                        value: Value::Boolean(BooleanValue { value: true }),
                                    }],
                                    &ctx.arena,
                                ),
                            },
                        }],
                        &ctx.arena,
                    ),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "__typename",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );

        assert_parse(
            &ctx,
            "...on Frag @skip(if: true) { __typename }",
            InlineFragment {
                type_condition: Some(NamedType { name: "Frag" }),
                directives: Directives {
                    children: Vec::from_iter_in(
                        [Directive {
                            name: "skip",
                            arguments: Arguments {
                                children: Vec::from_iter_in(
                                    [Argument {
                                        name: "if",
                                        value: Value::Boolean(BooleanValue { value: true }),
                                    }],
                                    &ctx.arena,
                                ),
                            },
                        }],
                        &ctx.arena,
                    ),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "__typename",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );
    }

    #[test]
    fn selections() {
        let ctx = ASTContext::new();

        assert_parse(
            &ctx,
            "{ name, ... on Frag { name }, ... OtherFrag, ... { name }, name2: name }",
            SelectionSet {
                selections: Vec::from_iter_in(
                    [
                        Selection::Field(Field {
                            alias: None,
                            name: "name",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        }),
                        Selection::InlineFragment(InlineFragment {
                            type_condition: Some(NamedType { name: "Frag" }),
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::from_iter_in(
                                    [Selection::Field(Field {
                                        alias: None,
                                        name: "name",
                                        arguments: Arguments {
                                            children: Vec::new_in(&ctx.arena),
                                        },
                                        directives: Directives {
                                            children: Vec::new_in(&ctx.arena),
                                        },
                                        selection_set: SelectionSet {
                                            selections: Vec::new_in(&ctx.arena),
                                        },
                                    })],
                                    &ctx.arena,
                                ),
                            },
                        }),
                        Selection::FragmentSpread(FragmentSpread {
                            name: NamedType { name: "OtherFrag" },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                        }),
                        Selection::InlineFragment(InlineFragment {
                            type_condition: None,
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::from_iter_in(
                                    [Selection::Field(Field {
                                        alias: None,
                                        name: "name",
                                        arguments: Arguments {
                                            children: Vec::new_in(&ctx.arena),
                                        },
                                        directives: Directives {
                                            children: Vec::new_in(&ctx.arena),
                                        },
                                        selection_set: SelectionSet {
                                            selections: Vec::new_in(&ctx.arena),
                                        },
                                    })],
                                    &ctx.arena,
                                ),
                            },
                        }),
                        Selection::Field(Field {
                            alias: Some("name2"),
                            name: "name",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        }),
                    ],
                    &ctx.arena,
                ),
            },
        )
    }

    #[test]
    fn types() {
        let ctx = ASTContext::new();

        assert_parse(&ctx, "Type", Type::NamedType(NamedType { name: "Type" }));

        assert_parse(
            &ctx,
            "Type!",
            Type::NonNullType(ctx.alloc(Type::NamedType(NamedType { name: "Type" }))),
        );

        assert_parse(
            &ctx,
            "[Type!]",
            Type::ListType(ctx.alloc(Type::NonNullType(
                ctx.alloc(Type::NamedType(NamedType { name: "Type" })),
            ))),
        );

        assert_parse(
            &ctx,
            "[Type!]!",
            Type::NonNullType(ctx.alloc(Type::ListType(ctx.alloc(Type::NonNullType(
                ctx.alloc(Type::NamedType(NamedType { name: "Type" })),
            ))))),
        );

        assert_parse(
            &ctx,
            "[[Type]]",
            Type::ListType(ctx.alloc(Type::ListType(
                ctx.alloc(Type::NamedType(NamedType { name: "Type" })),
            ))),
        );
    }

    #[test]
    fn var_definitions() {
        let ctx = ASTContext::new();
        assert_parse(
            &ctx,
            "#",
            VariableDefinitions {
                children: Vec::new_in(&ctx.arena),
            },
        );

        // A variable definition cannot refer to another variable
        VariableDefinitions::parse(&ctx, "($var: $var)").unwrap_err();
        VariableDefinitions::parse(&ctx, "($var: [$var])").unwrap_err();

        assert_parse(
            &ctx,
            "($test: String)",
            VariableDefinitions {
                children: Vec::from_iter_in(
                    [VariableDefinition {
                        variable: Variable { name: "test" },
                        of_type: Type::NamedType(NamedType { name: "String" }),
                        default_value: Value::Null,
                        directives: Directives {
                            children: Vec::new_in(&ctx.arena),
                        },
                    }],
                    &ctx.arena,
                ),
            },
        );

        assert_parse(
            &ctx,
            "($test1: String, $test2: Int)",
            VariableDefinitions {
                children: Vec::from_iter_in(
                    [
                        VariableDefinition {
                            variable: Variable { name: "test1" },
                            of_type: Type::NamedType(NamedType { name: "String" }),
                            default_value: Value::Null,
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                        },
                        VariableDefinition {
                            variable: Variable { name: "test2" },
                            of_type: Type::NamedType(NamedType { name: "Int" }),
                            default_value: Value::Null,
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                        },
                    ],
                    &ctx.arena,
                ),
            },
        );

        assert_parse(
            &ctx,
            "$x: Int = 123",
            VariableDefinition {
                variable: Variable { name: "x" },
                of_type: Type::NamedType(NamedType { name: "Int" }),
                default_value: Value::Int(IntValue { value: "123" }),
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
            },
        );

        assert_parse(
            &ctx,
            "$x: Int = 123 @test",
            VariableDefinition {
                variable: Variable { name: "x" },
                of_type: Type::NamedType(NamedType { name: "Int" }),
                default_value: Value::Int(IntValue { value: "123" }),
                directives: Directives {
                    children: Vec::from_iter_in(
                        [Directive {
                            name: "test",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                        }],
                        &ctx.arena,
                    ),
                },
            },
        );

        assert_parse(
            &ctx,
            "$x: Int @test",
            VariableDefinition {
                variable: Variable { name: "x" },
                of_type: Type::NamedType(NamedType { name: "Int" }),
                default_value: Value::Null,
                directives: Directives {
                    children: Vec::from_iter_in(
                        [Directive {
                            name: "test",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                        }],
                        &ctx.arena,
                    ),
                },
            },
        );
    }

    #[test]
    fn fragment() {
        let ctx = ASTContext::new();

        assert_parse(
            &ctx,
            "fragment Test on Type { name }",
            FragmentDefinition {
                name: NamedType { name: "Test" },
                type_condition: NamedType { name: "Type" },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "name",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );

        assert_parse(
            &ctx,
            "fragment Test on Type @test { name }",
            FragmentDefinition {
                name: NamedType { name: "Test" },
                type_condition: NamedType { name: "Type" },
                directives: Directives {
                    children: Vec::from_iter_in(
                        [Directive {
                            name: "test",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                        }],
                        &ctx.arena,
                    ),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "name",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );
    }

    #[test]
    fn operation_with_high_int_value() {
        let ctx = ASTContext::new();

        assert_parse(
            &ctx,
            "query { field(id: 1002275100009989500000000000000000000000000000000000) }",
            OperationDefinition {
                operation: OperationKind::Query,
                name: None,
                variable_definitions: VariableDefinitions { children: Vec::new_in(&ctx.arena )},
                directives: Directives { children: Vec::new_in(&ctx.arena )},
                selection_set: SelectionSet{ selections: Vec::from_iter_in(
                    [Selection::Field(Field {
                        alias: None,
                        name: "field",
                        arguments: Arguments { children: Vec::from_iter_in(
                            [Argument {
                                name: "id",
                                value: Value::Int(IntValue {
                                    value: "1002275100009989500000000000000000000000000000000000",
                                }),
                            }],
                            &ctx.arena,
                        )},
                        directives: Directives { children: Vec::new_in(&ctx.arena )},
                        selection_set: SelectionSet { selections: Vec::new_in(&ctx.arena )},
                    })],
                    &ctx.arena,
                )},
            },
        )
    }

    #[test]
    fn operation() {
        let ctx = ASTContext::new();

        assert_parse(
            &ctx,
            "{ name }",
            OperationDefinition {
                operation: OperationKind::Query,
                name: None,
                variable_definitions: VariableDefinitions {
                    children: Vec::new_in(&ctx.arena),
                },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "name",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );

        assert_parse(
            &ctx,
            "query { name }",
            OperationDefinition {
                operation: OperationKind::Query,
                name: None,
                variable_definitions: VariableDefinitions {
                    children: Vec::new_in(&ctx.arena),
                },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "name",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );

        assert_parse(
            &ctx,
            "mutation { name }",
            OperationDefinition {
                operation: OperationKind::Mutation,
                name: None,
                variable_definitions: VariableDefinitions {
                    children: Vec::new_in(&ctx.arena),
                },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "name",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );

        assert_parse(
            &ctx,
            "subscription { name }",
            OperationDefinition {
                operation: OperationKind::Subscription,
                name: None,
                variable_definitions: VariableDefinitions {
                    children: Vec::new_in(&ctx.arena),
                },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "name",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );

        assert_parse(
            &ctx,
            "query Name { name }",
            OperationDefinition {
                operation: OperationKind::Query,
                name: Some(NamedType { name: "Name" }),
                variable_definitions: VariableDefinitions {
                    children: Vec::new_in(&ctx.arena),
                },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "name",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );

        assert_parse(
            &ctx,
            "query Name($test: Int) { name }",
            OperationDefinition {
                operation: OperationKind::Query,
                name: Some(NamedType { name: "Name" }),
                variable_definitions: VariableDefinitions {
                    children: Vec::from_iter_in(
                        [VariableDefinition {
                            variable: Variable { name: "test" },
                            of_type: Type::NamedType(NamedType { name: "Int" }),
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            default_value: Value::Null,
                        }],
                        &ctx.arena,
                    ),
                },
                directives: Directives {
                    children: Vec::new_in(&ctx.arena),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "name",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );

        assert_parse(
            &ctx,
            "query Name @test { name }",
            OperationDefinition {
                operation: OperationKind::Query,
                name: Some(NamedType { name: "Name" }),
                variable_definitions: VariableDefinitions {
                    children: Vec::new_in(&ctx.arena),
                },
                directives: Directives {
                    children: Vec::from_iter_in(
                        [Directive {
                            name: "test",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                        }],
                        &ctx.arena,
                    ),
                },
                selection_set: SelectionSet {
                    selections: Vec::from_iter_in(
                        [Selection::Field(Field {
                            alias: None,
                            name: "name",
                            arguments: Arguments {
                                children: Vec::new_in(&ctx.arena),
                            },
                            directives: Directives {
                                children: Vec::new_in(&ctx.arena),
                            },
                            selection_set: SelectionSet {
                                selections: Vec::new_in(&ctx.arena),
                            },
                        })],
                        &ctx.arena,
                    ),
                },
            },
        );
    }
}
