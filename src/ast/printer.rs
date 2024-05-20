use super::ast::*;
use std::{fmt, fmt::Write};

/// Trait for printing AST Nodes to a new String allocated on the heap.
/// This is implemented by all AST Nodes and can hence be used to granularly print GraphQL language.
/// However, mostly this will be used via `Document::print`.
///
/// This typically is the last operation that's done in a given AST context and is hence outside
/// of its lifetime and arena.
///
/// For convience when debugging, AST Nodes that implement `PrintNode` also automatically
/// implement the [`fmt::Display`] trait.
pub trait PrintNode {
    /// Write an AST node to a buffer implementing the [Write] trait.
    ///
    /// The `level` indicates the level of nesting, which increases with each [`SelectionSet`]
    /// and is typically initialized as zero (`0`).
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result;

    /// Print an AST Node to source text as a String allocated on the heap.
    ///
    /// For convience when debugging, AST Nodes that implement `PrintNode` also automatically
    /// implement the [`fmt::Display`] trait.
    fn print(&self) -> String {
        let mut buf = String::new();
        match self.write_to_buffer(0, &mut buf) {
            Ok(()) => buf,
            _ => "".to_string(),
        }
    }
}

impl fmt::Display for dyn PrintNode {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write_to_buffer(0, f)
    }
}

impl<'a> PrintNode for NamedType<'a> {
    #[inline]
    fn write_to_buffer(&self, _level: usize, buffer: &mut dyn Write) -> fmt::Result {
        buffer.write_str(self.name)
    }
}

impl<'a> PrintNode for Variable<'a> {
    #[inline]
    fn write_to_buffer(&self, _level: usize, buffer: &mut dyn Write) -> fmt::Result {
        write!(buffer, "${}", self.name)
    }
}

impl PrintNode for BooleanValue {
    #[inline]
    fn write_to_buffer(&self, _level: usize, buffer: &mut dyn Write) -> fmt::Result {
        match self.value {
            true => buffer.write_str("true"),
            false => buffer.write_str("false"),
        }
    }
}

impl<'a> PrintNode for EnumValue<'a> {
    #[inline]
    fn write_to_buffer(&self, _level: usize, buffer: &mut dyn Write) -> fmt::Result {
        buffer.write_str(self.value)
    }
}

impl<'a> PrintNode for FloatValue<'a> {
    #[inline]
    fn write_to_buffer(&self, _level: usize, buffer: &mut dyn Write) -> fmt::Result {
        buffer.write_str(self.value)
    }
}

impl<'a> PrintNode for IntValue<'a> {
    #[inline]
    fn write_to_buffer(&self, _level: usize, buffer: &mut dyn Write) -> fmt::Result {
        buffer.write_str(self.value)
    }
}

impl<'a> PrintNode for StringValue<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        use lexical_core::*;
        let mut buf = [b'0'; u32::FORMATTED_SIZE];

        // See: https://github.com/graphql-rust/graphql-parser/blob/ff34bae/src/format.rs#L127-L167
        if !self.is_block() {
            buffer.write_char('"')?;
            for c in self.value.chars() {
                match c {
                    '\r' => buffer.write_str(r"\r")?,
                    '\n' => buffer.write_str(r"\n")?,
                    '\t' => buffer.write_str(r"\t")?,
                    '"' => buffer.write_str("\\\"")?,
                    '\\' => buffer.write_str(r"\\")?,
                    '\u{0020}'..='\u{FFFF}' => buffer.write_char(c)?,
                    _ => unsafe {
                        const FORMAT: u128 = NumberFormatBuilder::hexadecimal();
                        const OPTIONS: WriteIntegerOptions = WriteIntegerOptions::new();
                        let buf =
                            write_with_options_unchecked::<_, FORMAT>(c as u32, &mut buf, &OPTIONS);
                        write!(buffer, "\\u{:0>4}", std::str::from_utf8_unchecked(buf))?;
                    },
                };
            }
            buffer.write_char('"')
        } else {
            buffer.write_str("\"\"\"\n")?;
            for line in self.value.lines() {
                if !line.trim().is_empty() {
                    write_indent(level, buffer)?;
                    buffer.write_str(&line.replace(r#"""""#, r#"\""""#))?;
                }
                buffer.write_char('\n')?;
            }
            write_indent(level, buffer)?;
            buffer.write_str("\"\"\"")
        }
    }
}

impl<'a> PrintNode for Value<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        match self {
            Value::Boolean(value) => value.write_to_buffer(level, buffer),
            Value::Enum(value) => value.write_to_buffer(level, buffer),
            Value::Float(value) => value.write_to_buffer(level, buffer),
            Value::Int(value) => value.write_to_buffer(level, buffer),
            Value::String(value) => value.write_to_buffer(level, buffer),
            Value::Variable(value) => value.write_to_buffer(level, buffer),
            Value::Object(value) => value.write_to_buffer(level, buffer),
            Value::List(value) => value.write_to_buffer(level, buffer),
            Value::Null => buffer.write_str("null"),
        }
    }
}

impl<'a> PrintNode for ObjectField<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        write!(buffer, "{}: ", self.name)?;
        self.value.write_to_buffer(level, buffer)
    }
}

impl<'a> PrintNode for ObjectValue<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        buffer.write_str("{")?;
        let mut first = true;
        for field in self.children.iter() {
            if first {
                first = false;
            } else {
                buffer.write_str(", ")?;
            }
            field.write_to_buffer(level, buffer)?;
        }
        buffer.write_str("}")
    }
}

impl<'a> PrintNode for ListValue<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        buffer.write_str("[")?;
        let mut first = true;
        for field in self.children.iter() {
            if first {
                first = false;
            } else {
                buffer.write_str(", ")?;
            }
            field.write_to_buffer(level, buffer)?;
        }
        buffer.write_str("]")
    }
}

impl<'a> PrintNode for Argument<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        write!(buffer, "{}: ", self.name)?;
        self.value.write_to_buffer(level, buffer)
    }
}

impl<'a> PrintNode for Arguments<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        if !self.is_empty() {
            buffer.write_str("(")?;
            let mut first = true;
            for argument in self.children.iter() {
                if first {
                    first = false;
                } else {
                    buffer.write_str(", ")?;
                }
                argument.write_to_buffer(level, buffer)?;
            }
            buffer.write_str(")")
        } else {
            Ok(())
        }
    }
}

impl<'a> PrintNode for Directive<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        write!(buffer, "@{}", self.name)?;
        self.arguments.write_to_buffer(level, buffer)
    }
}

impl<'a> PrintNode for Directives<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        for directive in self.children.iter() {
            buffer.write_str(" ")?;
            directive.write_to_buffer(level, buffer)?;
        }
        Ok(())
    }
}

impl<'a> PrintNode for SelectionSet<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        if !self.is_empty() {
            let level = level + 1;
            buffer.write_str("{")?;
            for selection in self.selections.iter() {
                buffer.write_char('\n')?;
                write_indent(level, buffer)?;
                match selection {
                    Selection::Field(field) => field.write_to_buffer(level, buffer)?,
                    Selection::FragmentSpread(spread) => spread.write_to_buffer(level, buffer)?,
                    Selection::InlineFragment(inline) => inline.write_to_buffer(level, buffer)?,
                };
            }
            buffer.write_char('\n')?;
            write_indent(level - 1, buffer)?;
            buffer.write_char('}')
        } else {
            Ok(())
        }
    }
}

impl<'a> PrintNode for Field<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        if let Some(alias) = self.alias {
            write!(buffer, "{}: {}", alias, self.name)?;
        } else {
            buffer.write_str(self.name)?;
        };
        self.arguments.write_to_buffer(level, buffer)?;
        self.directives.write_to_buffer(level, buffer)?;
        if !self.selection_set.is_empty() {
            buffer.write_str(" ")?;
        };
        self.selection_set.write_to_buffer(level, buffer)
    }
}

impl<'a> PrintNode for FragmentSpread<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        buffer.write_str("...")?;
        self.name.write_to_buffer(level, buffer)?;
        self.directives.write_to_buffer(level, buffer)
    }
}

impl<'a> PrintNode for InlineFragment<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        buffer.write_str("...")?;
        if let Some(name) = &self.type_condition {
            buffer.write_str(" on ")?;
            name.write_to_buffer(level, buffer)?;
        };
        self.directives.write_to_buffer(level, buffer)?;
        buffer.write_str(" ")?;
        self.selection_set.write_to_buffer(level, buffer)
    }
}

impl<'a> PrintNode for Type<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        match self {
            Type::NamedType(name) => name.write_to_buffer(level, buffer),
            Type::ListType(inner) => {
                buffer.write_str("[")?;
                inner.write_to_buffer(level, buffer)?;
                buffer.write_str("]")
            }
            Type::NonNullType(inner) => {
                inner.write_to_buffer(level, buffer)?;
                buffer.write_str("!")
            }
        }
    }
}

impl<'a> PrintNode for VariableDefinition<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        self.variable.write_to_buffer(level, buffer)?;
        buffer.write_str(": ")?;
        self.of_type.write_to_buffer(level, buffer)?;
        if self.default_value != Value::Null {
            buffer.write_str(" = ")?;
            self.default_value.write_to_buffer(level, buffer)?;
        }
        self.directives.write_to_buffer(level, buffer)
    }
}

impl<'a> PrintNode for VariableDefinitions<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        if !self.is_empty() {
            buffer.write_str("(")?;
            let mut first = true;
            for var_definition in self.children.iter() {
                if first {
                    first = false;
                } else {
                    buffer.write_str(", ")?;
                }
                var_definition.write_to_buffer(level, buffer)?;
            }
            buffer.write_str(")")
        } else {
            Ok(())
        }
    }
}

impl<'a> PrintNode for FragmentDefinition<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        buffer.write_str("fragment ")?;
        self.name.write_to_buffer(level, buffer)?;
        buffer.write_str(" on ")?;
        self.type_condition.write_to_buffer(level, buffer)?;
        self.directives.write_to_buffer(level, buffer)?;
        buffer.write_str(" ")?;
        self.selection_set.write_to_buffer(level, buffer)
    }
}

impl<'a> PrintNode for OperationDefinition<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        if self.operation == OperationKind::Query
            && self.name.is_none()
            && self.variable_definitions.is_empty()
            && self.directives.is_empty()
        {
            self.selection_set.write_to_buffer(level, buffer)
        } else {
            match self.operation {
                OperationKind::Query => buffer.write_str("query")?,
                OperationKind::Mutation => buffer.write_str("mutation")?,
                OperationKind::Subscription => buffer.write_str("subscription")?,
            };
            if let Some(name) = &self.name {
                buffer.write_str(" ")?;
                name.write_to_buffer(level, buffer)?;
            };
            if self.name.is_none() && !self.variable_definitions.is_empty() {
                buffer.write_str(" ")?;
            }
            self.variable_definitions.write_to_buffer(level, buffer)?;
            self.directives.write_to_buffer(level, buffer)?;
            buffer.write_str(" ")?;
            self.selection_set.write_to_buffer(level, buffer)
        }
    }
}

impl<'a> PrintNode for Definition<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        match self {
            Definition::Operation(operation) => operation.write_to_buffer(level, buffer),
            Definition::Fragment(fragment) => fragment.write_to_buffer(level, buffer),
        }
    }
}

impl<'a> PrintNode for Document<'a> {
    #[inline]
    fn write_to_buffer(&self, level: usize, buffer: &mut dyn Write) -> fmt::Result {
        let mut first = true;
        for definition in self.definitions.iter() {
            if first {
                first = false;
            } else {
                buffer.write_str("\n\n")?;
            }
            definition.write_to_buffer(level, buffer)?;
        }
        Ok(())
    }

    #[inline]
    fn print(&self) -> String {
        let mut buf = String::with_capacity(self.size_hint);
        match self.write_to_buffer(0, &mut buf) {
            Ok(()) => buf,
            _ => "".to_string(),
        }
    }
}

#[inline(always)]
fn write_indent(level: usize, buffer: &mut dyn Write) -> fmt::Result {
    for _ in 0..level {
        buffer.write_str("  ")?
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::*;

    #[test]
    fn values() {
        let ctx = ASTContext::new();
        let ast = Value::parse(&ctx, "{ a: true, b: [1, 2] }");
        assert_eq!(ast.unwrap().print(), "{a: true, b: [1, 2]}");
        let ast = Value::parse(&ctx, "123.23");
        assert_eq!(ast.unwrap().print(), "123.23");
        let ast = Value::parse(&ctx, "123.23e20");
        assert_eq!(ast.unwrap().print(), "123.23e20");
    }

    #[test]
    fn arguments() {
        let ctx = ASTContext::new();
        let ast = Arguments::parse(&ctx, "()");
        assert_eq!(ast.unwrap().print(), "");
        let ast = Arguments::parse(&ctx, "(a:1)");
        assert_eq!(ast.unwrap().print(), "(a: 1)");
        let ast = Arguments::parse(&ctx, "(a:1 b:2)");
        assert_eq!(ast.unwrap().print(), "(a: 1, b: 2)");
    }

    #[test]
    fn directives() {
        let ctx = ASTContext::new();
        let ast = Directives::parse(&ctx, "@skip(if: true)");
        assert_eq!(ast.unwrap().print(), " @skip(if: true)");
        let ast = Directives::parse(&ctx, "@skip(if: true) @include(if: false)");
        assert_eq!(ast.unwrap().print(), " @skip(if: true) @include(if: false)");
    }

    #[test]
    fn field() {
        let ctx = ASTContext::new();
        let ast = Field::parse(&ctx, "field { child }");
        assert_eq!(ast.unwrap().print(), "field {\n  child\n}");
        let ast = Field::parse(&ctx, "field { child { child } }");
        assert_eq!(
            ast.unwrap().print(),
            "field {\n  child {\n    child\n  }\n}"
        );
        let ast = Field::parse(&ctx, "alias : field");
        assert_eq!(ast.unwrap().print(), "alias: field");
        let ast = Field::parse(&ctx, "field: field");
        assert_eq!(ast.unwrap().print(), "field: field");
        let ast = Field::parse(&ctx, "field (test: true)");
        assert_eq!(ast.unwrap().print(), "field(test: true)");
        let ast = Field::parse(&ctx, "field (test: true) @test");
        assert_eq!(ast.unwrap().print(), "field(test: true) @test");
    }

    #[test]
    fn fragment_spread() {
        let ctx = ASTContext::new();
        let ast = FragmentSpread::parse(&ctx, "...Type");
        assert_eq!(ast.unwrap().print(), "...Type");
        let ast = FragmentSpread::parse(&ctx, "...Type @test");
        assert_eq!(ast.unwrap().print(), "...Type @test");
    }

    #[test]
    fn inline_fragment() {
        let ctx = ASTContext::new();
        let ast = InlineFragment::parse(&ctx, "... on Type { field }");
        assert_eq!(ast.unwrap().print(), "... on Type {\n  field\n}");
        let ast = InlineFragment::parse(&ctx, "... on Type @test{ field }");
        assert_eq!(ast.unwrap().print(), "... on Type @test {\n  field\n}");
        let ast = InlineFragment::parse(&ctx, "...@test { field }");
        assert_eq!(ast.unwrap().print(), "... @test {\n  field\n}");
    }

    #[test]
    fn _type() {
        let ctx = ASTContext::new();
        let ast = Type::parse(&ctx, "[Type]");
        assert_eq!(ast.unwrap().print(), "[Type]");
        let ast = Type::parse(&ctx, "[Type !] !");
        assert_eq!(ast.unwrap().print(), "[Type!]!");
        let ast = Type::parse(&ctx, "Type!");
        assert_eq!(ast.unwrap().print(), "Type!");
    }

    #[test]
    fn variable_definitions() {
        let ctx = ASTContext::new();
        let ast = VariableDefinitions::parse(&ctx, "($x : Int)");
        assert_eq!(ast.unwrap().print(), "($x: Int)");
        let ast = VariableDefinitions::parse(&ctx, "($x : Int = 1)");
        assert_eq!(ast.unwrap().print(), "($x: Int = 1)");
        let ast = VariableDefinitions::parse(&ctx, "($x : Int = 1, $y: Bool)");
        assert_eq!(ast.unwrap().print(), "($x: Int = 1, $y: Bool)");
    }

    #[test]
    fn strings() {
        let ctx = ASTContext::new();
        let ast = Value::parse(&ctx, "\"\\u0001\"");
        assert_eq!(ast.unwrap().print(), "\"\\u0001\"");
        let ast = Value::parse(&ctx, "\"\\u0019\"");
        assert_eq!(ast.unwrap().print(), "\"\\u0019\"");
        let ast = Value::parse(&ctx, "\"\0\"");
        assert_eq!(ast.unwrap().print(), "\"\\u0000\"");
    }

    #[test]
    fn block_strings() {
        let ctx = ASTContext::new();
        let ast = Value::parse(
            &ctx,
            r#"
            """
            this
              is
            doc
            """
        "#,
        );
        assert_eq!(ast.unwrap().print(), "\"\"\"\nthis\n  is\ndoc\n\"\"\"");

        let ast = Value::parse(
            &ctx,
            r#"
            """this
              is
            doc"""
        "#,
        );
        assert_eq!(ast.unwrap().print(), "\"\"\"\nthis\n  is\ndoc\n\"\"\"");

        let ast = Value::parse(
            &ctx,
            r#"
            """
              this
              is
              doc
            """
        "#,
        );
        assert_eq!(ast.unwrap().print(), "\"\"\"\n  this\n  is\n  doc\n\"\"\"");
    }

    #[test]
    fn fragment_definitions() {
        let ctx = ASTContext::new();
        let ast = FragmentDefinition::parse(
            &ctx,
            r#"
            fragment Test on Type {
              field
            }
        "#,
        );
        assert_eq!(ast.unwrap().print(), "fragment Test on Type {\n  field\n}");

        let ast = FragmentDefinition::parse(
            &ctx,
            r#"
            fragment Test on Type @test {
              field
            }
        "#,
        );
        assert_eq!(
            ast.unwrap().print(),
            "fragment Test on Type @test {\n  field\n}"
        );
    }

    #[test]
    fn operation_definition() {
        let ctx = ASTContext::new();
        let ast = OperationDefinition::parse(
            &ctx,
            r#"
            query {
              field
            }
        "#,
        );
        assert_eq!(ast.unwrap().print(), "{\n  field\n}");

        let ast = OperationDefinition::parse(
            &ctx,
            r#"
            query Name {
              field
            }
        "#,
        );
        assert_eq!(ast.unwrap().print(), "query Name {\n  field\n}");

        let ast = OperationDefinition::parse(
            &ctx,
            r#"
            query Name ($var: String) {
              field
            }
        "#,
        );
        assert_eq!(
            ast.unwrap().print(),
            "query Name($var: String) {\n  field\n}"
        );

        let ast = OperationDefinition::parse(
            &ctx,
            r#"
            query ($var: String) {
              field
            }
        "#,
        );
        assert_eq!(ast.unwrap().print(), "query ($var: String) {\n  field\n}");

        let ast = OperationDefinition::parse(
            &ctx,
            r#"
            query Name ($var: String) @defer{
              field
            }
        "#,
        );
        assert_eq!(
            ast.unwrap().print(),
            "query Name($var: String) @defer {\n  field\n}"
        );

        let ast = OperationDefinition::parse(
            &ctx,
            r#"
            mutation {
              doThing
            }
        "#,
        );
        assert_eq!(ast.unwrap().print(), "mutation {\n  doThing\n}");
    }

    #[test]
    fn kitchen_sink() {
        let ctx = ASTContext::new();
        let query = include_str!("../../fixture/kitchen_sink.graphql");
        let ast = Document::parse(&ctx, query);
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

            mutation likeStory @onMutation {
              like(story: 123) @onField {
                story {
                  id @onField
                }
              }
            }

            subscription StoryLikeSubscription($input: StoryLikeSubscribeInput) @onSubscription {
              storyLikeSubscribe(input: $input) {
                story {
                  likers {
                    count
                  }
                  likeSentence {
                    text
                  }
                }
              }
            }

            fragment frag on Friend @onFragmentDefinition {
              foo(size: $site, bar: 12, obj: {key: "value", block: """
              block string uses \"""
              """})
            }

            query teeny {
              unnamed(truthy: true, falsey: false, nullish: null)
              query
            }

            query tiny {
              __typename
            }"#};
        assert_eq!(ast.unwrap().print(), expected);
    }
}
