use super::{
  error::{syntax, syntax_err, validation, SchemaError}, finalizers::initialize_type_definition, parse_ast::*
};
use crate::{
  ast::{ASTContext, DefaultIn},
  error::{get_location, print_span, Error, ErrorType, Result},
  schema::{
      sdl::lexer::{Extras, Token}, Schema, SchemaEnum, SchemaPossibleTypes, SchemaScalar
  },
};
use bumpalo::collections::Vec;
use hashbrown::{HashMap, HashSet};
use logos::{Lexer, Logos, Span};

pub type ParseResult<T> = std::result::Result<T, SchemaError>;

pub(crate) mod private {
  use bumpalo::collections::Vec;

use super::*;

  /// Private Parser context state that's kept to keep track of the current parser's progress and
  /// state. This contains the AST context and a [Lexer].
  pub struct ParserContext<'a> {
      pub(crate) ast_ctx: &'a ASTContext,
      pub(crate) peek: Option<Token<'a>>,
      pub(crate) iter: Lexer<'a, Token<'a>>,
  }

  impl<'a> ParserContext<'a> {
      /// Create a new Parser context for a given AST context and initialize it with an input source
      /// string to parse from.
      pub(crate) fn new(ctx: &'a ASTContext, source: &'a str) -> Self {
          let extras = Extras { arena: &ctx.arena };
          ParserContext {
              ast_ctx: ctx,
              peek: None,
              iter: Token::lexer_with_extras(source, extras),
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

      pub(crate) fn get_implements_interfaces(&mut self) -> ParseResult<Vec<'a, &'a str>> {
          let mut interfaces = Vec::new_in(&self.ast_ctx.arena);
          if self.peek() == &Token::Name("implements") {
              // Skip `implements`
              self.next();

              // Skip optional leading `&`
              if self.peek() == &Token::Ampersand {
                  self.next();
              }

              // Get name of first interface
              match self.next() {
                  Token::Name(interface) => interfaces.push(interface),
                  t => return syntax_err!("Expected interface name, got {:?}", t),
              }

              // Get any remaining interfaces
              while self.peek() == &Token::Ampersand {
                  // Skip `&`
                  self.next();

                  // Get name of next interface
                  match self.next() {
                      Token::Name(interface) => interfaces.push(interface),
                      t => return syntax_err!("Expected interface name, got {:?}", t),
                  }
              }
          }

          Ok(interfaces)
      }
  }

  /// (Private) Trait for parsing AST Nodes from a Parser Context.
  /// The [`super::ParseNode`] trait implements the public `parse` method instead.
  pub trait ParseFromCtx<'a>: Sized {
      fn parse_from_ctx(ctx: &mut ParserContext<'a>) -> ParseResult<Self>;
  }
}

impl<'a, T: private::ParseFromCtx<'a>> ParseSdl<'a> for T {}

/// Trait for parsing SDL AST nodes from source text using recursive descent and a lexer.
///
/// This trait is implemented by all AST nodes and can hence be used to granularly parse GraphQL SDL.
/// However, mostly this will be used via `Schema::parse`.
pub trait ParseSdl<'a>: private::ParseFromCtx<'a> {
  /// Parse an input source text into the implementor's AST node structure and allocate the
  /// resulting AST into the context arena.
  fn parse<T: ToString>(ctx: &'a ASTContext, source: T) -> Result<&'a Self> {
      let source = ctx.alloc_string(source.to_string());
      let mut parser_ctx = private::ParserContext::new(ctx, source);
      match Self::parse_from_ctx(&mut parser_ctx) {
          Ok(value) => Ok(ctx.alloc(value)),
          Err(error) => {
              let span = print_span(parser_ctx.source(), parser_ctx.span());
              let location = get_location(parser_ctx.source(), parser_ctx.span());
              let message = error.to_string();

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

const DEFAULT_SCALARS: [&str; 5] = [
  "String",
  "Int",
  "Float",
  "Boolean",
  "ID",
];

impl<'a> private::ParseFromCtx<'a> for Schema<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      let mut schema = Schema::default_in(&ctx.ast_ctx.arena);
      let mut schema_def = None;
      let mut type_defs = HashMap::new_in(&ctx.ast_ctx.arena);
      let mut directive_defs = HashMap::new_in(&ctx.ast_ctx.arena);
      // TODO: store schema extensions here and merge after first loop
      // let mut schema_extensions = HashMap::new_in(&ctx.ast_ctx.arena);
      loop {
          match ctx.peek() {
              Token::End => break,
              Token::Name("schema") => {
                  if schema_def.is_none() {
                      schema_def = Some(SchemaDefinition::parse_from_ctx(ctx)?)
                  } else {
                      return syntax_err!("Must not specify more than one Schema Definition.");
                  }
              }
              Token::Name("directive") => {
                let directive_def = DirectiveDefinition::parse_from_ctx(ctx)?;
                directive_defs.insert(directive_def.name(), directive_def);
              }
              _ => {
                let type_def = TypeDefinition::parse_from_ctx(ctx)?;
                type_defs.insert(type_def.name(), type_def);
              },
          }
      }

      dbg!(directive_defs);

      for (name, typ) in type_defs.clone().iter() {
        match typ {
            TypeDefinition::InterfaceTypeDefinition(i) => {
                for interface in i.interfaces.iter() {
                    if let Some(TypeDefinition::InterfaceTypeDefinition(implemented_interface)) = type_defs.get_mut(interface) {
                        // TODO: check whether this is the right way to go about it, might be an existing bug in
                        // the client-schema
                        implemented_interface.add_possible_interface(name);
                    }
                }
            },
            TypeDefinition::ObjectTypeDefinition(obj) => {
                for interface in obj.interfaces.iter() {
                    if let Some(TypeDefinition::InterfaceTypeDefinition(i)) = type_defs.get_mut(interface) {
                        i.add_possible_type(name);
                    }
                }
            }
            _ => (),
        }
      }

      for scalar in DEFAULT_SCALARS.iter() {
          schema.types.insert(
              *scalar,
              ctx.ast_ctx.alloc(crate::schema::SchemaType::Scalar(ctx.ast_ctx.alloc(SchemaScalar { name: *scalar }))),
          );
      }

      // Build type skeletons in a first pass.
      // We need to create all type definitions first to avoid circular evaluation of types.
      for typ in type_defs.into_iter() {
          schema.types.insert(
              typ.0,
              initialize_type_definition(&ctx.ast_ctx, ctx.ast_ctx.alloc(typ.1)),
          );
      }

      for (name, typ) in schema.types.iter() {
        match typ {
            crate::schema::SchemaType::Object(schema_obj) => {
                for field in schema_obj.fields.iter() {
                    // TODO: check field-arguments

                    field.1.output_type.of_type(&schema).output_type().ok_or_else(|| validation!(
                        "Field `{}` of type `{}` has an invalid type.",
                        field.0,
                        name
                    ))?;
                }

                for type_name in schema_obj.interfaces.iter() {
                    schema.get_type(type_name).ok_or_else(|| validation!(
                        "Interface `{}` on object `{}` does not exist.",
                        type_name,
                        schema_obj.name
                    ))?;
                }
            }
            crate::schema::SchemaType::InputObject(schema_input) => {
                for field in schema_input.fields.iter() {
                    field.1.input_type.of_type(&schema).input_type().ok_or_else(|| validation!(
                        "Field `{}` of type `{}` has an invalid type.",
                        field.0,
                        name
                    ))?;
                }
            }
            crate::schema::SchemaType::Union(schema_union) => {
                for type_name in schema_union.get_possible_types().iter() {
                    schema.get_type(type_name).ok_or_else(|| validation!(
                        "Type `{}` on union `{}` is not a valid type.",
                        type_name,
                        schema_union.name
                    ))?;
                }
            }
            crate::schema::SchemaType::Interface(schema_interface) => {
                for field in schema_interface.fields.iter() {
                    // TODO: Check whether each argument refers to a valid type

                    field.1.output_type.of_type(&schema).output_type().ok_or_else(|| validation!(
                        "Field `{}` of type `{}` has an invalid type.",
                        field.0,
                        name
                    ))?;
                }

                for type_name in schema_interface.get_possible_types().iter() {
                    schema.get_type(type_name).ok_or_else(|| validation!(
                        "Type `{}` on interface `{}` is not a valid type.",
                        type_name,
                        schema_interface.name
                    ))?;
                }

                for type_name in schema_interface.interfaces.iter() {
                    schema.get_type(type_name).ok_or_else(|| validation!(
                        "Interface `{}` on object `{}` does not exist.",
                        type_name,
                        schema_interface.name
                    ))?;
                }
            }
            _ => {
                // Other types have no issues
            }
        }
      } 

      // fill in the root types
      if let Some(query_type) = schema_def
          .map_or(Some("Query"), |def| def.query_root_type_name)
          .and_then(|name| schema.types.get(name))
      {
          schema.query_type = match query_type.object() {
              Some(obj_type) => Some(obj_type),
              None => {
                  return syntax_err!(
                      "Query root type `{}` must be an object type.",
                      query_type.name()
                  )
              }
          }
      }

      if let Some(mutation_type) = schema_def
          .map_or(Some("Mutation"), |def| def.mutation_root_type_name)
          .and_then(|name| schema.types.get(name))
      {
          schema.mutation_type = match mutation_type.object() {
              Some(obj_type) => Some(obj_type),
              None => {
                  return syntax_err!(
                      "Mutation root type `{}` must be an object type.",
                      mutation_type.name()
                  )
              }
          }
      }

      if let Some(subscription_type) = schema_def
          .map_or(Some("Subscription"), |def| def.subscription_root_type_name)
          .and_then(|name| schema.types.get(name))
      {
          schema.subscription_type = match subscription_type.object() {
              Some(obj_type) => Some(obj_type),
              None => {
                  return syntax_err!(
                      "Subscription root type `{}` must be an object type.",
                      subscription_type.name()
                  )
              }
          }
      }

      Ok(schema)
  }
}

impl<'a> private::ParseFromCtx<'a> for SchemaDefinition<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      if ctx.next() != Token::Name("schema") {
          return syntax_err!("Schema definition must start with the `schema` keyword.");
      }

      if Token::BraceOpen != ctx.next() {
          return syntax_err!("Expected `{{`");
      }

      let mut defs = Self {
          query_root_type_name: None,
          mutation_root_type_name: None,
          subscription_root_type_name: None,
      };

      while !matches!(ctx.peek(), Token::BraceClose | Token::End) {
          let operation_type = match ctx.next() {
              Token::Name(op @ ("query" | "mutation" | "subscription")) => op,
              t => return syntax_err!("Expected operation type, got {:?}", t),
          };

          if Token::Colon != ctx.next() {
              return syntax_err!("Expected `:`");
          }

          let type_name = if let Token::Name(n) = ctx.next() {
              n
          } else {
              return syntax_err!("Expected named type");
          };

          match operation_type {
              "query" => defs.query_root_type_name = Some(type_name),
              "mutation" => defs.mutation_root_type_name = Some(type_name),
              "subscription" => defs.subscription_root_type_name = Some(type_name),
              _ => unreachable!("Invalid operation type {operation_type}"),
          }
      }

      if Token::BraceClose != ctx.next() {
          return syntax_err!("Expected `}}`");
      }

      Ok(defs)
  }
}

impl<'a> private::ParseFromCtx<'a> for DirectiveDefinition<'a> {
    #[inline]
    fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
        if ctx.next() != Token::Name("directive") {
            return syntax_err!("Directive definition must start with the `directive` keyword.");
        }
    
        let name = match ctx.next() {
            Token::DirectiveName(name) => name,
            t => return syntax_err!("Expected directive name, got {:?}", t),
        };

        let arguments = match ctx.peek() {
            Token::ParenOpen => ArgumentList::parse_from_ctx(ctx)?,
            _ => {
                ArgumentList::default_in(&ctx.ast_ctx.arena)
            }
        };

        dbg!(ctx.peek());
        let is_repeatable = match ctx.peek() {
            Token::Name("repeatable") => {
                ctx.next(); // Skip `repeatable`
                true
            }
            _ => false,
        };
    
        let locations = match ctx.peek() {
            Token::On => {
                ctx.next(); // Skip `on`
                let mut locations = Vec::new_in(&ctx.ast_ctx.arena);
                loop {
                    match ctx.next() {
                        Token::Name("QUERY") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::Query)),
                        Token::Name("MUTATION") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::Mutation)),
                        Token::Name("SUBSCRIPTION") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::Subscription)),
                        Token::Name("FIELD") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::Field)),
                        Token::Name("FRAGMENT_DEFINITION") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::FragmentDefinition)),
                        Token::Name("FRAGMENT_SPREAD") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::FragmentSpread)),
                        Token::Name("INLINE_FRAGMENT") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::InlineFragment)),
                        Token::Name("SCHEMA") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::Schema)),
                        Token::Name("SCALAR") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::Scalar)),
                        Token::Name("OBJECT") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::Object)),
                        Token::Name("FIELD_DEFINITION") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::FieldDefinition)),
                        Token::Name("ARGUMENT_DEFINITION") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::ArgumentDefinition)),
                        Token::Name("INTERFACE") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::Interface)),
                        Token::Name("UNION") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::Union)),
                        Token::Name("ENUM") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::Enum)),
                        Token::Name("ENUM_VALUE") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::EnumValue)),
                        Token::Name("INPUT_OBJECT") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::InputObject)),
                        Token::Name("INPUT_FIELD_DEFINITION") => locations.push(ctx.ast_ctx.alloc(DirectiveLocation::InputFieldDefinition)),
                        t => return syntax_err!("Expected directive location, got {:?}", t),
                    }
    
                    if ctx.peek() == &Token::Pipe {
                        ctx.next();
                    } else {
                        break;
                    }
                }

                locations
            }
            _ => {
                return syntax_err!("Expected `on`");
            }
        };

        Ok(DirectiveDefinition {
            name,
            locations,
            is_repeatable,
            arguments,
        })
    }
}

impl<'a> private::ParseFromCtx<'a> for TypeDefinition<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      match ctx.peek() {
          Token::Name("type") => SchemaObjectPlaceholder::parse_from_ctx(ctx)
              .map(TypeDefinition::ObjectTypeDefinition),
          Token::Name("input") => SchemaInputObjectPlaceholder::parse_from_ctx(ctx)
              .map(TypeDefinition::InputObjectTypeDefinition),
          Token::Name("enum") => {
              SchemaEnum::parse_from_ctx(ctx).map(TypeDefinition::EnumTypeDefinition)
          }
          Token::Name("scalar") => {
              SchemaScalar::parse_from_ctx(ctx).map(TypeDefinition::ScalarTypeDefinition)
          }
          Token::Name("interface") => SchemaInterfacePlaceholder::parse_from_ctx(ctx)
              .map(TypeDefinition::InterfaceTypeDefinition),
          Token::Name("union") => {
              SchemaUnionPlaceholder::parse_from_ctx(ctx).map(TypeDefinition::UnionTypeDefinition)
          }

          t => {
              syntax_err!("Expected valid type definition, got {:?}.", t)
          }
      }
  }
}

impl<'a> private::ParseFromCtx<'a> for SchemaObjectPlaceholder<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      if ctx.next() != Token::Name("type") {
          return syntax_err!("Object type must start with the `type` keyword.");
      }

      let name = match ctx.next() {
          Token::Name(name) => name,
          t => return syntax_err!("Expected type name, got {:?}", t),
      };

      let interfaces = ctx.get_implements_interfaces()?;

      let fields = match ctx.peek() {
          Token::BraceOpen => FieldDefinitions::parse_from_ctx(ctx)?,
          t => return syntax_err!("Expected `{{`, got {:?}", t),
      };

      Ok(SchemaObjectPlaceholder {
          name,
          fields,
          interfaces,
      })
  }
}

impl<'a> private::ParseFromCtx<'a> for SchemaInputObjectPlaceholder<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      if ctx.next() != Token::Name("input") {
          return syntax_err!("Input object type must start with the `input` keyword.");
      }

      let name = match ctx.next() {
          Token::Name(name) => name,
          t => return syntax_err!("Expected input type name, got {:?}", t),
      };

      let fields = match ctx.peek() {
          Token::BraceOpen => InputFieldDefinitions::parse_from_ctx(ctx)?,
          t => return syntax_err!("Expected `{{`, got {:?}", t),
      };

      Ok(SchemaInputObjectPlaceholder { name, fields })
  }
}

impl<'a> private::ParseFromCtx<'a> for SchemaInterfacePlaceholder<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      if ctx.next() != Token::Name("interface") {
          return syntax_err!("Interface type must start with the `interface` keyword.");
      }

      let name = match ctx.next() {
          Token::Name(name) => name,
          t => return syntax_err!("Expected type name, got {:?}", t),
      };

      let interfaces = ctx.get_implements_interfaces()?;

      let fields = match ctx.peek() {
          Token::BraceOpen => FieldDefinitions::parse_from_ctx(ctx)?,
          t => return syntax_err!("Expected `{{`, got {:?}", t),
      };

      Ok(SchemaInterfacePlaceholder {
          name,
          fields,
          interfaces,
          possible_types: Vec::new_in(&ctx.ast_ctx.arena),
          possible_interfaces: Vec::new_in(&ctx.ast_ctx.arena),
      })
  }
}

impl<'a> private::ParseFromCtx<'a> for SchemaUnionPlaceholder<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      if ctx.next() != Token::Name("union") {
          return syntax_err!("Union type must start with the `union` keyword.");
      }

      let name = match ctx.next() {
          Token::Name(name) => name,
          t => return syntax_err!("Expected type name, got {:?}", t),
      };

      let mut types = Vec::new_in(&ctx.ast_ctx.arena);
      if ctx.peek() == &Token::Equal {
          // Skip `=`
          ctx.next();

          // Skip optional leading `|`
          if ctx.peek() == &Token::Pipe {
              ctx.next();
          }

          // Get name of first type
          match ctx.next() {
              Token::Name(t) => types.push(t),
              t => return syntax_err!("Expected type name, got {:?}", t),
          }

          // Get any remaining types
          while ctx.peek() == &Token::Pipe {
              // Skip `|`
              ctx.next();

              // Get name of next type
              match ctx.next() {
                  Token::Name(t) => types.push(t),
                  t => return syntax_err!("Expected interface name, got {:?}", t),
              }
          }
      }

      Ok(SchemaUnionPlaceholder {
          name,
          types: types,
      })
  }
}

impl<'a> private::ParseFromCtx<'a> for FieldDefinitions<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      if let Token::BraceOpen = ctx.peek() {
          ctx.next(); // Skip brace open

          let mut fields = HashMap::new_in(&ctx.ast_ctx.arena);
          while !matches!(ctx.peek(), Token::BraceClose | Token::End) {
              let field = SchemaFieldPlaceholder::parse_from_ctx(ctx)?;
              fields.insert(field.name, field);
          }

          match ctx.peek() {
              Token::BraceClose => {
                  ctx.next();
                  Ok(FieldDefinitions { fields })
              }

              t => return syntax_err!("Expected `}}`, got {:?}", t),
          }
      } else {
          Ok(FieldDefinitions::default_in(&ctx.ast_ctx.arena))
      }
  }
}

impl<'a> private::ParseFromCtx<'a> for InputFieldDefinitions<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      if let Token::BraceOpen = ctx.peek() {
          ctx.next(); // Skip brace open

          let mut fields = HashMap::new_in(&ctx.ast_ctx.arena);
          while !matches!(ctx.peek(), Token::BraceClose | Token::End) {
              let field = SchemaInputFieldPlaceholder::parse_from_ctx(ctx)?;
              fields.insert(field.name, field);
          }

          match ctx.peek() {
              Token::BraceClose => {
                  ctx.next();
                  Ok(InputFieldDefinitions { fields })
              }

              t => syntax_err!("Expected `}}`, got {:?}", t),
          }
      } else {
          Ok(InputFieldDefinitions::default_in(&ctx.ast_ctx.arena))
      }
  }
}

impl<'a> private::ParseFromCtx<'a> for SchemaFieldPlaceholder<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      let name = match ctx.next() {
          Token::Name(name) => name,
          t => return syntax_err!("Expected field name, got {:?}", t),
      };

      let arguments = match ctx.peek() {
          Token::Colon => ArgumentList::default_in(&ctx.ast_ctx.arena),
          Token::ParenOpen => ArgumentList::parse_from_ctx(ctx)?,
          t => return syntax_err!("Expected `(` or `:`, got {:?}", t),
      };

      if Token::Colon != ctx.next() {
          return syntax_err!("Expected `:`");
      }

      let output_type = ctx.ast_ctx.arena.alloc(TypeWrapper::parse_from_ctx(ctx)?);
      Ok(SchemaFieldPlaceholder {
          name,
          arguments,
          output_type,
      })
  }
}

impl<'a> private::ParseFromCtx<'a> for ArgumentList<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      if let Token::ParenOpen = ctx.peek() {
          ctx.next(); // Skip parens open

          let mut arguments = HashMap::new_in(&ctx.ast_ctx.arena);
          while !matches!(ctx.peek(), Token::ParenClose | Token::End) {
              let argument = SchemaInputFieldPlaceholder::parse_from_ctx(ctx)?;
              arguments.insert(argument.name, argument);
          }

          match ctx.peek() {
              Token::ParenClose => {
                  ctx.next();
                  Ok(ArgumentList { arguments })
              }

              t => return syntax_err!("Expected `)`, got {:?}", t),
          }
      } else {
          Ok(ArgumentList::default_in(&ctx.ast_ctx.arena))
      }
  }
}

impl<'a> private::ParseFromCtx<'a> for SchemaInputFieldPlaceholder<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      // Skip leading comma
      if let Token::Comma = ctx.peek() {
          ctx.next();
      }

      let name = match ctx.next() {
          Token::Name(name) => name,
          t => return syntax_err!("Expected input field name, got {:?}", t),
      };

      if Token::Colon != ctx.next() {
          return syntax_err!("Expected `:`");
      }

      let input_type = ctx.ast_ctx.arena.alloc(TypeWrapper::parse_from_ctx(ctx)?);
      Ok(SchemaInputFieldPlaceholder { name, input_type })
  }
}

impl<'a> private::ParseFromCtx<'a> for TypeWrapper<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      let typ = match ctx.next() {
          Token::Name(name) => TypeWrapper::Named(name),
          Token::BracketOpen => {
              let typ =
                  TypeWrapper::List(ctx.ast_ctx.arena.alloc(TypeWrapper::parse_from_ctx(ctx)?));
              if ctx.next() != Token::BracketClose {
                  return syntax_err!("Unterminated list");
              }

              typ
          }

          t => return syntax_err!("Expected type name or list type start `[`, got {:?}", t),
      };

      match ctx.peek() {
          Token::Exclam => {
              ctx.next();
              Ok(TypeWrapper::NonNull(ctx.ast_ctx.arena.alloc(typ)))
          }
          _ => Ok(typ),
      }
  }
}

impl<'a> private::ParseFromCtx<'a> for SchemaEnum<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      if ctx.next() != Token::Name("enum") {
          return syntax_err!("Enum definition must start with the `enum` keyword.");
      }

      let name = match ctx.next() {
          Token::Name(name) => name,
          t => return syntax_err!("Expected type name, got {:?}", t),
      };

      let mut values = HashSet::new_in(&ctx.ast_ctx.arena);
      match ctx.next() {
          Token::BraceOpen => loop {
              match ctx.next() {
                  Token::BraceClose => break,
                  Token::Name(name) => {
                    values.insert(name);
                  },
                  t => {
                      return syntax_err!(
                          "Expected either closing brace or value name, got {:?}",
                          t
                      )
                  }
              }
          },

          t => {
              return syntax_err!(
                  "Enum definition must include values starting with `{{`, got {:?}",
                  t
              )
          }
      };

      Ok(SchemaEnum { name, values })
  }
}

impl<'a> private::ParseFromCtx<'a> for SchemaScalar<'a> {
  #[inline]
  fn parse_from_ctx(ctx: &mut private::ParserContext<'a>) -> ParseResult<Self> {
      if ctx.next() != Token::Name("scalar") {
          return syntax_err!("Scalar definition must start with the `scalar` keyword.");
      }

      let name = match ctx.next() {
          Token::Name(name) => name,
          t => return syntax_err!("Expected type name, got {:?}", t),
      };

      Ok(SchemaScalar { name })
  }
}