use bumpalo::collections::Vec;
use hashbrown::HashMap;

use super::parse_ast::*;
use super::parser::ParseResult;
use crate::ast::ASTContext;
use crate::schema::{
    SchemaField, SchemaInputField, SchemaInputObject, SchemaInterface, SchemaObject, SchemaType, SchemaUnion, TypeRef
};

fn convert_to_schema_field<'a>(ctx: &'a ASTContext, name: &'a str, field: &'a SchemaFieldPlaceholder) -> ParseResult<SchemaField<'a>> {
  let mut arguments = HashMap::new_in(&ctx.arena);
  for (name, argument) in field.arguments.arguments.iter() {
      arguments.insert(
          *name,
          SchemaInputField {
              name,
              input_type: map_input_type(&ctx, argument.input_type)?,
          },
      );
  }

    Ok(SchemaField {
        name,
        arguments,
        output_type: map_output_type(&ctx, field.output_type)?,
    })
}

fn map_input_type<'a>(
  arena: &'a ASTContext,
  wrapper: &'a TypeWrapper<'a>,
) -> ParseResult<&'a TypeRef<'a>> {
  match wrapper {
      TypeWrapper::NonNull(inner) => {
          Ok(arena.alloc(TypeRef::NonNullType(map_input_type(arena, inner)?)))
      }

      TypeWrapper::List(inner) => {
          Ok(arena.alloc(TypeRef::ListType(map_input_type(arena, inner)?)))
      }

      TypeWrapper::Named(name) => {
          // Todo check if this is necessary? (relates to schema change)
          Ok(arena.alloc(TypeRef::Type(name)))
      }
  }
}

fn map_output_type<'a>(
  arena: &'a ASTContext,
  wrapper: &'a TypeWrapper<'a>,
) -> ParseResult<&'a TypeRef<'a>> {
  match wrapper {
      TypeWrapper::NonNull(inner) => {
          Ok(arena.alloc(TypeRef::NonNullType(map_output_type(arena, inner)?)))
      }

      TypeWrapper::List(inner) => {
          Ok(arena.alloc(TypeRef::ListType(map_output_type(arena, inner)?)))
      }

      TypeWrapper::Named(name) => {
          Ok(arena.alloc(TypeRef::Type(name)))
      }
  }
}

/// Converts a raw TypeDefinition into a schema type, but doesn't initialize everything, like object fields.
/// Instead, it just creates the top level shells in order to allow referencing them in a second step (see finalizers).
pub(super) fn initialize_type_definition<'a>(
    ctx: &'a ASTContext,
    typ: &'a TypeDefinition<'a>,
) -> ParseResult<&'a SchemaType<'a>> {
    match typ {
        TypeDefinition::ObjectTypeDefinition(obj) => {
            let mut fields = HashMap::new_in(&ctx.arena);
            for field in obj.fields.fields.iter() {
              let schema_field = convert_to_schema_field(ctx, field.0, field.1)?;
              fields.insert(*field.0, ctx.alloc(schema_field));
            }

            let mut interfaces = Vec::new_in(&ctx.arena);
            for interface in obj.interfaces.iter() {
                interfaces.push(*interface)
            }

            Ok(ctx.arena.alloc(SchemaType::Object(ctx.arena.alloc(SchemaObject {
                name: obj.name,
                fields,
                interfaces
            }))))
        }
        TypeDefinition::InputObjectTypeDefinition(obj) => {
            let mut fields = HashMap::new_in(&ctx.arena);
            for (name, input_field) in obj.fields.fields.iter() {
                fields.insert(
                    *name,
                    SchemaInputField {
                        name,
                        input_type: map_input_type(&ctx, input_field.input_type)?,
                    },
                );
            }

            Ok(ctx.arena.alloc(SchemaType::InputObject(ctx.arena.alloc(SchemaInputObject {
                name: obj.name,
                fields
            }))))
        }
        // TODO: similar to object-type definition but for input types
        TypeDefinition::EnumTypeDefinition(e) => Ok(ctx.arena.alloc(SchemaType::Enum(e))),
        TypeDefinition::ScalarTypeDefinition(s) => Ok(ctx.arena.alloc(SchemaType::Scalar(s))),
        TypeDefinition::InterfaceTypeDefinition(i) => Ok(ctx.arena.alloc(SchemaType::Interface(
            ctx.arena.alloc(SchemaInterface::new(ctx, i.name)),
        ))),
        TypeDefinition::UnionTypeDefinition(u) => {
          Ok(ctx.arena.alloc(SchemaType::Union(ctx.arena.alloc(SchemaUnion::new(ctx, u.name)))))
        }
    }
}
