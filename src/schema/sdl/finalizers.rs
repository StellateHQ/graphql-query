use bumpalo::collections::Vec;
use hashbrown::HashMap;

use super::parse_ast::*;
use crate::ast::ASTContext;
use crate::schema::{
    SchemaField, SchemaFields, SchemaInputField, SchemaInputObject, SchemaInterface, SchemaInterfaces, SchemaObject, SchemaPossibleTypes, SchemaType, SchemaUnion, TypeRef
};

fn convert_to_schema_field<'a>(ctx: &'a ASTContext, name: &'a str, field: &'a SchemaFieldPlaceholder) -> SchemaField<'a> {
  let mut arguments = HashMap::new_in(&ctx.arena);
  for (name, argument) in field.arguments.arguments.iter() {
      arguments.insert(
          *name,
          SchemaInputField {
              name,
              input_type: map_input_type(ctx, argument.input_type),
          },
      );
  }

    SchemaField {
        name,
        arguments,
        output_type: map_output_type(ctx, field.output_type),
    }
}

fn map_input_type<'a>(
  arena: &'a ASTContext,
  wrapper: &'a TypeWrapper<'a>,
) -> &'a TypeRef<'a> {
  match wrapper {
      TypeWrapper::NonNull(inner) => {
          arena.alloc(TypeRef::NonNullType(map_input_type(arena, inner)))
      }
      TypeWrapper::List(inner) => {
          arena.alloc(TypeRef::ListType(map_input_type(arena, inner)))
      }
      TypeWrapper::Named(name) => {
          arena.alloc(TypeRef::Type(name))
      }
  }
}

fn map_output_type<'a>(
  arena: &'a ASTContext,
  wrapper: &'a TypeWrapper<'a>,
) -> &'a TypeRef<'a> {
  match wrapper {
      TypeWrapper::NonNull(inner) => {
          arena.alloc(TypeRef::NonNullType(map_output_type(arena, inner)))
      }

      TypeWrapper::List(inner) => {
          arena.alloc(TypeRef::ListType(map_output_type(arena, inner)))
      }

      TypeWrapper::Named(name) => {
          arena.alloc(TypeRef::Type(name))
      }
  }
}

/// Converts a raw TypeDefinition into a schema type, but doesn't initialize everything, like object fields.
/// Instead, it just creates the top level shells in order to allow referencing them in a second step (see finalizers).
pub(super) fn initialize_type_definition<'a>(
    ctx: &'a ASTContext,
    typ: &'a TypeDefinition<'a>,
) -> &'a SchemaType<'a> {
    match typ {
        TypeDefinition::ObjectTypeDefinition(obj) => {
            let mut fields = HashMap::new_in(&ctx.arena);
            for field in obj.fields.fields.iter() {
              let schema_field = convert_to_schema_field(ctx, field.0, field.1);
              fields.insert(*field.0, ctx.alloc(schema_field));
            }

            let mut interfaces = Vec::new_in(&ctx.arena);
            for interface in obj.interfaces.iter() {
                interfaces.push(*interface)
            }

            ctx.arena.alloc(SchemaType::Object(ctx.arena.alloc(SchemaObject {
                name: obj.name,
                fields,
                interfaces
            })))
        }
        TypeDefinition::InputObjectTypeDefinition(obj) => {
            let mut fields = HashMap::new_in(&ctx.arena);
            for (name, input_field) in obj.fields.fields.iter() {
                fields.insert(
                    *name,
                    SchemaInputField {
                        name,
                        input_type: map_input_type(ctx, input_field.input_type),
                    },
                );
            }

            ctx.arena.alloc(SchemaType::InputObject(ctx.arena.alloc(SchemaInputObject {
                name: obj.name,
                fields
            })))
        }
        TypeDefinition::EnumTypeDefinition(e) => ctx.arena.alloc(SchemaType::Enum(e)),
        TypeDefinition::ScalarTypeDefinition(s) => ctx.arena.alloc(SchemaType::Scalar(s)),

        TypeDefinition::InterfaceTypeDefinition(i) => {
            let mut schema_interface = SchemaInterface::new(ctx, i.name);
            for field in i.fields.fields.iter() {
                let schema_field = convert_to_schema_field(ctx, field.0, field.1);
                schema_interface.add_field(ctx, schema_field);
            }

            for obj in i.interfaces.iter() {
                schema_interface.add_interface(ctx, obj);
            }

            for obj in i.possible_types.iter() {
                schema_interface.add_possible_type(ctx, obj);
            }

            // TODO: possible interfaces aren't conveyed in the introspection
            //for obj in i.possible_interfaces.iter() {
            // schema_interface.add_possible_interface(ctx, obj);
            //}

            ctx.arena.alloc(SchemaType::Interface(
                ctx.arena.alloc(schema_interface),
            ))
        },
        TypeDefinition::UnionTypeDefinition(u) => {
            let mut schema_union = SchemaUnion::new(ctx, u.name);
            for obj in u.types.iter() {
                schema_union.add_possible_type(ctx, obj);
            }
            ctx.arena.alloc(SchemaType::Union(ctx.arena.alloc(schema_union)))
        }
    }
}
