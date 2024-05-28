use self::private::BuildSchemaContext;

use super::introspection::{IntrospectionQuery, IntrospectionSchema};
use super::schema::Schema;
use crate::ast::ASTContext;

pub(crate) mod private {
    use hashbrown::{hash_map::DefaultHashBuilder, HashMap};

    use super::super::{introspection::*, schema::*};
    use super::ASTContext;

    fn from_output_type_ref<'a>(
        ctx: &'a ASTContext,
        output: &IntrospectionOutputTypeRef,
    ) -> &'a TypeRef<'a> {
        use std::ops::Deref;
        let out = match output {
            IntrospectionOutputTypeRef::List { of_type } => {
                TypeRef::ListType(from_output_type_ref(ctx, of_type.deref()))
            }
            IntrospectionOutputTypeRef::NonNull { of_type } => {
                TypeRef::NonNullType(from_output_type_ref(ctx, of_type.deref()))
            }
            IntrospectionOutputTypeRef::ScalarType { name }
            | IntrospectionOutputTypeRef::EnumType { name }
            | IntrospectionOutputTypeRef::ObjectType { name }
            | IntrospectionOutputTypeRef::InterfaceType { name }
            | IntrospectionOutputTypeRef::UnionType { name } => {
                // TODO: Check whether type matches here
                let name = ctx.alloc_str(name);
                TypeRef::Type(name)
            }
        };

        ctx.alloc(out)
    }

    #[derive(Clone)]
    pub struct BuildSchemaContext<'arena> {
        pub(crate) ctx: &'arena ASTContext,
    }

    impl<'arena> BuildSchemaContext<'arena> {
        pub(crate) fn new(ctx: &'arena ASTContext) -> Self {
            BuildSchemaContext { ctx }
        }

        pub fn build_schema(
            &'arena self,
            introspection: &IntrospectionSchema<'arena>,
        ) -> &'arena Schema<'arena> {
            // We have created our initial set of types, this ensures that every type is present
            // before we start building them.
            let mut schema_types: HashMap<
                &str,
                &'arena SchemaType,
                DefaultHashBuilder,
                &bumpalo::Bump,
            > = HashMap::new_in(&self.ctx.arena);
            for introspection_type in introspection.types.iter() {
                let schema_type = BuildSchemaType::on_create(introspection_type, self, &introspection.types);
                schema_types.insert(
                    self.ctx.alloc_str(introspection_type.name()),
                    self.ctx.alloc(schema_type),
                );
            }

            let query_type = introspection
                .query_type
                .as_ref()
                .and_then(|type_ref| schema_types.get(&type_ref.name))
                .and_then(|schema_type| schema_type.object());

            let mutation_type = introspection
                .mutation_type
                .as_ref()
                .and_then(|type_ref| schema_types.get(&type_ref.name))
                .and_then(|schema_type| schema_type.object());

            let subscription_type = introspection
                .subscription_type
                .as_ref()
                .and_then(|type_ref| schema_types.get(&type_ref.name))
                .and_then(|schema_type| schema_type.object());

            self.ctx.alloc(Schema {
                query_type,
                mutation_type,
                subscription_type,
                types: schema_types,
            })
        }
    }

    pub trait BuildSchemaType<'arena, T>: Sized {
        fn on_create(&self, ctx: &'arena BuildSchemaContext<'arena>, introspection_types: &[IntrospectionType<'arena>]) -> T;
    }

    impl<'arena> BuildSchemaType<'arena, SchemaType<'arena>> for IntrospectionType<'arena> {
        #[inline]
        fn on_create(&self, ctx: &'arena BuildSchemaContext<'arena>, introspection_types: &[IntrospectionType<'arena>]) -> SchemaType<'arena> {
            match self {
                IntrospectionType::Scalar(scalar) => {
                    SchemaType::Scalar(ctx.ctx.alloc(scalar.on_create(ctx, introspection_types)))
                }
                IntrospectionType::Object(object) => {
                    SchemaType::Object(ctx.ctx.alloc(object.on_create(ctx, introspection_types)))
                }
                IntrospectionType::Interface(interface) => {
                    SchemaType::Interface(ctx.ctx.alloc(interface.on_create(ctx, introspection_types)))
                }
                IntrospectionType::Union(union_type) => {
                    SchemaType::Union(ctx.ctx.alloc(union_type.on_create(ctx, introspection_types)))
                }
                IntrospectionType::Enum(enum_type) => {
                    SchemaType::Enum(ctx.ctx.alloc(enum_type.on_create(ctx, introspection_types)))
                }
                IntrospectionType::InputObject(input_object) => {
                    SchemaType::InputObject(ctx.ctx.alloc(input_object.on_create(ctx, introspection_types)))
                }
            }
        }
    }

    impl<'arena> BuildSchemaType<'arena, SchemaScalar<'arena>> for IntrospectionScalarType<'arena> {
        #[inline]
        fn on_create(&self, ctx: &'arena BuildSchemaContext<'arena>, _introspection_types: &[IntrospectionType<'arena>]) -> SchemaScalar<'arena> {
            SchemaScalar::new(ctx.ctx.alloc_str(self.name))
        }
    }

    impl<'arena> BuildSchemaType<'arena, SchemaEnum<'arena>> for IntrospectionEnumType<'arena> {
        #[inline]
        fn on_create(&self, ctx: &'arena BuildSchemaContext<'arena>, _introspection_types: &[IntrospectionType<'arena>]) -> SchemaEnum<'arena> {
            let name = ctx.ctx.alloc_str(self.name);
            let mut enum_type = SchemaEnum::new(ctx.ctx, name);
            for value in self.enum_values.iter() {
                let value_name = ctx.ctx.alloc_str(value.name);
                enum_type.add_value(ctx.ctx, value_name);
            }
            enum_type
        }
    }

    impl<'arena> BuildSchemaType<'arena, SchemaUnion<'arena>> for IntrospectionUnionType<'arena> {
        #[inline]
        fn on_create(&self, ctx: &'arena BuildSchemaContext<'arena>, _introspection_types: &[IntrospectionType<'arena>]) -> SchemaUnion<'arena> {
            let name = ctx.ctx.alloc_str(self.name);
            let mut schema_union_type = SchemaUnion::new(ctx.ctx, name);
            for introspection_type_ref in self.possible_types.possible_types.iter() {
                let name = ctx.ctx.alloc_str(introspection_type_ref.name);
                schema_union_type.add_possible_type(ctx.ctx, name);
            }
            schema_union_type
        }
    }

    fn from_input_type_ref<'arena>(
        ctx: &'arena ASTContext,
        input: &IntrospectionInputTypeRef,
    ) -> &'arena TypeRef<'arena> {
        use std::ops::Deref;
        let type_ref = match input {
            IntrospectionInputTypeRef::List { of_type } => {
                TypeRef::ListType(from_input_type_ref(ctx, of_type.deref()))
            }
            IntrospectionInputTypeRef::NonNull { of_type } => {
                TypeRef::NonNullType(from_input_type_ref(ctx, of_type.deref()))
            }
            IntrospectionInputTypeRef::ScalarType { name }
            | IntrospectionInputTypeRef::EnumType { name }
            | IntrospectionInputTypeRef::InputObjectType { name } => {
                // TODO: Check whether type matches here
                let name = ctx.alloc_str(name);
                TypeRef::Type(name)
            }
        };

        ctx.alloc(type_ref)
    }

    impl<'arena> BuildSchemaType<'arena, SchemaObject<'arena>> for IntrospectionObjectType<'arena> {
        #[inline]
        fn on_create(&self, ctx: &'arena BuildSchemaContext<'arena>, _introspection_types: &[IntrospectionType<'arena>]) -> SchemaObject<'arena> {
            let name = ctx.ctx.alloc_str(self.name);
            let mut schema_object_type = SchemaObject::new(ctx.ctx, name);
            for field in self.implementation.fields.iter() {
                let field_name = ctx.ctx.alloc_str(field.name);
                let mut schema_field = SchemaField::new(
                     ctx.ctx,
                    field_name,
                    from_output_type_ref(ctx.ctx, &field.of_type),
                );

                for arg in field.args.iter() {
                    let arg_name = ctx.ctx.alloc_str(arg.name);
                    let input_field =
                        SchemaInputField::new(arg_name, from_input_type_ref(ctx.ctx, &arg.of_type));
                    schema_field.add_argument(ctx.ctx, input_field);
                }

                schema_object_type.add_field(ctx.ctx, schema_field);
            }

            if let Some(interfaces) = &self.implementation.interfaces {
                for introspection_type_ref in interfaces.iter() {
                    let name = ctx.ctx.alloc_str(introspection_type_ref.name);
                    schema_object_type.add_interface(ctx.ctx, name);
                }
            }
            schema_object_type
        }
    }

    impl<'arena> BuildSchemaType<'arena, SchemaInterface<'arena>>
        for IntrospectionInterfaceType<'arena>
    {
        #[inline]
        fn on_create(&self, ctx: &'arena BuildSchemaContext<'arena>, introspection_types: &[IntrospectionType<'arena>]) -> SchemaInterface<'arena> {
            let name = ctx.ctx.alloc_str(self.name);
            let mut schema_interface_type = SchemaInterface::new(ctx.ctx, name);

            for field in self.implementation.fields.iter() {
                let field_name = ctx.ctx.alloc_str(field.name);
                let mut schema_field = SchemaField::new(
                    ctx.ctx,
                    field_name,
                    from_output_type_ref(ctx.ctx, &field.of_type),
                );
                for arg in field.args.iter() {
                    let arg_name = ctx.ctx.alloc_str(arg.name);
                    let input_field =
                        SchemaInputField::new(arg_name, from_input_type_ref(ctx.ctx, &arg.of_type));
                    schema_field.add_argument(ctx.ctx, input_field);
                }
                schema_interface_type.add_field(ctx.ctx, schema_field);
            }

            if let Some(interfaces) = &self.implementation.interfaces {
                for introspection_type_ref in interfaces.iter() {
                    let name = ctx.ctx.alloc_str(introspection_type_ref.name);
                    schema_interface_type.add_interface(ctx.ctx, name);
                }
            }

            for introspection_type_ref in self.possible_types.possible_types.iter() {
                let name = ctx.ctx.alloc_str(introspection_type_ref.name);
                if let Some(kind) = introspection_type_ref.kind {
                    if kind == "INTERFACE" {
                        schema_interface_type.add_possible_interface(ctx.ctx, name);
                    } else {
                        schema_interface_type.add_possible_type(ctx.ctx, name);
                    }
                } else {
                    let introspection_type = introspection_types.iter().find(|f| f.name() == name);
                    if let Some(IntrospectionType::Interface(_)) = introspection_type {
                        schema_interface_type.add_possible_interface(ctx.ctx, name);
                    } else {
                        schema_interface_type.add_possible_type(ctx.ctx, name);
                    }
                }

            }

            schema_interface_type
        }
    }

    impl<'arena> BuildSchemaType<'arena, SchemaInputObject<'arena>>
        for IntrospectionInputObjectType<'arena>
    {
        #[inline]
        fn on_create(&self, ctx: &'arena BuildSchemaContext<'arena>, _introspection_types: &[IntrospectionType<'arena>]) -> SchemaInputObject<'arena> {
            let name = ctx.ctx.alloc_str(self.name);
            let mut input = SchemaInputObject::new(ctx.ctx, name);
            for field in self.input_fields.iter() {
                let field_name = ctx.ctx.alloc_str(field.name);
                let input_field =
                    SchemaInputField::new(field_name, from_input_type_ref(ctx.ctx, &field.of_type));
                input.add_field(ctx.ctx, input_field);
            }

            input
        }
    }
}

pub trait BuildClientSchema<'arena> {
    /// Converts the introspected data to a [Schema].
    fn build_client_schema(&self, ctx: &'arena ASTContext) -> &'arena Schema<'arena>;
}

impl<'arena> BuildClientSchema<'arena> for IntrospectionSchema<'arena> {
    /// Converts the introspected data to a [Schema].
    fn build_client_schema(&self, ctx: &'arena ASTContext) -> &'arena Schema<'arena> {
        let builder_ctx: &mut BuildSchemaContext =
            ctx.arena.alloc(private::BuildSchemaContext::new(ctx));
        builder_ctx.build_schema(self)
    }
}

impl<'arena> BuildClientSchema<'arena> for IntrospectionQuery<'arena> {
    /// Converts the introspected data to a [Schema].
    fn build_client_schema(&self, ctx: &'arena ASTContext) -> &'arena Schema<'arena> {
        self.schema.build_client_schema(ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::super::schema::{SchemaFields, SchemaPossibleTypes, SchemaSuperType};
    use super::*;

    #[test]
    fn build_schema() {
        let ctx = ASTContext::new();
        let introspection_json = include_str!("../../fixture/introspection_query.json");
        let introspection: IntrospectionQuery = serde_json::from_str(introspection_json).unwrap();
        let schema = introspection.build_client_schema(&ctx);

        let query_root_name = schema.query_type.map(|obj| obj.name).unwrap();
        assert_eq!(query_root_name, "query_root");

        assert!(std::ptr::eq(
            schema
                .get_type(query_root_name)
                .and_then(|t| t.object())
                .unwrap(),
            schema.query_type.unwrap()
        ));
    }

    #[test]
    fn schema_fields() {
        let ctx = ASTContext::new();
        let introspection_json = include_str!("../../fixture/introspection_query.json");
        let introspection: IntrospectionQuery = serde_json::from_str(introspection_json).unwrap();
        let schema = introspection.build_client_schema(&ctx);

        let todo_type = schema.get_type("Todo").and_then(|t| t.object()).unwrap();
        let author_type = schema.get_type("Author").unwrap();

        todo_type.get_field("id").unwrap();
        todo_type.get_field("text").unwrap();

        let author_field = todo_type.get_field("author").unwrap();
        let maybe_author_type = author_field.output_type.of_type(schema);
        assert!(schema.is_sub_type(*author_type, *maybe_author_type));
        assert!(author_type == maybe_author_type);
    }

    #[test]
    fn schema_abstract_relationships() {
        let ctx = ASTContext::new();
        let introspection_json = include_str!("../../fixture/introspection_query.json");
        let introspection: IntrospectionQuery = serde_json::from_str(introspection_json).unwrap();
        let schema = introspection.build_client_schema(&ctx);

        let type_itodo = schema
            .get_type("ITodo")
            .and_then(|t| t.interface())
            .unwrap();
        let type_bigtodo = schema.get_type("BigTodo").and_then(|t| t.object()).unwrap();
        let type_smalltodo = schema
            .get_type("SmallTodo")
            .and_then(|t| t.object())
            .unwrap();
        let type_search = schema
            .get_type("Search")
            .and_then(|t| t.union_type())
            .unwrap();

        assert!(type_search.get_possible_type("SmallTodo").is_some());
        assert!(type_search.get_possible_type("BigTodo").is_some());

        assert!(type_search.is_sub_type(type_smalltodo.into()));
        assert!(type_search.is_sub_type(type_bigtodo.into()));

        assert!(type_itodo.is_sub_type(type_smalltodo.into()));
        assert!(type_itodo.is_sub_type(type_bigtodo.into()));

        assert!(type_itodo.get_possible_type(type_bigtodo.name).is_some());
        assert!(type_itodo.get_possible_type(type_smalltodo.name).is_some());
    }
}
