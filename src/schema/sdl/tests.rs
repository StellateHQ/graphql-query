use super::{
  parse_ast::{
      ArgumentList, FieldDefinitions, SchemaFieldPlaceholder, SchemaInputFieldPlaceholder,
      SchemaInterfacePlaceholder, SchemaObjectPlaceholder, TypeWrapper,
  },
  parser::ParseSdl,
};
use crate::{
  ast::{ASTContext, DefaultIn},
  schema::{
      BuildClientSchema, IntrospectionQuery, Schema, SchemaEnum, SchemaField, SchemaFields, SchemaInterface, SchemaInterfaces, SchemaObject, SchemaPossibleTypes, SchemaScalar, SchemaType, SchemaUnion, TypeRef
  },
};
use bumpalo::collections::Vec;
use hashbrown::{HashMap, HashSet};
use indoc::indoc;

const DEFAULT_SCALARS: [&str; 5] = [
  "String",
  "Int",
  "Float",
  "Boolean",
  "ID",
];

fn assert_parse<'a, T: 'a>(ctx: &'a ASTContext, source: &'a str, expected: T)
where
  T: ParseSdl<'a> + std::fmt::Debug + PartialEq,
{
  assert_eq!(*T::parse(ctx, source).unwrap(), expected);
}

#[test]
fn empty_schema() {
  let ctx = ASTContext::new();
  let mut types = HashMap::new_in(&ctx.arena);
  for scalar in DEFAULT_SCALARS.iter() {
    types.insert(
      *scalar,
      ctx.alloc(crate::schema::SchemaType::Scalar(ctx.alloc(SchemaScalar { name: scalar }))),
    );
  }

  assert_parse(
      &ctx,
      "",
      Schema {
          query_type: None,
          mutation_type: None,
          subscription_type: None,
          types,
      },
  );
}

#[test]
fn schema_definition() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      schema {
          query: Foo
          mutation: Bar
      }

      type Foo {
        foo: String
      }

      type Bar {
        bar: String
      }
  "};


  let foo_field_type = TypeRef::Type("String");
  let foo_field = SchemaField::new(&ctx, "foo", &foo_field_type);
  let mut foo = SchemaObject::new(&ctx, "Foo");
  foo.add_field(&ctx, foo_field);

  let bar_field_type = TypeRef::Type("String");
  let bar_field = SchemaField::new(&ctx, "bar", &bar_field_type);
  let mut bar = SchemaObject::new(&ctx, "Bar");
  bar.add_field(&ctx, bar_field);

  let foo_type = SchemaType::Object(&foo);
  let bar_type = SchemaType::Object(&bar);
  let mut types = HashMap::new_in(&ctx.arena);
  types.insert("Foo", &foo_type);
  types.insert("Bar", &bar_type);

  for scalar in DEFAULT_SCALARS.iter() {
    types.insert(
      *scalar,
      ctx.alloc(crate::schema::SchemaType::Scalar(ctx.alloc(SchemaScalar { name: scalar }))),
    );
  }

  assert_parse(
      &ctx,
      source,
      Schema {
          query_type: Some(&foo),
          mutation_type: Some(&bar),
          subscription_type: None,
          types,
      },
  );
}

#[test]
fn schema_definition_repeated() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      schema {
          query: Foo
      }

      schema {
          query: Bar
      }

      type Foo {
        foo: String
      }

      type Bar {
        bar: String
      }
  "};

  assert_eq!(
      Schema::parse(&ctx, source).unwrap_err().message,
      "Must not specify more than one Schema Definition."
  );
}

#[test]
fn schema_definition_invalid_operation_type() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      schema {
          query: Foo
          doesNotExist: Foo
      }

      type Foo {
        foo: String
      }
  "};

  assert_eq!(
      Schema::parse(&ctx, source).unwrap_err().message,
      "Expected operation type, got Name(\"doesNotExist\")"
  );
}

#[test]
fn schema_definition_non_object_type() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      schema {
          query: Foo
      }

      scalar Foo
  "};

  assert_eq!(
      Schema::parse(&ctx, source).unwrap_err().message,
      "Query root type `Foo` must be an object type."
  );
}

#[test]
fn schema_definition_non_object_non_root_type() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      schema {
          query: Foo
      }

      type Foo {
        foo: String
      }

      # Note that it's fine to have a non-object type with a default root operation type name as long as the schema
      # definition does not include it.
      interface Mutation {
        foo: String
      }
  "};

  assert!(Schema::parse(&ctx, source).is_ok());
}

#[test]
fn object_type_definition() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      type MyType {
          fieldA: String
          fieldB: Int
      }
  "};

  let mut fields = HashMap::new_in(&ctx.arena);
  fields.insert(
      "fieldA",
      SchemaFieldPlaceholder {
          name: "fieldA",
          arguments: ArgumentList::default_in(&ctx.arena),
          output_type: &TypeWrapper::Named("String"),
      },
  );
  fields.insert(
      "fieldB",
      SchemaFieldPlaceholder {
          name: "fieldB",
          arguments: ArgumentList::default_in(&ctx.arena),
          output_type: &TypeWrapper::Named("Int"),
      },
  );

  assert_parse(
      &ctx,
      source,
      SchemaObjectPlaceholder {
          name: "MyType",
          fields: FieldDefinitions { fields },
          interfaces: Vec::new_in(&ctx.arena)
      },
  );
}

#[test]
fn interface_type_definition() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      interface MyInterface {
          fieldA: String
          fieldB: Int
      }
  "};

  let mut fields = HashMap::new_in(&ctx.arena);
  fields.insert(
      "fieldA",
      SchemaFieldPlaceholder {
          name: "fieldA",
          arguments: ArgumentList::default_in(&ctx.arena),
          output_type: &TypeWrapper::Named("String"),
      },
  );
  fields.insert(
      "fieldB",
      SchemaFieldPlaceholder {
          name: "fieldB",
          arguments: ArgumentList::default_in(&ctx.arena),
          output_type: &TypeWrapper::Named("Int"),
      },
  );

  assert_parse(
      &ctx,
      source,
      SchemaInterfacePlaceholder {
          name: "MyInterface",
          fields: FieldDefinitions { fields },
          interfaces: Vec::new_in(&ctx.arena),
          possible_interfaces: Vec::new_in(&ctx.arena),
          possible_types: Vec::new_in(&ctx.arena),
      },
  );
}

#[test]
fn objects_implementing_interfaces() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      type MyType implements MyInt1 & MyInt2 {
          fieldA: String
          fieldB: Int
      }

      interface MyInt1 {
          fieldA: String
      }

      interface MyInt2 {
          fieldB: Int
      }
  "};

  let field_a_type = TypeRef::Type("String");
  let field_a = SchemaField::new(&ctx,"fieldA", &field_a_type);
  let field_b_type = TypeRef::Type("Int");
  let field_b = SchemaField::new(&ctx,"fieldB", &field_b_type);

  let mut interface1 = SchemaInterface::new(&ctx,"MyInt1");
  interface1.add_field(&ctx, field_a.clone());

  let mut interface2 = SchemaInterface::new(&ctx,"MyInt2");
  interface2.add_field(&ctx, field_b.clone());

  let mut obj = SchemaObject::new(&ctx,"MyType");
  obj.add_field(&ctx, field_a.clone());
  obj.add_field(&ctx, field_b.clone());
  obj.add_interface(&ctx, "MyInt1");
  obj.add_interface(&ctx, "MyInt2");

  let mut type_fields = HashMap::new_in(&ctx.arena);
  type_fields.insert("fieldA", &field_a);
  type_fields.insert("fieldB", &field_b);

  interface1.add_possible_type(&ctx, "MyType");
  interface2.add_possible_type(&ctx, "MyType");

  let obj = SchemaType::Object(&obj);
  let interface1 = SchemaType::Interface(&interface1);
  let interface2 = SchemaType::Interface(&interface2);
  let mut types = HashMap::new_in(&ctx.arena);
  types.insert("MyType", &obj);
  types.insert("MyInt1", &interface1);
  types.insert("MyInt2", &interface2);

  for scalar in DEFAULT_SCALARS.iter() {
    types.insert(
      *scalar,
      ctx.alloc(crate::schema::SchemaType::Scalar(ctx.alloc(SchemaScalar { name: scalar }))),
    );
  }

  assert_parse(
      &ctx,
      source,
      Schema {
          query_type: None,
          mutation_type: None,
          subscription_type: None,
          types,
      },
  );
}

#[test]
fn interfaces_implementing_interfaces() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      type MyType implements MyInt1 {
          fieldA: String
          fieldB: Int
      }

      interface MyInt1 implements MyInt2 {
          fieldA: String
          fieldB: Int
      }

      interface MyInt2 {
          fieldB: Int
      }
  "};

  let field_a_type = TypeRef::Type("String");
  let field_a = SchemaField::new(&ctx,"fieldA", &field_a_type);
  let field_b_type = TypeRef::Type("Int");
  let field_b = SchemaField::new(&ctx,"fieldB", &field_b_type);

  let mut interface2 = SchemaInterface::new(&ctx, "MyInt2");
  interface2.add_field(&ctx, field_b.clone());

  let mut interface1 = SchemaInterface::new(&ctx, "MyInt1");
  interface1.add_field(&ctx, field_a.clone());
  interface1.add_field(&ctx, field_b.clone());
  interface1.add_interface(&ctx, "MyInt2");

  let mut obj = SchemaObject::new(&ctx, "MyType");
  obj.add_field(&ctx, field_a);
  obj.add_field(&ctx, field_b);
  obj.add_interface(&ctx, "MyInt1");

  interface1.add_possible_type(&ctx, "MyType");
  //interface2.add_possible_interface(&ctx, "MyInt1");

  let obj = SchemaType::Object(&obj);
  let interface1 = SchemaType::Interface(&interface1);
  let interface2 = SchemaType::Interface(&interface2);
  let mut types = HashMap::new_in(&ctx.arena);
  types.insert("MyType", &obj);
  types.insert("MyInt1", &interface1);
  types.insert("MyInt2", &interface2);

  for scalar in DEFAULT_SCALARS.iter() {
    types.insert(
      *scalar,
      ctx.alloc(crate::schema::SchemaType::Scalar(ctx.alloc(SchemaScalar { name: scalar }))),
    );
  }

  assert_parse(
      &ctx,
      source,
      Schema {
          query_type: None,
          mutation_type: None,
          subscription_type: None,
          types,
      },
  );
}

#[test]
fn unions() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      type MyTypeA {
          fieldA: String
      }

      type MyTypeB {
          fieldB: Int
      }

      union MyUnion = MyTypeA | MyTypeB
  "};

  let field_a_type = TypeRef::Type("String");
  let field_a = SchemaField::new(&ctx,"fieldA", &field_a_type);
  let field_b_type = TypeRef::Type("Int");
  let field_b = SchemaField::new(&ctx,"fieldB", &field_b_type);

  let mut obj_a = SchemaObject::new(&ctx, "MyTypeA");
  obj_a.add_field(&ctx, field_a);

  let mut obj_b = SchemaObject::new(&ctx, "MyTypeB");
  obj_b.add_field(&ctx, field_b);

  let mut union = SchemaUnion::new(&ctx, "MyUnion");
  union.add_possible_type(&ctx, "MyTypeA");
  union.add_possible_type(&ctx, "MyTypeB");

  let obj_a = SchemaType::Object(&obj_a);
  let obj_b = SchemaType::Object(&obj_b);
  let union = SchemaType::Union(&union);
  let mut types = HashMap::new_in(&ctx.arena);
  types.insert("MyTypeA", &obj_a);
  types.insert("MyTypeB", &obj_b);
  types.insert("MyUnion", &union);

  for scalar in DEFAULT_SCALARS.iter() {
    types.insert(
      *scalar,
      ctx.alloc(crate::schema::SchemaType::Scalar(ctx.alloc(SchemaScalar { name: scalar }))),
    );
  }

  assert_parse(
      &ctx,
      source,
      Schema {
          query_type: None,
          mutation_type: None,
          subscription_type: None,
          types,
      },
  );
}

#[test]
fn enum_type_definition() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      enum MyEnum {
          FOO
          BAR
      }
  "};

  let mut values = HashSet::new_in(&ctx.arena);
  values.insert("FOO");
  values.insert("BAR");

  assert_parse(
      &ctx,
      source,
      SchemaEnum {
          name: "MyEnum",
          values,
      },
  );
}

#[test]
fn scalar_type_definition() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      scalar MyScalar
  "};

  assert_parse(&ctx, source, SchemaScalar { name: "MyScalar" });
}

#[test]
fn field_definition() {
  let ctx = ASTContext::new();
  let source = indoc! {"
          fieldA(arg1: Int, arg2: SomeType): String
  "};

  let mut arguments = HashMap::new_in(&ctx.arena);
  arguments.insert(
      "arg1",
      SchemaInputFieldPlaceholder {
          name: "arg1",
          input_type: &TypeWrapper::Named("Int"),
      },
  );
  arguments.insert(
      "arg2",
      SchemaInputFieldPlaceholder {
          name: "arg2",
          input_type: &TypeWrapper::Named("SomeType"),
      },
  );

  let arguments = ArgumentList { arguments };

  assert_parse(
      &ctx,
      source,
      SchemaFieldPlaceholder {
          name: "fieldA",
          arguments,
          output_type: &TypeWrapper::Named("String"),
      },
  );
}

#[test]
fn input_output_type() {
  let ctx = ASTContext::new();
  let source = indoc! {"
      String
  "};

  assert_parse(&ctx, source, TypeWrapper::Named("String"));

  let source = indoc! {"
      String!
  "};

  assert_parse(
      &ctx,
      source,
      TypeWrapper::NonNull(&TypeWrapper::Named("String")),
  );

  let source = indoc! {"
      [String]
  "};

  assert_parse(
      &ctx,
      source,
      TypeWrapper::List(&TypeWrapper::Named("String")),
  );

  let source = indoc! {"
      [String]!
  "};

  assert_parse(
      &ctx,
      source,
      TypeWrapper::NonNull(&TypeWrapper::List(&TypeWrapper::Named("String"))),
  );

  let source = indoc! {"
      [String!]
  "};

  assert_parse(
      &ctx,
      source,
      TypeWrapper::List(&TypeWrapper::NonNull(&TypeWrapper::Named("String"))),
  );

  let source = indoc! {"
      [[String]]
  "};

  assert_parse(
      &ctx,
      source,
      TypeWrapper::List(&TypeWrapper::List(&TypeWrapper::Named("String"))),
  );

  let source = indoc! {"
      [[String!]!]!
  "};

  assert_parse(
      &ctx,
      source,
      TypeWrapper::NonNull(&TypeWrapper::List(&TypeWrapper::NonNull(
          &TypeWrapper::List(&TypeWrapper::NonNull(&TypeWrapper::Named("String"))),
      ))),
  );
}

/// Complete test schema of what we currently support.
/// As opposed to the tests above, this is a full-stack test of actually building
/// a schema, not only building the partial templates used to assemble it.
#[test]
fn kitchen_sink() {
  let ctx = ASTContext::new();
  let source = indoc! {"
        type AllBasicScalars {
            string: String!
            stringOpt: String
            stringList: [String]
            int: Int!
            intOpt: Int
            intList: [Int]
            float: Float!
            floatOpt: Float
            floatList: [Float]
            bool: Boolean!
            boolOpt: Boolean
            boolList: [Boolean]
            id: ID!
            idOpt: ID
            idList: [ID]
        }

        type CircularA {
            other: String
            fieldB: CircularB!
        }

        type CircularB {
            fieldA: CircularA
            other: Int
        }

        union Circulars = CircularA | CircularB

        type FlatImpl implements SomeInterface1 {
            field1: String
        }

        type ListMadness {
            single: [String]
            required: [String]!
            reqReq: [String!]!
            multi: [[String]]
            multiReq: [[String]]!
            multiReqReq: [[String!]!]!
        }

        type Mutation {
            updateSomething(data: UpdateData!): Boolean!
        }

        scalar MyScalar

        input NestedData {
            booleanField: Boolean!
            stringField: String!
        }

        type NestedImpl implements SomeInterface1 & SomeInterface2 & SomeInterface3 {
            field1: String
            field2: String
            field3: String
        }

        type Query {
            someQuery(input: QueryInput!): String
        }

        input QueryInput {
            list: [String!]!
            someEnum: SomeEnum!
        }

        enum SomeEnum {
            ValOne
            ValTwo
            ValThree
        }

        interface SomeInterface1 {
            field1: String
        }

        interface SomeInterface2 {
            field2: String
        }

        interface SomeInterface3 implements SomeInterface1 & SomeInterface2 {
            field1: String
            field2: String
            field3: String
        }

        type Subscription {
            streamData: [String!]!
        }

        input UpdateData {
            id: ID!
            nested: NestedData
        }
  "};

  let introspection: IntrospectionQuery = serde_json::from_str(KITCHEN_SINK).unwrap();
  let mut expected = introspection.build_client_schema(&ctx).clone();
  for (key, _typ) in expected.types.clone().iter() {
    if key.starts_with("__") {
        expected.types.remove(key);
    }
}

  let parsed = Schema::parse(&ctx, source).unwrap();
  assert_eq!(parsed, &expected);
}

const KITCHEN_SINK: &str = r#"{"__schema":{"description":null,"queryType":{"name":"Query"},"mutationType":{"name":"Mutation"},"subscriptionType":{"name":"Subscription"},"types":[{"kind":"OBJECT","name":"AllBasicScalars","description":null,"specifiedByURL":null,"fields":[{"name":"string","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"stringOpt","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"stringList","description":null,"args":[],"type":{"kind":"LIST","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"int","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Int","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"intOpt","description":null,"args":[],"type":{"kind":"SCALAR","name":"Int","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"intList","description":null,"args":[],"type":{"kind":"LIST","name":null,"ofType":{"kind":"SCALAR","name":"Int","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"float","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Float","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"floatOpt","description":null,"args":[],"type":{"kind":"SCALAR","name":"Float","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"floatList","description":null,"args":[],"type":{"kind":"LIST","name":null,"ofType":{"kind":"SCALAR","name":"Float","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"bool","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"boolOpt","description":null,"args":[],"type":{"kind":"SCALAR","name":"Boolean","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"boolList","description":null,"args":[],"type":{"kind":"LIST","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"id","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"ID","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"idOpt","description":null,"args":[],"type":{"kind":"SCALAR","name":"ID","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"idList","description":null,"args":[],"type":{"kind":"LIST","name":null,"ofType":{"kind":"SCALAR","name":"ID","ofType":null}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"SCALAR","name":"String","description":"The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text.","specifiedByURL":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"SCALAR","name":"Int","description":"The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1.","specifiedByURL":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"SCALAR","name":"Float","description":"The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point).","specifiedByURL":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"SCALAR","name":"Boolean","description":"The `Boolean` scalar type represents `true` or `false`.","specifiedByURL":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"SCALAR","name":"ID","description":"The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID.","specifiedByURL":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"CircularA","description":null,"specifiedByURL":null,"fields":[{"name":"other","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"fieldB","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"CircularB","ofType":null}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"CircularB","description":null,"specifiedByURL":null,"fields":[{"name":"fieldA","description":null,"args":[],"type":{"kind":"OBJECT","name":"CircularA","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"other","description":null,"args":[],"type":{"kind":"SCALAR","name":"Int","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"UNION","name":"Circulars","description":null,"specifiedByURL":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":[{"kind":"OBJECT","name":"CircularA","ofType":null},{"kind":"OBJECT","name":"CircularB","ofType":null}]},{"kind":"OBJECT","name":"FlatImpl","description":null,"specifiedByURL":null,"fields":[{"name":"field1","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[{"kind":"INTERFACE","name":"SomeInterface1","ofType":null}],"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"ListMadness","description":null,"specifiedByURL":null,"fields":[{"name":"single","description":null,"args":[],"type":{"kind":"LIST","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"required","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}}},"isDeprecated":false,"deprecationReason":null},{"name":"reqReq","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}}}},"isDeprecated":false,"deprecationReason":null},{"name":"multi","description":null,"args":[],"type":{"kind":"LIST","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}}},"isDeprecated":false,"deprecationReason":null},{"name":"multiReq","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}}}},"isDeprecated":false,"deprecationReason":null},{"name":"multiReqReq","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}}}}}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"Mutation","description":null,"specifiedByURL":null,"fields":[{"name":"updateSomething","description":null,"args":[{"name":"data","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"INPUT_OBJECT","name":"UpdateData","ofType":null}},"defaultValue":null,"isDeprecated":false,"deprecationReason":null}],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"SCALAR","name":"MyScalar","description":null,"specifiedByURL":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"INPUT_OBJECT","name":"NestedData","description":null,"specifiedByURL":null,"fields":null,"inputFields":[{"name":"booleanField","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"defaultValue":null,"isDeprecated":false,"deprecationReason":null},{"name":"stringField","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"defaultValue":null,"isDeprecated":false,"deprecationReason":null}],"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"NestedImpl","description":null,"specifiedByURL":null,"fields":[{"name":"field1","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"field2","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"field3","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[{"kind":"INTERFACE","name":"SomeInterface1","ofType":null},{"kind":"INTERFACE","name":"SomeInterface2","ofType":null},{"kind":"INTERFACE","name":"SomeInterface3","ofType":null}],"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"Query","description":null,"specifiedByURL":null,"fields":[{"name":"someQuery","description":null,"args":[{"name":"input","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"INPUT_OBJECT","name":"QueryInput","ofType":null}},"defaultValue":null,"isDeprecated":false,"deprecationReason":null}],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"INPUT_OBJECT","name":"QueryInput","description":null,"specifiedByURL":null,"fields":null,"inputFields":[{"name":"list","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}}}},"defaultValue":null,"isDeprecated":false,"deprecationReason":null},{"name":"someEnum","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"ENUM","name":"SomeEnum","ofType":null}},"defaultValue":null,"isDeprecated":false,"deprecationReason":null}],"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"ENUM","name":"SomeEnum","description":null,"specifiedByURL":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":[{"name":"ValOne","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"ValTwo","description":null,"isDeprecated":false,"deprecationReason":null},{"name":"ValThree","description":null,"isDeprecated":false,"deprecationReason":null}],"possibleTypes":null},{"kind":"INTERFACE","name":"SomeInterface1","description":null,"specifiedByURL":null,"fields":[{"name":"field1","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":[{"kind":"OBJECT","name":"FlatImpl","ofType":null},{"kind":"OBJECT","name":"NestedImpl","ofType":null}]},{"kind":"INTERFACE","name":"SomeInterface2","description":null,"specifiedByURL":null,"fields":[{"name":"field2","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":[{"kind":"OBJECT","name":"NestedImpl","ofType":null}]},{"kind":"INTERFACE","name":"SomeInterface3","description":null,"specifiedByURL":null,"fields":[{"name":"field1","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"field2","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"field3","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[{"kind":"INTERFACE","name":"SomeInterface1","ofType":null},{"kind":"INTERFACE","name":"SomeInterface2","ofType":null}],"enumValues":null,"possibleTypes":[{"kind":"OBJECT","name":"NestedImpl","ofType":null}]},{"kind":"OBJECT","name":"Subscription","description":null,"specifiedByURL":null,"fields":[{"name":"streamData","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}}}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"INPUT_OBJECT","name":"UpdateData","description":null,"specifiedByURL":null,"fields":null,"inputFields":[{"name":"id","description":null,"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"ID","ofType":null}},"defaultValue":null,"isDeprecated":false,"deprecationReason":null},{"name":"nested","description":null,"type":{"kind":"INPUT_OBJECT","name":"NestedData","ofType":null},"defaultValue":null,"isDeprecated":false,"deprecationReason":null}],"interfaces":null,"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"__Schema","description":"A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations.","specifiedByURL":null,"fields":[{"name":"description","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"types","description":"A list of all types supported by this server.","args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Type","ofType":null}}}},"isDeprecated":false,"deprecationReason":null},{"name":"queryType","description":"The type that query operations will be rooted at.","args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Type","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"mutationType","description":"If this server supports mutation, the type that mutation operations will be rooted at.","args":[],"type":{"kind":"OBJECT","name":"__Type","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"subscriptionType","description":"If this server support subscription, the type that subscription operations will be rooted at.","args":[],"type":{"kind":"OBJECT","name":"__Type","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"directives","description":"A list of all directives supported by this server.","args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Directive","ofType":null}}}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"__Type","description":"The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.\n\nDepending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name, description and optional `specifiedByURL`, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.","specifiedByURL":null,"fields":[{"name":"kind","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"ENUM","name":"__TypeKind","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"name","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"description","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"specifiedByURL","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"fields","description":null,"args":[{"name":"includeDeprecated","description":null,"type":{"kind":"SCALAR","name":"Boolean","ofType":null},"defaultValue":"false","isDeprecated":false,"deprecationReason":null}],"type":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Field","ofType":null}}},"isDeprecated":false,"deprecationReason":null},{"name":"interfaces","description":null,"args":[],"type":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Type","ofType":null}}},"isDeprecated":false,"deprecationReason":null},{"name":"possibleTypes","description":null,"args":[],"type":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Type","ofType":null}}},"isDeprecated":false,"deprecationReason":null},{"name":"enumValues","description":null,"args":[{"name":"includeDeprecated","description":null,"type":{"kind":"SCALAR","name":"Boolean","ofType":null},"defaultValue":"false","isDeprecated":false,"deprecationReason":null}],"type":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__EnumValue","ofType":null}}},"isDeprecated":false,"deprecationReason":null},{"name":"inputFields","description":null,"args":[{"name":"includeDeprecated","description":null,"type":{"kind":"SCALAR","name":"Boolean","ofType":null},"defaultValue":"false","isDeprecated":false,"deprecationReason":null}],"type":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__InputValue","ofType":null}}},"isDeprecated":false,"deprecationReason":null},{"name":"ofType","description":null,"args":[],"type":{"kind":"OBJECT","name":"__Type","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"ENUM","name":"__TypeKind","description":"An enum describing what kind of type a given `__Type` is.","specifiedByURL":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":[{"name":"SCALAR","description":"Indicates this type is a scalar.","isDeprecated":false,"deprecationReason":null},{"name":"OBJECT","description":"Indicates this type is an object. `fields` and `interfaces` are valid fields.","isDeprecated":false,"deprecationReason":null},{"name":"INTERFACE","description":"Indicates this type is an interface. `fields`, `interfaces`, and `possibleTypes` are valid fields.","isDeprecated":false,"deprecationReason":null},{"name":"UNION","description":"Indicates this type is a union. `possibleTypes` is a valid field.","isDeprecated":false,"deprecationReason":null},{"name":"ENUM","description":"Indicates this type is an enum. `enumValues` is a valid field.","isDeprecated":false,"deprecationReason":null},{"name":"INPUT_OBJECT","description":"Indicates this type is an input object. `inputFields` is a valid field.","isDeprecated":false,"deprecationReason":null},{"name":"LIST","description":"Indicates this type is a list. `ofType` is a valid field.","isDeprecated":false,"deprecationReason":null},{"name":"NON_NULL","description":"Indicates this type is a non-null. `ofType` is a valid field.","isDeprecated":false,"deprecationReason":null}],"possibleTypes":null},{"kind":"OBJECT","name":"__Field","description":"Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type.","specifiedByURL":null,"fields":[{"name":"name","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"description","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"args","description":null,"args":[{"name":"includeDeprecated","description":null,"type":{"kind":"SCALAR","name":"Boolean","ofType":null},"defaultValue":"false","isDeprecated":false,"deprecationReason":null}],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__InputValue","ofType":null}}}},"isDeprecated":false,"deprecationReason":null},{"name":"type","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Type","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"isDeprecated","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"deprecationReason","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"__InputValue","description":"Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value.","specifiedByURL":null,"fields":[{"name":"name","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"description","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"type","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__Type","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"defaultValue","description":"A GraphQL-formatted string representing the default value for this input value.","args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"isDeprecated","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"deprecationReason","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"__EnumValue","description":"One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string.","specifiedByURL":null,"fields":[{"name":"name","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"description","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"isDeprecated","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"deprecationReason","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"OBJECT","name":"__Directive","description":"A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.\n\nIn some cases, you need to provide options to alter GraphQL's execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.","specifiedByURL":null,"fields":[{"name":"name","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"description","description":null,"args":[],"type":{"kind":"SCALAR","name":"String","ofType":null},"isDeprecated":false,"deprecationReason":null},{"name":"isRepeatable","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"isDeprecated":false,"deprecationReason":null},{"name":"locations","description":null,"args":[],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"ENUM","name":"__DirectiveLocation","ofType":null}}}},"isDeprecated":false,"deprecationReason":null},{"name":"args","description":null,"args":[{"name":"includeDeprecated","description":null,"type":{"kind":"SCALAR","name":"Boolean","ofType":null},"defaultValue":"false","isDeprecated":false,"deprecationReason":null}],"type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"LIST","name":null,"ofType":{"kind":"NON_NULL","name":null,"ofType":{"kind":"OBJECT","name":"__InputValue","ofType":null}}}},"isDeprecated":false,"deprecationReason":null}],"inputFields":null,"interfaces":[],"enumValues":null,"possibleTypes":null},{"kind":"ENUM","name":"__DirectiveLocation","description":"A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies.","specifiedByURL":null,"fields":null,"inputFields":null,"interfaces":null,"enumValues":[{"name":"QUERY","description":"Location adjacent to a query operation.","isDeprecated":false,"deprecationReason":null},{"name":"MUTATION","description":"Location adjacent to a mutation operation.","isDeprecated":false,"deprecationReason":null},{"name":"SUBSCRIPTION","description":"Location adjacent to a subscription operation.","isDeprecated":false,"deprecationReason":null},{"name":"FIELD","description":"Location adjacent to a field.","isDeprecated":false,"deprecationReason":null},{"name":"FRAGMENT_DEFINITION","description":"Location adjacent to a fragment definition.","isDeprecated":false,"deprecationReason":null},{"name":"FRAGMENT_SPREAD","description":"Location adjacent to a fragment spread.","isDeprecated":false,"deprecationReason":null},{"name":"INLINE_FRAGMENT","description":"Location adjacent to an inline fragment.","isDeprecated":false,"deprecationReason":null},{"name":"VARIABLE_DEFINITION","description":"Location adjacent to a variable definition.","isDeprecated":false,"deprecationReason":null},{"name":"SCHEMA","description":"Location adjacent to a schema definition.","isDeprecated":false,"deprecationReason":null},{"name":"SCALAR","description":"Location adjacent to a scalar definition.","isDeprecated":false,"deprecationReason":null},{"name":"OBJECT","description":"Location adjacent to an object type definition.","isDeprecated":false,"deprecationReason":null},{"name":"FIELD_DEFINITION","description":"Location adjacent to a field definition.","isDeprecated":false,"deprecationReason":null},{"name":"ARGUMENT_DEFINITION","description":"Location adjacent to an argument definition.","isDeprecated":false,"deprecationReason":null},{"name":"INTERFACE","description":"Location adjacent to an interface definition.","isDeprecated":false,"deprecationReason":null},{"name":"UNION","description":"Location adjacent to a union definition.","isDeprecated":false,"deprecationReason":null},{"name":"ENUM","description":"Location adjacent to an enum definition.","isDeprecated":false,"deprecationReason":null},{"name":"ENUM_VALUE","description":"Location adjacent to an enum value definition.","isDeprecated":false,"deprecationReason":null},{"name":"INPUT_OBJECT","description":"Location adjacent to an input object type definition.","isDeprecated":false,"deprecationReason":null},{"name":"INPUT_FIELD_DEFINITION","description":"Location adjacent to an input object field definition.","isDeprecated":false,"deprecationReason":null}],"possibleTypes":null}],"directives":[{"name":"include","description":"Directs the executor to include this field or fragment only when the `if` argument is true.","isRepeatable":false,"locations":["FIELD","FRAGMENT_SPREAD","INLINE_FRAGMENT"],"args":[{"name":"if","description":"Included when true.","type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"defaultValue":null,"isDeprecated":false,"deprecationReason":null}]},{"name":"skip","description":"Directs the executor to skip this field or fragment when the `if` argument is true.","isRepeatable":false,"locations":["FIELD","FRAGMENT_SPREAD","INLINE_FRAGMENT"],"args":[{"name":"if","description":"Skipped when true.","type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"Boolean","ofType":null}},"defaultValue":null,"isDeprecated":false,"deprecationReason":null}]},{"name":"deprecated","description":"Marks an element of a GraphQL schema as no longer supported.","isRepeatable":false,"locations":["FIELD_DEFINITION","ARGUMENT_DEFINITION","INPUT_FIELD_DEFINITION","ENUM_VALUE"],"args":[{"name":"reason","description":"Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted using the Markdown syntax, as specified by [CommonMark](https://commonmark.org/).","type":{"kind":"SCALAR","name":"String","ofType":null},"defaultValue":"\"No longer supported\"","isDeprecated":false,"deprecationReason":null}]},{"name":"specifiedBy","description":"Exposes a URL that specifies the behavior of this scalar.","isRepeatable":false,"locations":["SCALAR"],"args":[{"name":"url","description":"The URL that specifies the behavior of this scalar.","type":{"kind":"NON_NULL","name":null,"ofType":{"kind":"SCALAR","name":"String","ofType":null}},"defaultValue":null,"isDeprecated":false,"deprecationReason":null}]}]}}"#;