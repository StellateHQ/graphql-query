use crate::ast::{ASTContext, DefaultIn, OperationKind};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use hashbrown::hash_map::DefaultHashBuilder;
use hashbrown::{HashMap, HashSet};

/// Schema Definition
///
/// A schema is created from root types for each kind of operation and is then used against
/// AST documents for validation and execution. In this library the schema is never executable and
/// serves only for metadata and type information. It is hence a "Client Schema".
/// [Reference](https://spec.graphql.org/October2021/#sec-Schema)
#[derive(Debug, Clone, PartialEq)]
pub struct Schema<'a> {
    pub(crate) query_type: Option<&'a SchemaObject<'a>>,
    pub(crate) mutation_type: Option<&'a SchemaObject<'a>>,
    pub(crate) subscription_type: Option<&'a SchemaObject<'a>>,
    pub(crate) types:
        hashbrown::HashMap<&'a str, &'a SchemaType<'a>, DefaultHashBuilder, &'a bumpalo::Bump>,
}

impl<'a> DefaultIn<'a> for Schema<'a> {
    fn default_in(arena: &'a Bump) -> Self {
        Schema {
            query_type: None,
            mutation_type: None,
            subscription_type: None,
            types: HashMap::new_in(&arena),
        }
    }
}

impl<'a> Schema<'a> {
    /// Returns whether the schema is a default, empty schema
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
            && self.query_type.is_none()
            && self.mutation_type.is_none()
            && self.subscription_type.is_none()
    }

    /// Returns the root object type for query operations
    #[inline]
    pub fn query_type(&self) -> Option<&'a SchemaObject<'a>> {
        self.query_type
    }

    /// Returns the root object type for mutation operations
    #[inline]
    pub fn mutation_type(&self) -> Option<&'a SchemaObject<'a>> {
        self.mutation_type
    }

    /// Returns the root object type for subscription operations
    #[inline]
    pub fn subscription_type(&self) -> Option<&'a SchemaObject<'a>> {
        self.subscription_type
    }

    /// Returns the appropriate object type depending on the passed operation kind
    #[inline]
    pub fn get_root_type(&self, operation_kind: OperationKind) -> Option<&'a SchemaObject<'a>> {
        match operation_kind {
            OperationKind::Query => self.query_type,
            OperationKind::Mutation => self.mutation_type,
            OperationKind::Subscription => self.subscription_type,
        }
    }

    /// Retrieves a kind by name from known schema types.
    #[inline]
    pub fn get_type(&self, name: &'a str) -> Option<&'a SchemaType<'a>> {
        self.types.get(name).map(|x| *x)
    }

    /// Checks whether a given type is a sub type of another.
    ///
    /// This is typically used for return types of fields. A return type may be any given sub type
    /// of the return type of said field.
    pub fn is_sub_type(&self, abstract_type: SchemaType<'a>, sub_type: SchemaType<'a>) -> bool {
        match abstract_type {
            SchemaType::Union(schema_union) => schema_union.is_sub_type(sub_type),
            SchemaType::Interface(schema_interface) => schema_interface.is_sub_type(sub_type),
            SchemaType::Object(schema_object) => {
                if let SchemaType::Object(sub_object_type) = sub_type {
                    sub_object_type == schema_object
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

/// Generic trait for any schema type that implements fields
pub trait SchemaFields<'a>: Sized {
    /// Add a new [SchemaField] to the list of fields
    fn add_field(&mut self, ctx: &'a ASTContext, field: SchemaField<'a>);

    /// Get a [Map] of all fields
    fn get_fields(
        &self,
    ) -> HashMap<&'a str, &'a SchemaField<'a>, DefaultHashBuilder, &'a bumpalo::Bump>;

    /// Get a known field by name
    fn get_field(&self, name: &'a str) -> Option<&SchemaField<'a>> {
        self.get_fields().get(name).map(|x| *x)
    }
}

/// Generic trait for any schema type that implements interfaces
pub trait SchemaInterfaces<'a>: Sized {
    /// Add a new [SchemaInterface] to the list of implemented interfaces
    fn add_interface(&mut self, ctx: &'a ASTContext, interface: &'a str);

    /// Get list of implemented [SchemaInterface]s
    fn get_interfaces(&self) -> Vec<'a, &'a str>;

    /// Checks whether given [ObjectType] is a possible subtype
    #[inline]
    fn implements_interface(&self, schema_interface: &SchemaInterface<'a>) -> bool {
        self.get_interfaces()
            .into_iter()
            .any(|interface| interface == schema_interface.name)
    }
}

/// Generic trait for any schema type that implements interfaces
pub trait SchemaPossibleTypes<'a>: Sized {
    /// Add a new [SchemaObject] to the list of possible types
    fn add_possible_type(&mut self, ctx: &'a ASTContext, object: &'a str);

    /// Get list of possible [SchemaObject] types
    fn get_possible_types(&self) -> Vec<'a, &'a str>;

    /// Get a specific possible type by name if it exists on the type
    #[inline]
    fn get_possible_type(&self, name: &'a str) -> Option<&'a str> {
        self.get_possible_types()
            .into_iter()
            .find(|&possible_type| possible_type == name)
    }

    /// Checks whether given [ObjectType] is a possible subtype
    #[inline]
    fn is_possible_type(&self, schema_object: &SchemaObject<'a>) -> bool {
        self.get_possible_types()
            .into_iter()
            .any(|possible_type| possible_type == schema_object.name)
    }
}

/// Generic trait for any schema type that may be a super type of other types
pub trait SchemaSuperType<'a>: Sized {
    /// Checks whether a given type is a sub type of the current super type.
    fn is_sub_type(&self, subtype: SchemaType<'a>) -> bool;
}

/// An Object type definition.
///
/// Most types in GraphQL are objects and define a set of fields and the interfaces they implement.
/// [Reference](https://spec.graphql.org/October2021/#sec-Objects)
#[derive(Debug, Clone, PartialEq)]
pub struct SchemaObject<'a> {
    pub name: &'a str,
    pub(crate) fields: HashMap<&'a str, &'a SchemaField<'a>, DefaultHashBuilder, &'a bumpalo::Bump>,
    pub(crate) interfaces: Vec<'a, &'a str>,
}

impl<'a> SchemaObject<'a> {
    #[inline]
    pub fn new(ctx: &'a ASTContext, name: &'a str) -> Self {
        SchemaObject {
            name,
            fields: HashMap::new_in(&ctx.arena),
            interfaces: Vec::new_in(&ctx.arena),
        }
    }
}

impl<'a> SchemaFields<'a> for SchemaObject<'a> {
    /// Add a new [SchemaField] to the list of fields
    fn add_field(&mut self, ctx: &'a ASTContext, field: SchemaField<'a>) {
        self.fields.insert(field.name, ctx.alloc(field));
    }

    /// Get a [Map] of all fields on the [SchemaObject]
    fn get_fields(
        &self,
    ) -> HashMap<&'a str, &'a SchemaField<'a>, DefaultHashBuilder, &'a bumpalo::Bump> {
        self.fields.clone()
    }
}

impl<'a> SchemaInterfaces<'a> for SchemaObject<'a> {
    /// Add a new [SchemaInterface] to the list of implemented interfaces
    fn add_interface(&mut self, _ctx: &'a ASTContext, interface: &'a str) {
        // TODO: this used to prepend
        self.interfaces.push(interface);
    }

    /// Get list of implemented [SchemaInterface]s
    #[inline]
    fn get_interfaces(&self) -> Vec<'a, &'a str> {
        self.interfaces.clone()
    }
}

/// An Interface type definition.
///
/// Any object or other interfaces may implement one or more interfaces and must then adhere to the
/// definition of this interface. A field that returns an interface as its return type may return
/// any object that implements this interface.
/// [Reference](https://spec.graphql.org/October2021/#sec-Interfaces)
#[derive(Clone, Debug)]
pub struct SchemaInterface<'a> {
    pub name: &'a str,
    pub(crate) fields: HashMap<&'a str, &'a SchemaField<'a>, DefaultHashBuilder, &'a bumpalo::Bump>,
    pub(crate) interfaces: Vec<'a, &'a str>,
    pub(crate) possible_interfaces: Vec<'a, &'a str>,
    pub(crate) possible_types: Vec<'a, &'a str>,
}

impl<'a> PartialEq for SchemaInterface<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && maps_are_equal(self.fields.clone(), other.fields.clone())
            && self.interfaces == other.interfaces
            && self.possible_interfaces == other.possible_interfaces
            && self.possible_types == other.possible_types
    }
}

impl<'a> SchemaInterface<'a> {
    #[inline]
    pub fn new(ctx: &'a ASTContext, name: &'a str) -> Self {
        SchemaInterface {
            name,
            fields: HashMap::new_in(&ctx.arena),
            interfaces: Vec::new_in(&ctx.arena),
            possible_interfaces: Vec::new_in(&ctx.arena),
            possible_types: Vec::new_in(&ctx.arena),
        }
    }

    /// Add a new [SchemaInterface] to the list that implements this [SchemaInterface]
    pub fn add_possible_interface(&mut self, _ctx: &'a ASTContext, interface: &'a str) {
        self.possible_interfaces.push(interface);
    }

    /// Get list of possible [SchemaInterface]s that implement this [SchemaInterface]
    #[inline]
    pub fn get_possible_interfaces(&self) -> Vec<'a, &'a str> {
        self.possible_interfaces.clone()
    }
}

impl<'a> SchemaFields<'a> for SchemaInterface<'a> {
    /// Add a new [SchemaField] to the list of fields
    fn add_field(&mut self, ctx: &'a ASTContext, field: SchemaField<'a>) {
        self.fields.insert(field.name, ctx.alloc(field));
    }

    /// Get a [Map] of all fields on the [SchemaInterface]
    fn get_fields(
        &self,
    ) -> HashMap<&'a str, &'a SchemaField<'a>, DefaultHashBuilder, &'a bumpalo::Bump> {
        self.fields.clone()
    }
}

impl<'a> SchemaInterfaces<'a> for SchemaInterface<'a> {
    /// Add a new [SchemaInterface] to the list of implemented interfaces
    fn add_interface(&mut self, _ctx: &'a ASTContext, interface: &'a str) {
        self.interfaces.push(interface);
    }

    /// Get list of implemented [SchemaInterface]s
    #[inline]
    fn get_interfaces(&self) -> Vec<'a, &'a str> {
        self.interfaces.clone()
    }
}

impl<'a> SchemaPossibleTypes<'a> for SchemaInterface<'a> {
    /// Add a new [SchemaObject] to the list of possible types
    fn add_possible_type(&mut self, _ctx: &'a ASTContext, object: &'a str) {
        self.possible_types.push(object);
    }

    /// Get list of possible [SchemaObject] types
    #[inline]
    fn get_possible_types(&self) -> Vec<'a, &'a str> {
        self.possible_types.clone()
    }
}

impl<'a> SchemaSuperType<'a> for SchemaInterface<'a> {
    #[inline]
    fn is_sub_type(&self, sub_type: SchemaType<'a>) -> bool {
        match sub_type {
            SchemaType::Object(schema_object) => schema_object.implements_interface(self),
            SchemaType::Interface(schema_interface) => schema_interface.implements_interface(self),
            _ => false,
        }
    }
}

/// An object Field type definition.
///
/// A field is like a function that given its arguments as input values produces an output value.
/// [Reference](https://spec.graphql.org/October2021/#FieldsDefinition)
#[derive(Debug, Clone, PartialEq)]
pub struct SchemaField<'a> {
    pub name: &'a str,
    pub arguments: HashMap<&'a str, SchemaInputField<'a>, DefaultHashBuilder, &'a bumpalo::Bump>,
    pub output_type: &'a TypeRef<'a>,
}

impl<'a> SchemaField<'a> {
    #[inline]
    pub fn new(ctx: &'a ASTContext, name: &'a str, output_type: &'a TypeRef<'a>) -> Self {
        SchemaField {
            name,
            arguments: HashMap::new_in(&ctx.arena),
            output_type,
        }
    }

    pub fn add_argument(&mut self, _ctx: &'a ASTContext, arg: SchemaInputField<'a>) {
        self.arguments.insert(arg.name, arg);
    }

    #[inline]
    pub fn get_argument(&self, name: &'a str) -> Option<&SchemaInputField<'a>> {
        self.arguments.get(name)
    }
}

/// A Union type definition.
///
/// A union contains a list of possible types that can be returned in its stead when its defined as
/// an output type.
/// [Reference](https://spec.graphql.org/October2021/#sec-Unions)
#[derive(Debug, Clone)]
pub struct SchemaUnion<'a> {
    pub name: &'a str,
    possible_types: Vec<'a, &'a str>,
}

impl<'a> PartialEq for SchemaUnion<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.possible_types == other.possible_types
    }
}

impl<'a> SchemaUnion<'a> {
    #[inline]
    pub fn new(ctx: &'a ASTContext, name: &'a str) -> Self {
        SchemaUnion {
            name,
            possible_types: Vec::new_in(&ctx.arena),
        }
    }

    #[inline]
    pub fn is_sub_type(&self, sub_type: SchemaType<'a>) -> bool {
        match sub_type {
            SchemaType::Object(schema_object) => self
                .possible_types
                .iter()
                .any(|possible| possible == &schema_object.name),
            _ => false,
        }
    }
}

impl<'a> SchemaPossibleTypes<'a> for SchemaUnion<'a> {
    /// Add a new [SchemaObject] to the list of possible types
    fn add_possible_type(&mut self, _ctx: &'a ASTContext, object: &'a str) {
        self.possible_types.push(object);
    }

    /// Get list of possible [SchemaObject] types
    #[inline]
    fn get_possible_types(&self) -> Vec<'a, &'a str> {
        self.possible_types.clone()
    }
}

impl<'a> SchemaSuperType<'a> for SchemaUnion<'a> {
    #[inline]
    fn is_sub_type(&self, sub_type: SchemaType<'a>) -> bool {
        if let SchemaType::Object(schema_object) = sub_type {
            self.is_possible_type(schema_object)
        } else {
            false
        }
    }
}

/// A Scalar type definition.
///
/// Scalars represent primitive leaf values in GraphQL that are represented with a specific
/// serializer and deserializer, which makes the values returnable to a GraphQL client or readable
/// by a GraphQL API.
/// [Reference](https://spec.graphql.org/October2021/#sec-Scalars)
#[derive(Debug, Clone, PartialEq)]
pub struct SchemaScalar<'a> {
    pub name: &'a str,
}

impl<'a> SchemaScalar<'a> {
    #[inline]
    pub fn new(name: &'a str) -> Self {
        SchemaScalar { name }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SchemaEnum<'a> {
    pub name: &'a str,
    pub values: HashSet<&'a str, DefaultHashBuilder, &'a bumpalo::Bump>,
}

impl<'a> SchemaEnum<'a> {
    #[inline]
    pub fn new(ctx: &'a ASTContext, name: &'a str) -> Self {
        SchemaEnum {
            name,
            values: HashSet::new_in(&ctx.arena),
        }
    }

    pub fn add_value(&mut self, _ctx: &'a ASTContext, value: &'a str) {
        self.values.insert(value);
    }
}

/// An Input Object type definition.
///
/// Inputs, such as arguments, may sometimes be nested and accept objects that must adhere to the
/// shape of an Input Object definition. This is often used to represent more complex inputs.
/// [Reference](https://spec.graphql.org/October2021/#sec-Input-Objects)
#[derive(Debug, Clone, PartialEq)]
pub struct SchemaInputObject<'a> {
    pub name: &'a str,
    pub fields: HashMap<&'a str, SchemaInputField<'a>, DefaultHashBuilder, &'a bumpalo::Bump>,
}

impl<'a> SchemaInputObject<'a> {
    #[inline]
    pub fn new(ctx: &'a ASTContext, name: &'a str) -> Self {
        SchemaInputObject {
            name,
            fields: HashMap::new_in(&ctx.arena),
        }
    }

    pub fn add_field(&mut self, _ctx: &'a ASTContext, field: SchemaInputField<'a>) {
        self.fields.insert(field.name, field);
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct SchemaInputField<'a> {
    pub name: &'a str,
    pub input_type: &'a TypeRef<'a>,
}

impl<'a> SchemaInputField<'a> {
    #[inline]
    pub fn new(name: &'a str, input_type: &'a TypeRef<'a>) -> Self {
        SchemaInputField { name, input_type }
    }
}

/// A named type enum that represents all possible GraphQL definition types.
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Types)
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SchemaType<'a> {
    InputObject(&'a SchemaInputObject<'a>),
    Object(&'a SchemaObject<'a>),
    Union(&'a SchemaUnion<'a>),
    Interface(&'a SchemaInterface<'a>),
    Scalar(&'a SchemaScalar<'a>),
    Enum(&'a SchemaEnum<'a>),
}

impl<'a> SchemaType<'a> {
    #[inline]
    pub fn name(&self) -> &'a str {
        match self {
            SchemaType::InputObject(x) => x.name,
            SchemaType::Object(x) => x.name,
            SchemaType::Union(x) => x.name,
            SchemaType::Interface(x) => x.name,
            SchemaType::Scalar(x) => x.name,
            SchemaType::Enum(x) => x.name,
        }
    }

    pub fn object(&self) -> Option<&'a SchemaObject<'a>> {
        match self {
            SchemaType::Object(x) => Some(x),
            _ => None,
        }
    }

    pub fn input_object(&self) -> Option<&'a SchemaInputObject<'a>> {
        match self {
            SchemaType::InputObject(x) => Some(x),
            _ => None,
        }
    }

    pub fn interface(&self) -> Option<&'a SchemaInterface<'a>> {
        match self {
            SchemaType::Interface(x) => Some(x),
            _ => None,
        }
    }

    pub fn union_type(&self) -> Option<&'a SchemaUnion<'a>> {
        match self {
            SchemaType::Union(x) => Some(x),
            _ => None,
        }
    }

    pub fn input_type(&self) -> Option<InputType<'a>> {
        match self {
            SchemaType::Scalar(x) => Some(InputType::Scalar(x)),
            SchemaType::Enum(x) => Some(InputType::Enum(x)),
            SchemaType::InputObject(x) => Some(InputType::InputObject(x)),
            _ => None,
        }
    }

    pub fn output_type(&self) -> Option<OutputType<'a>> {
        match self {
            SchemaType::Object(x) => Some(OutputType::Object(x)),
            SchemaType::Union(x) => Some(OutputType::Union(x)),
            SchemaType::Interface(x) => Some(OutputType::Interface(x)),
            SchemaType::Scalar(x) => Some(OutputType::Scalar(x)),
            SchemaType::Enum(x) => Some(OutputType::Enum(x)),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum OwnedSchemaType<'a> {
    InputObject(SchemaInputObject<'a>),
    Object(SchemaObject<'a>),
    Union(SchemaUnion<'a>),
    Interface(SchemaInterface<'a>),
    Scalar(SchemaScalar<'a>),
    Enum(SchemaEnum<'a>),
}

impl<'a> OwnedSchemaType<'a> {
    #[inline]
    pub fn name(&self) -> &'a str {
        match self {
            OwnedSchemaType::InputObject(x) => x.name,
            OwnedSchemaType::Object(x) => x.name,
            OwnedSchemaType::Union(x) => x.name,
            OwnedSchemaType::Interface(x) => x.name,
            OwnedSchemaType::Scalar(x) => x.name,
            OwnedSchemaType::Enum(x) => x.name,
        }
    }

    pub fn object(&'a self) -> Option<&'a SchemaObject<'a>> {
        match self {
            OwnedSchemaType::Object(x) => Some(x),
            _ => None,
        }
    }

    pub fn input_object(&'a self) -> Option<&'a SchemaInputObject<'a>> {
        match self {
            OwnedSchemaType::InputObject(x) => Some(x),
            _ => None,
        }
    }

    pub fn interface(&'a self) -> Option<&'a SchemaInterface<'a>> {
        match self {
            OwnedSchemaType::Interface(x) => Some(x),
            _ => None,
        }
    }

    pub fn union_type(&'a self) -> Option<&'a SchemaUnion<'a>> {
        match self {
            OwnedSchemaType::Union(x) => Some(x),
            _ => None,
        }
    }

    pub fn input_type(&'a self) -> Option<InputType<'a>> {
        match self {
            OwnedSchemaType::Scalar(x) => Some(InputType::Scalar(x)),
            OwnedSchemaType::Enum(x) => Some(InputType::Enum(x)),
            OwnedSchemaType::InputObject(x) => Some(InputType::InputObject(x)),
            _ => None,
        }
    }

    pub fn output_type(&'a self) -> Option<OutputType<'a>> {
        match self {
            OwnedSchemaType::Object(x) => Some(OutputType::Object(x)),
            OwnedSchemaType::Union(x) => Some(OutputType::Union(x)),
            OwnedSchemaType::Interface(x) => Some(OutputType::Interface(x)),
            OwnedSchemaType::Scalar(x) => Some(OutputType::Scalar(x)),
            OwnedSchemaType::Enum(x) => Some(OutputType::Enum(x)),
            _ => None,
        }
    }
}

impl<'a> From<&'a SchemaObject<'a>> for SchemaType<'a> {
    #[inline]
    fn from(schema_object: &'a SchemaObject<'a>) -> Self {
        SchemaType::Object(schema_object)
    }
}

impl<'a> From<&'a SchemaUnion<'a>> for SchemaType<'a> {
    #[inline]
    fn from(schema_union: &'a SchemaUnion<'a>) -> Self {
        SchemaType::Union(schema_union)
    }
}

impl<'a> From<&'a SchemaInterface<'a>> for SchemaType<'a> {
    #[inline]
    fn from(schema_interface: &'a SchemaInterface<'a>) -> Self {
        SchemaType::Interface(schema_interface)
    }
}

impl<'a> From<OutputType<'a>> for SchemaType<'a> {
    #[inline]
    fn from(type_ref: OutputType<'a>) -> Self {
        match type_ref {
            OutputType::Object(x) => SchemaType::Object(x),
            OutputType::Union(x) => SchemaType::Union(x),
            OutputType::Interface(x) => SchemaType::Interface(x),
            OutputType::Scalar(x) => SchemaType::Scalar(x),
            OutputType::Enum(x) => SchemaType::Enum(x),
        }
    }
}

impl<'a> From<InputType<'a>> for SchemaType<'a> {
    #[inline]
    fn from(type_ref: InputType<'a>) -> Self {
        match type_ref {
            InputType::InputObject(x) => SchemaType::InputObject(x),
            InputType::Scalar(x) => SchemaType::Scalar(x),
            InputType::Enum(x) => SchemaType::Enum(x),
        }
    }
}

/// An output type enum that represents all possible GraphQL definition types that a field may
/// return.
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Input-and-Output-Types)
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OutputType<'a> {
    Object(&'a SchemaObject<'a>),
    Union(&'a SchemaUnion<'a>),
    Interface(&'a SchemaInterface<'a>),
    Scalar(&'a SchemaScalar<'a>),
    Enum(&'a SchemaEnum<'a>),
}

impl<'a> OutputType<'a> {
    #[inline]
    pub fn name(&self) -> &str {
        match self {
            OutputType::Object(x) => x.name,
            OutputType::Union(x) => x.name,
            OutputType::Interface(x) => x.name,
            OutputType::Scalar(x) => x.name,
            OutputType::Enum(x) => x.name,
        }
    }

    #[inline]
    pub fn into_schema_type(&self) -> SchemaType<'a> {
        match self {
            OutputType::Object(x) => SchemaType::Object(x),
            OutputType::Union(x) => SchemaType::Union(x),
            OutputType::Interface(x) => SchemaType::Interface(x),
            OutputType::Scalar(x) => SchemaType::Scalar(x),
            OutputType::Enum(x) => SchemaType::Enum(x),
        }
    }
}

/// An input type enum that represents all possible GraphQL definition types that an argument or
/// input object field may accept.
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Input-and-Output-Types)
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum InputType<'a> {
    InputObject(&'a SchemaInputObject<'a>),
    Scalar(&'a SchemaScalar<'a>),
    Enum(&'a SchemaEnum<'a>),
}

impl<'a> InputType<'a> {
    #[inline]
    pub fn name(&self) -> &str {
        match self {
            InputType::InputObject(o) => o.name,
            InputType::Scalar(s) => s.name,
            InputType::Enum(e) => e.name,
        }
    }

    #[inline]
    pub fn named_type(&self) -> SchemaType<'a> {
        match self {
            InputType::InputObject(x) => SchemaType::InputObject(x),
            InputType::Scalar(x) => SchemaType::Scalar(x),
            InputType::Enum(x) => SchemaType::Enum(x),
        }
    }
}

#[derive(Clone, Copy)]
pub enum TypeRef<'a> {
    Type(&'a str),
    ListType(&'a TypeRef<'a>),
    NonNullType(&'a TypeRef<'a>),
}

impl<'a> TypeRef<'a> {
    #[inline]
    pub fn of_type(&self, schema: &'a Schema<'a>) -> &'a SchemaType<'a> {
        match self {
            TypeRef::Type(of_type) => {
                let schema_type = schema
                    .get_type(of_type)
                    .expect("Referenced type should exist in the schema.");
                schema_type
            }
            TypeRef::ListType(of_type) => of_type.of_type(schema),
            TypeRef::NonNullType(of_type) => of_type.of_type(schema),
        }
    }
}

/// This implementation is necessary to circuit break circular types.
/// Without this impl, `Debug` would print on and on, overflowing the stack as it's bouncing between types over and over.
impl<'a> std::fmt::Debug for TypeRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type(arg0) => f.debug_tuple("OutputTypeRef").field(&arg0).finish(),
            Self::ListType(arg0) => f.debug_tuple("ListType").field(arg0).finish(),
            Self::NonNullType(arg0) => f.debug_tuple("NonNullType").field(arg0).finish(),
        }
    }
}

/// This implementation is necessary to circuit break circular types.
/// Without this impl, `PartialEq` would never stop comparing types referencing each other.
/// We achieve this by only ever comparing type names, which is all we need for comparing references.
impl<'a> PartialEq for TypeRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Type(left), Self::Type(right)) => left == right,
            (Self::ListType(left), Self::ListType(right)) => left == right,
            (Self::NonNullType(left), Self::NonNullType(right)) => left == right,
            _ => false,
        }
    }
}

/// By default, Toolshed Maps also compare insertion order. This utility compares only entries, not ordering.
fn maps_are_equal<'a, V>(
    left: HashMap<&'a str, V, DefaultHashBuilder, &'a bumpalo::Bump>,
    right: HashMap<&'a str, V, DefaultHashBuilder, &'a bumpalo::Bump>,
) -> bool
where
    V: PartialEq + Copy,
{
    let length_matches = left.len() == right.len();

    // If the length matches and all entries in `left` are contained in `right` and equal, we can consider the maps equal.
    length_matches
        && left.iter().all(|(k, left_val)| {
            right
                .get(k)
                .map(|right_val| left_val == right_val)
                .unwrap_or(false)
        })
}

/// Helper trait to generalize comparisons (see `eq_named_lists`).
trait Named {
    fn name(&self) -> &str;
}

impl<'a> Named for &'a SchemaInterface<'a> {
    fn name(&self) -> &str {
        self.name
    }
}

impl<'a> Named for &'a SchemaObject<'a> {
    fn name(&self) -> &str {
        self.name
    }
}

impl<'a> Named for &'a SchemaUnion<'a> {
    fn name(&self) -> &str {
        self.name
    }
}
