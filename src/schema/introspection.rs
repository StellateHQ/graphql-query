#[cfg(feature = "json")]
use serde::{Deserialize, Serialize};

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
pub struct IntrospectionQuery<'a> {
    #[cfg_attr(feature = "json", serde(rename = "__schema", borrow))]
    pub schema: IntrospectionSchema<'a>,
}

#[derive(Debug, Default)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionSchema<'a> {
    pub query_type: Option<IntrospectionNamedTypeRef<'a>>,
    pub mutation_type: Option<IntrospectionNamedTypeRef<'a>>,
    pub subscription_type: Option<IntrospectionNamedTypeRef<'a>>,
    #[cfg_attr(feature = "json", serde(borrow))]
    pub types: Vec<IntrospectionType<'a>>,
    #[cfg_attr(feature = "json", serde(skip))]
    pub directives: Option<Vec<IntrospectionDirective<'a>>>,
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionDeprecation<'a> {
    pub is_deprecated: Option<bool>,
    #[cfg_attr(feature = "json", serde(skip))]
    pub deprecation_reason: Option<&'a str>,
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionObjectImplementation<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub fields: Vec<IntrospectionField<'a>>,
    #[cfg_attr(feature = "json", serde(borrow))]
    pub interfaces: Option<Vec<IntrospectionNamedTypeRef<'a>>>,
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionPossibleTypes<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub possible_types: Vec<IntrospectionNamedTypeRef<'a>>,
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(
    feature = "json",
    serde(tag = "kind", rename_all = "SCREAMING_SNAKE_CASE")
)]
pub enum IntrospectionType<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    Scalar(IntrospectionScalarType<'a>),
    Object(IntrospectionObjectType<'a>),
    Interface(IntrospectionInterfaceType<'a>),
    Union(IntrospectionUnionType<'a>),
    Enum(IntrospectionEnumType<'a>),
    InputObject(IntrospectionInputObjectType<'a>),
}

impl<'a> IntrospectionType<'a> {
    #[inline]
    pub fn name(&self) -> &'a str {
        match self {
            IntrospectionType::Scalar(x) => x.name,
            IntrospectionType::Object(x) => x.name,
            IntrospectionType::Interface(x) => x.name,
            IntrospectionType::Union(x) => x.name,
            IntrospectionType::Enum(x) => x.name,
            IntrospectionType::InputObject(x) => x.name,
        }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(tag = "kind"))]
pub enum IntrospectionOutputTypeRef<'a> {
    #[cfg_attr(feature = "json", serde(rename = "LIST"))]
    List {
        #[cfg_attr(feature = "json", serde(rename = "ofType"))]
        of_type: Box<IntrospectionOutputTypeRef<'a>>,
    },
    #[cfg_attr(feature = "json", serde(rename = "NON_NULL"))]
    NonNull {
        #[cfg_attr(feature = "json", serde(rename = "ofType"))]
        of_type: Box<IntrospectionOutputTypeRef<'a>>,
    },
    #[cfg_attr(feature = "json", serde(rename = "SCALAR"))]
    ScalarType {
        #[cfg_attr(feature = "json", serde(borrow))]
        name: &'a str,
    },
    #[cfg_attr(feature = "json", serde(rename = "OBJECT"))]
    ObjectType {
        #[cfg_attr(feature = "json", serde(borrow))]
        name: &'a str,
    },
    #[cfg_attr(feature = "json", serde(rename = "INTERFACE"))]
    InterfaceType {
        #[cfg_attr(feature = "json", serde(borrow))]
        name: &'a str,
    },
    #[cfg_attr(feature = "json", serde(rename = "UNION"))]
    UnionType {
        #[cfg_attr(feature = "json", serde(borrow))]
        name: &'a str,
    },
    #[cfg_attr(feature = "json", serde(rename = "ENUM"))]
    EnumType {
        #[cfg_attr(feature = "json", serde(borrow))]
        name: &'a str,
    },
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(tag = "kind"))]
pub enum IntrospectionInputTypeRef<'a> {
    #[cfg_attr(feature = "json", serde(rename = "LIST"))]
    List {
        #[cfg_attr(feature = "json", serde(rename = "ofType"))]
        of_type: Box<IntrospectionInputTypeRef<'a>>,
    },
    #[cfg_attr(feature = "json", serde(rename = "NON_NULL"))]
    NonNull {
        #[cfg_attr(feature = "json", serde(rename = "ofType"))]
        of_type: Box<IntrospectionInputTypeRef<'a>>,
    },
    #[cfg_attr(feature = "json", serde(rename = "SCALAR"))]
    ScalarType {
        #[cfg_attr(feature = "json", serde(borrow))]
        name: &'a str,
    },
    #[cfg_attr(feature = "json", serde(rename = "ENUM"))]
    EnumType {
        #[cfg_attr(feature = "json", serde(borrow))]
        name: &'a str,
    },
    #[cfg_attr(feature = "json", serde(rename = "INPUT_OBJECT"))]
    InputObjectType {
        #[cfg_attr(feature = "json", serde(borrow))]
        name: &'a str,
    },
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionNamedTypeRef<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub kind: Option<&'a str>,
    #[cfg_attr(feature = "json", serde(borrow))]
    pub name: &'a str,
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionScalarType<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub name: &'a str,
    #[cfg_attr(feature = "json", serde(skip))]
    pub description: Option<&'a str>,
    #[cfg_attr(feature = "json", serde(skip))]
    pub specified_by_url: Option<&'a str>,
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionObjectType<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub name: &'a str,
    #[cfg_attr(feature = "json", serde(skip))]
    pub description: Option<&'a str>,
    #[cfg_attr(feature = "json", serde(flatten))]
    pub implementation: IntrospectionObjectImplementation<'a>,
}

impl<'a> From<IntrospectionObjectType<'a>> for IntrospectionObjectImplementation<'a> {
    #[inline]
    fn from(obj: IntrospectionObjectType<'a>) -> Self {
        obj.implementation
    }
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionInterfaceType<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub name: &'a str,
    #[cfg_attr(feature = "json", serde(skip))]
    pub description: Option<String>,
    #[cfg_attr(feature = "json", serde(flatten))]
    pub implementation: IntrospectionObjectImplementation<'a>,
    #[cfg_attr(feature = "json", serde(flatten))]
    pub possible_types: IntrospectionPossibleTypes<'a>,
}

impl<'a> From<IntrospectionInterfaceType<'a>> for IntrospectionObjectImplementation<'a> {
    #[inline]
    fn from(obj: IntrospectionInterfaceType<'a>) -> Self {
        obj.implementation
    }
}

impl<'a> From<IntrospectionInterfaceType<'a>> for IntrospectionPossibleTypes<'a> {
    #[inline]
    fn from(obj: IntrospectionInterfaceType<'a>) -> Self {
        obj.possible_types
    }
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionUnionType<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub name: &'a str,
    #[cfg_attr(feature = "json", serde(skip))]
    pub description: Option<&'a str>,
    #[cfg_attr(feature = "json", serde(flatten))]
    pub possible_types: IntrospectionPossibleTypes<'a>,
}

impl<'a> From<IntrospectionUnionType<'a>> for IntrospectionPossibleTypes<'a> {
    #[inline]
    fn from(obj: IntrospectionUnionType<'a>) -> Self {
        obj.possible_types
    }
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionEnumType<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub name: &'a str,
    #[cfg_attr(feature = "json", serde(skip))]
    pub description: Option<&'a str>,
    pub enum_values: Vec<IntrospectionEnumValue<'a>>,
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionInputObjectType<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub name: &'a str,
    #[cfg_attr(feature = "json", serde(skip))]
    pub description: Option<&'a str>,
    pub input_fields: Vec<IntrospectionInputValue<'a>>,
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionField<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub name: &'a str,
    #[cfg_attr(feature = "json", serde(skip))]
    pub description: Option<&'a str>,

    pub args: Vec<IntrospectionInputValue<'a>>,

    #[cfg_attr(feature = "json", serde(rename = "type"))]
    pub of_type: IntrospectionOutputTypeRef<'a>,
    #[cfg_attr(feature = "json", serde(flatten))]
    pub deprecation: IntrospectionDeprecation<'a>,
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionInputValue<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub name: &'a str,
    #[cfg_attr(feature = "json", serde(skip))]
    pub description: Option<&'a str>,
    #[cfg_attr(feature = "json", serde(skip))]
    pub default_value: Option<String>,

    #[cfg_attr(feature = "json", serde(rename = "type"))]
    pub of_type: IntrospectionInputTypeRef<'a>,

    #[cfg_attr(feature = "json", serde(flatten))]
    pub deprecation: IntrospectionDeprecation<'a>,
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionEnumValue<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub name: &'a str,
    #[cfg_attr(feature = "json", serde(skip))]
    pub description: Option<&'a str>,
    #[cfg_attr(feature = "json", serde(flatten))]
    pub deprecation: IntrospectionDeprecation<'a>,
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "camelCase"))]
pub struct IntrospectionDirective<'a> {
    #[cfg_attr(feature = "json", serde(borrow))]
    pub name: &'a str,
    #[cfg_attr(feature = "json", serde(skip))]
    pub description: Option<&'a str>,
    #[cfg_attr(feature = "json", serde(default))]
    pub is_repeatable: bool,
    pub locations: Vec<IntrospectionDirectiveLocation>,
    pub args: Vec<IntrospectionInputValue<'a>>,
}

#[derive(Debug)]
#[cfg_attr(feature = "json", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "json", serde(rename_all = "SCREAMING_SNAKE_CASE"))]
pub enum IntrospectionDirectiveLocation {
    Query,
    Mutation,
    Subscription,
    Field,
    FragmentDefinition,
    FragmentSpread,
    InlineFragment,
    VariableDefinition,
    Schema,
    Scalar,
    Object,
    FieldDefinition,
    ArgumentDefinition,
    Interface,
    Union,
    Enum,
    EnumValue,
    InputObject,
    InputFieldDefinition,
}
