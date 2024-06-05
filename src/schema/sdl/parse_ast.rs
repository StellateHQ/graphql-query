
use bumpalo::{collections::Vec, Bump};
use hashbrown::{hash_map::DefaultHashBuilder, HashMap};

use crate::{ast::DefaultIn, schema::{SchemaEnum, SchemaScalar}};

#[derive(Debug, Clone, Copy)]
pub(super) struct SchemaDefinition<'a> {
    pub(super) query_root_type_name: Option<&'a str>,
    pub(super) mutation_root_type_name: Option<&'a str>,
    pub(super) subscription_root_type_name: Option<&'a str>,
}

/// https://github.com/graphql/graphql-spec/blob/main/spec/Section%203%20--%20Type%20System.md#types
#[derive(Debug, Clone)]
pub(super) enum TypeDefinition<'a> {
    ObjectTypeDefinition(SchemaObjectPlaceholder<'a>),
    InputObjectTypeDefinition(SchemaInputObjectPlaceholder<'a>),
    EnumTypeDefinition(SchemaEnum<'a>),
    ScalarTypeDefinition(SchemaScalar<'a>),
    InterfaceTypeDefinition(SchemaInterfacePlaceholder<'a>),
    UnionTypeDefinition(SchemaUnionPlaceholder<'a>),
}

// https://github.com/graphql/graphql-spec/blob/main/spec/Section%203%20--%20Type%20System.md#directives
// #[derive(Debug, Clone)]
// pub(super) enum DirectiveDefinition<'a> {
//     DirectiveDefinition(&'a str),
// }

impl<'a> TypeDefinition<'a> {
    pub(crate) fn name(&self) -> &'a str {
        match self {
            TypeDefinition::ObjectTypeDefinition(obj) => obj.name,
            TypeDefinition::InputObjectTypeDefinition(obj) => obj.name,
            TypeDefinition::EnumTypeDefinition(e) => e.name,
            TypeDefinition::ScalarTypeDefinition(s) => s.name,
            TypeDefinition::InterfaceTypeDefinition(i) => i.name,
            TypeDefinition::UnionTypeDefinition(u) => u.name,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct SchemaObjectPlaceholder<'a> {
    pub name: &'a str,
    pub fields: FieldDefinitions<'a>,
    pub(crate) interfaces: Vec<'a, &'a str>,
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct SchemaInputObjectPlaceholder<'a> {
    pub name: &'a str,
    pub fields: InputFieldDefinitions<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct SchemaInterfacePlaceholder<'a> {
    pub name: &'a str,
    pub fields: FieldDefinitions<'a>,
    pub(crate) interfaces: Vec<'a, &'a str>,
    pub(crate) possible_types: Vec<'a, &'a str>,
    pub(crate) possible_interfaces: Vec<'a, &'a str>,
}

impl<'a> SchemaInterfacePlaceholder<'a> {
    pub fn add_possible_type(&mut self, possible_type: &'a str) {
        self.possible_types.push(possible_type);
    }

    pub fn add_possible_interface(&mut self, possible_type: &'a str) {
        self.possible_interfaces.push(possible_type);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct SchemaUnionPlaceholder<'a> {
    pub name: &'a str,
    pub(crate) types: Vec<'a, &'a str>,
}

/// A sequence of schema fields enclosed by braces, as found on object types, interfaces, etc.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct FieldDefinitions<'a> {
    pub fields: HashMap<&'a str, SchemaFieldPlaceholder<'a>, DefaultHashBuilder, &'a Bump>,
}

impl<'a> DefaultIn<'a> for FieldDefinitions<'a> {
  fn default_in(bump: &'a Bump) -> Self {
      Self {
          fields: HashMap::new_in(bump),
      }
  }
}

/// A sequence of schema fields enclosed by braces, as found on object types, interfaces, etc.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct InputFieldDefinitions<'a> {
    pub fields: HashMap<&'a str, SchemaInputFieldPlaceholder<'a>, DefaultHashBuilder, &'a Bump>,
}

impl<'a> DefaultIn<'a> for InputFieldDefinitions<'a> {
  fn default_in(bump: &'a Bump) -> Self {
      Self {
          fields: HashMap::new_in(bump),
      }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct ArgumentList<'a> {
    pub arguments: HashMap<&'a str, SchemaInputFieldPlaceholder<'a>, DefaultHashBuilder, &'a Bump>,
}

impl<'a> DefaultIn<'a> for ArgumentList<'a> {
    fn default_in(bump: &'a Bump) -> Self {
        Self {
            arguments: HashMap::new_in(bump),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct SchemaFieldPlaceholder<'a> {
    pub name: &'a str,
    pub arguments: ArgumentList<'a>,
    pub output_type: &'a TypeWrapper<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct SchemaInputFieldPlaceholder<'a> {
    pub name: &'a str,
    pub input_type: &'a TypeWrapper<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum TypeWrapper<'a> {
    NonNull(&'a TypeWrapper<'a>),
    List(&'a TypeWrapper<'a>),
    Named(&'a str),
}