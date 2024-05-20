use crate::{
    ast::OperationKind,
    error::{Error, Result},
};

use super::{
    OutputType, Schema, SchemaField, SchemaFields, SchemaInterfaces, SchemaObject,
    SchemaPossibleTypes,
};

/// Schema Reference
///
/// A stateful traverser that may be used to traverse a schema as a query is traversed. It supports
/// diving into types by specifying fields and fragment conditions. As a query is traversed it
/// keeps a stack of previous types, hence, as fields and fragments are traversed it can keep track
/// of the current type that a selection set is operating on.
#[derive(Clone)]
pub struct SchemaReference<'a> {
    output_stack: Vec<OutputType<'a>>,
    pointer: OutputType<'a>,
    schema: &'a Schema<'a>,
}

impl<'a> SchemaReference<'a> {
    /// Create a schema reference pointer from a given [SchemaObject] type to start from.
    #[inline]
    pub fn from_object_type(schema: &'a Schema, object: &'a SchemaObject<'a>) -> Self {
        SchemaReference {
            schema,
            output_stack: Vec::with_capacity(32),
            pointer: OutputType::Object(object),
        }
    }

    /// Create a schema reference pointer from a schema and selected fragment type-condition
    #[inline]
    pub fn from_fragment(schema: &'a Schema<'a>, type_condition: &'a str) -> Result<Self> {
        let typename = schema.get_type(type_condition);
        match typename {
            Some(typename) => Ok(SchemaReference {
                schema,
                output_stack: Vec::with_capacity(32),
                pointer: typename
                    .output_type()
                    .expect("This should be an object-output-type."),
            }),
            None => Err(Error::new(
                "Schema does not support given type-condition.",
                None,
            )),
        }
    }

    /// Create a schema reference pointer from a schema and selected root operation kind.
    #[inline]
    pub fn from_schema(schema: &'a Schema<'a>, operation_kind: OperationKind) -> Result<Self> {
        Ok(SchemaReference {
            schema,
            output_stack: Vec::with_capacity(32),
            pointer: OutputType::Object(schema.get_root_type(operation_kind).ok_or_else(|| {
                Error::new(
                    "Schema does not support selected root operation type.",
                    None,
                )
            })?),
        })
    }

    #[inline(always)]
    fn push_pointer(&mut self, pointer: OutputType<'a>) {
        self.output_stack.push(self.pointer);
        self.pointer = pointer;
    }

    /// Leave the current type and return to the previously pointed at output type.
    #[inline]
    pub fn leave_type(&mut self) -> Result<OutputType<'a>> {
        if let Some(pointer) = self.output_stack.pop() {
            self.pointer = pointer;
            Ok(pointer)
        } else {
            Err(Error::new("Cannot leave root type.", None))
        }
    }

    /// Returns the current pointer's referenced [OutputType].
    #[inline]
    pub fn output_type(&self) -> OutputType<'a> {
        self.pointer
    }

    /// Returns a field, if possible, on the current [OutputType].
    #[inline]
    pub fn get_field(&self, field_name: &'a str) -> Option<&SchemaField<'a>> {
        match self.pointer {
            OutputType::Object(object) => object.get_field(field_name),
            OutputType::Interface(interface) => interface.get_field(field_name),
            _ => None,
        }
    }

    /// Traverse deeper by selecting a field on the current [OutputType] and return the next
    /// [OutputType].
    #[inline]
    pub fn select_field(&mut self, field_name: &'a str) -> Result<OutputType<'a>> {
        let fields = match self.pointer {
            OutputType::Object(object) => Ok(object.get_fields()),
            OutputType::Interface(interface) => Ok(interface.get_fields()),
            _ => Err(Error::new(
                "Cannot select fields on non-object/interface type.",
                None,
            )),
        }?;
        if let Some(field) = fields.get(field_name) {
            let output_type = field
                .output_type
                .of_type(self.schema)
                .output_type()
                .expect("This to be an output type.");
            self.push_pointer(output_type);
            Ok(output_type)
        } else {
            Err(Error::new("Cannot select unknown fields.", None))
        }
    }

    /// Traverse deeper by applying a fragment condition on the current [OutputType] and return the next
    /// [OutputType].
    pub fn select_condition(&mut self, type_name: &'a str) -> Result<OutputType<'a>> {
        match self.pointer {
            OutputType::Object(schema_object) if schema_object.name == type_name => {
                self.push_pointer(self.pointer);
                Ok(self.pointer)
            }
            OutputType::Object(schema_object) => {
                let maybe_interface = schema_object
                    .get_interfaces()
                    .into_iter()
                    .find(|interface| *interface == type_name)
                    .map(|x| {
                        self.schema
                            .get_type(x)
                            .expect("The type to exist")
                            .output_type()
                            .expect("and it to be an output type.")
                    });
                if let Some(output_type) = maybe_interface {
                    self.push_pointer(output_type);
                    Ok(output_type)
                } else {
                    Err(Error::new(
                        "No possible interface type found for spread name.",
                        None,
                    ))
                }
            }
            OutputType::Interface(interface) => {
                let maybe_interface = interface
                    .get_interfaces()
                    .into_iter()
                    .chain(interface.get_possible_interfaces())
                    .find(|interface| *interface == type_name)
                    .map(|x| {
                        self.schema
                            .get_type(x)
                            .expect("The type to exist")
                            .output_type()
                            .expect("and it to be an output type.")
                    });
                if let Some(output_type) = maybe_interface {
                    self.push_pointer(output_type);
                    return Ok(output_type);
                };
                let maybe_object = interface
                    .get_possible_types()
                    .into_iter()
                    .find(|object| *object == type_name)
                    .map(|x| {
                        self.schema
                            .get_type(x)
                            .expect("The type to exist")
                            .output_type()
                            .expect("and it to be an output type.")
                    });
                if let Some(output_type) = maybe_object {
                    self.push_pointer(output_type);
                    Ok(output_type)
                } else {
                    Err(Error::new(
                        "No possible interface/object type found for spread name.",
                        None,
                    ))
                }
            }
            OutputType::Union(schema_union) => {
                let maybe_object = schema_union.get_possible_type(type_name).map(|x| {
                    self.schema
                        .get_type(x)
                        .expect("The type to exist")
                        .output_type()
                        .expect("and it to be an output type.")
                });
                if let Some(output_type) = maybe_object {
                    self.push_pointer(output_type);
                    Ok(output_type)
                } else {
                    Err(Error::new(
                        "No possible object type found for spread name.",
                        None,
                    ))
                }
            }
            OutputType::Scalar(_) | OutputType::Enum(_) => Err(Error::new(
                "Cannot spread fragment on non-abstract type.",
                None,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::{BuildClientSchema, IntrospectionQuery};
    use super::*;
    use crate::ast::ASTContext;

    #[test]
    fn walk_schema() {
        let ctx = ASTContext::new();
        let introspection_json = include_str!("../../fixture/introspection_query.json");
        let introspection: IntrospectionQuery = serde_json::from_str(introspection_json).unwrap();
        let schema = introspection.build_client_schema(&ctx);

        let mut schema_ref = SchemaReference::from_schema(schema, OperationKind::Query).unwrap();

        assert_eq!(schema_ref.output_type().name(), "query_root");

        let todo_type = schema_ref.select_field("todos").unwrap();
        assert_eq!(todo_type.name(), "Todo");

        let author_type = schema_ref.select_field("author").unwrap();
        assert_eq!(author_type.name(), "Author");

        assert_eq!(schema_ref.leave_type().unwrap().name(), "Todo");
        assert_eq!(schema_ref.leave_type().unwrap().name(), "query_root");
    }

    #[test]
    fn selecting_type_repeatedly() {
        let ctx = ASTContext::new();
        let introspection_json = include_str!("../../fixture/introspection_query.json");
        let introspection: IntrospectionQuery = serde_json::from_str(introspection_json).unwrap();
        let schema = introspection.build_client_schema(&ctx);

        let mut schema_ref = SchemaReference::from_schema(schema, OperationKind::Mutation).unwrap();

        assert_eq!(schema_ref.output_type().name(), "mutation_root");

        let mutation_type = schema_ref.select_condition("mutation_root").unwrap();
        assert_eq!(mutation_type.name(), "mutation_root");

        // We can leave once since we selected once...
        assert_eq!(schema_ref.leave_type().unwrap().name(), "mutation_root");
        // ..but not a second time
        assert_eq!(
            schema_ref.leave_type().unwrap_err().message,
            "Cannot leave root type."
        );
    }
}
