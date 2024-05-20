use std::fmt;

/// An enum of identifiers representing AST nodes.
///
/// This enum can be printed using the [`fmt::Display`] trait.
/// When parsing this enum is used to indicate what AST node encountered a parsing error.
pub enum ASTKind {
    /// See: [crate::ast::Definition]
    Definition,
    /// See: [crate::ast::Document]
    Document,
    /// See: [crate::ast::OperationDefinition]
    OperationDefinition,
    /// See: [crate::ast::OperationKind]
    OperationKind,
    /// See: [crate::ast::FragmentDefinition]
    FragmentDefinition,
    /// See: [crate::ast::VariableDefinitions]
    VariableDefinitions,
    /// See: [crate::ast::VariableDefinition]
    VariableDefinition,
    /// See: [crate::ast::Type]
    Type,
    /// See: [crate::ast::NamedType]
    NamedType,
    /// See: `ListType` on [crate::ast::Type]
    ListType,
    /// See: `NonNullType` on [crate::ast::Type]
    NonNullType,
    /// See: [crate::ast::Field]
    Field,
    /// See: [crate::ast::FragmentSpread]
    FragmentSpread,
    /// See: [crate::ast::InlineFragment]
    InlineFragment,
    /// See: [crate::ast::SelectionSet]
    SelectionSet,
    /// See: [crate::ast::Selection]
    Selection,
    /// See: [crate::ast::Directives]
    Directives,
    /// See: [crate::ast::Directive]
    Directive,
    /// See: [crate::ast::Arguments]
    Arguments,
    /// See: [crate::ast::Argument]
    Argument,
    /// See: [crate::ast::ObjectValue]
    Object,
    /// See: [crate::ast::ObjectField]
    ObjectField,
    /// See: [crate::ast::Value]
    Value,
    /// See: [crate::ast::Variable]
    Variable,
    /// See: [crate::ast::StringValue]
    String,
    /// See: [crate::ast::FloatValue]
    Float,
    /// See: [crate::ast::IntValue]
    Int,
    /// See: [crate::ast::BooleanValue]
    Boolean,
    /// See: [crate::ast::EnumValue]
    Enum,
    /// See: [crate::ast::ListValue]
    List,
}

impl fmt::Display for ASTKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ASTKind::Definition => f.write_str("Definition"),
            ASTKind::Document => f.write_str("Document"),
            ASTKind::OperationDefinition => f.write_str("Operation Definition"),
            ASTKind::OperationKind => f.write_str("Operation Kind"),
            ASTKind::FragmentDefinition => f.write_str("Fragment Definition"),
            ASTKind::VariableDefinitions => f.write_str("Variable Definitions"),
            ASTKind::VariableDefinition => f.write_str("Variable Definition"),
            ASTKind::Type => f.write_str("Type"),
            ASTKind::NamedType => f.write_str("Type Name"),
            ASTKind::ListType => f.write_str("List Type"),
            ASTKind::NonNullType => f.write_str("Non-null Type"),
            ASTKind::Field => f.write_str("Field"),
            ASTKind::FragmentSpread => f.write_str("Fragment Spread"),
            ASTKind::InlineFragment => f.write_str("Inline Fragment"),
            ASTKind::SelectionSet => f.write_str("Selection Set"),
            ASTKind::Selection => f.write_str("Selection"),
            ASTKind::Directives => f.write_str("Directives"),
            ASTKind::Directive => f.write_str("Directive"),
            ASTKind::Arguments => f.write_str("Arguments"),
            ASTKind::Argument => f.write_str("Argument"),
            ASTKind::Object => f.write_str("Object"),
            ASTKind::ObjectField => f.write_str("Object Field"),
            ASTKind::Value => f.write_str("Value"),
            ASTKind::Variable => f.write_str("Variable"),
            ASTKind::String => f.write_str("String"),
            ASTKind::Float => f.write_str("Float"),
            ASTKind::Int => f.write_str("Integer"),
            ASTKind::Boolean => f.write_str("Boolean"),
            ASTKind::Enum => f.write_str("Enum"),
            ASTKind::List => f.write_str("List"),
        }
    }
}
