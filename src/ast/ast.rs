pub use super::ast_conversion::*;
use crate::error::{Error, ErrorType, Result};
use bumpalo::collections::CollectIn;
use hashbrown::{HashMap, hash_map::DefaultHashBuilder};

/// A context for a GraphQL document which holds an arena allocator.
///
/// For the duration of parsing, storing, validating, traversing, and printing an AST its
/// performant and convenient to allocate memory in one chunk for the AST's operations. This
/// context represents the lifetime of an AST and its derivatives.
///
/// An AST Context in other words represents the memory a query and the operations you perform on
/// it take up. This is efficient since once you're done with the query this entire allocated
/// memory can be dropped all at once. Hence however, it's inadvisable to reuse the AST Context
/// across multiple incoming GraphQL requests.
pub struct ASTContext {
    /// An arena allocator that holds the memory allocated for the AST Context's lifetime
    pub arena: bumpalo::Bump,
}

impl ASTContext {
    /// Create a new AST context with a preallocated arena.
    pub fn new() -> Self {
        let arena = bumpalo::Bump::new();
        ASTContext { arena }
    }

    /// Put the value of `item` onto the arena and return a reference to it.
    #[inline]
    pub fn alloc<T>(&self, item: T) -> &T {
        self.arena.alloc(item)
    }

    /// Allocate an `&str` slice onto the arena and return a reference to it.
    ///
    /// This is useful when the original slice has an undefined lifetime.
    /// This is typically unnecessary for static slices (`&'static str`) whose lifetimes are as
    /// long as the running program and don't need to be allocated dynamically.
    #[inline]
    pub fn alloc_str(&self, str: &str) -> &str {
        self.arena.alloc_str(str)
    }

    /// Puts a `String` onto the arena and returns a reference to it to tie the `String`'s lifetime
    /// to this AST context without reallocating or copying it.
    #[inline]
    pub fn alloc_string(&self, str: String) -> &str {
        self.arena.alloc(str)
    }
}

impl Default for ASTContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Map of AST Values for GraphQL Variables
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Coercing-Variable-Values)
pub type Variables<'a> = HashMap<&'a str, Value<'a>, DefaultHashBuilder, &'a bumpalo::Bump>;

/// AST Node of a boolean value
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Boolean-Value)
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct BooleanValue {
    pub value: bool,
}

/// AST Node of a variable identifier value.
///
/// These are identifiers prefixed with a `$` sign, typically in variable definitions.
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Language.Variables)
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Variable<'a> {
    pub name: &'a str,
}

/// AST Node of an enum value.
///
/// These are typically written in all caps and snake case, e.g. "`MOBILE_WEB`".
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Enum-Value)
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct EnumValue<'a> {
    pub value: &'a str,
}

/// AST Node of an integer value.
///
/// Integers in GraphQL are limited to 32-bit signed, non-fractional values.
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Int)
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct IntValue<'a> {
    pub value: &'a str,
}

/// AST Node of a floating point value.
///
/// Floats in GraphQL are signed, double precision values as defined by [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754).
/// They are however limited to finite values only.
/// [Reference](https://spec.graphql.org/October2021/#sec-Float)
#[derive(Debug, Clone, Copy)]
pub struct FloatValue<'a> {
    pub value: &'a str,
}

/// AST Node of a string value.
///
/// GraphQL has a number of escaped characters that are normalised away when parsing and
/// hence this `value` is expected to not contain escaped characters.
/// The strings in GraphQL can be compared to JSON Unicode strings.
/// [Reference](https://spec.graphql.org/October2021/#sec-String)
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct StringValue<'a> {
    pub value: &'a str,
}

impl<'a> StringValue<'a> {
    pub fn new<S: AsRef<str>>(ctx: &'a ASTContext, str: S) -> Self {
        StringValue {
            value: ctx.alloc_str(str.as_ref()),
        }
    }

    /// Determines whether a string should be printed as a block string
    /// rather than a regular single-line string.
    #[inline]
    pub fn is_block(&self) -> bool {
        let mut has_newline = false;
        let mut has_nonprintable = false;
        for c in self.value.chars() {
            match c {
                '\n' => has_newline = true,
                '\r' | '\t' | '\u{0020}'..='\u{FFFF}' => {}
                _ => has_nonprintable = true,
            }
        }
        has_newline && !has_nonprintable
    }
}

/// AST Node of possible input values in GraphQL.
///
/// Fields and Directives accept input values as arguments.
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Input-Values)
#[derive(Debug, PartialEq, Clone)]
pub enum Value<'a> {
    Variable(Variable<'a>),
    String(StringValue<'a>),
    Float(FloatValue<'a>),
    Int(IntValue<'a>),
    Boolean(BooleanValue),
    Enum(EnumValue<'a>),
    List(ListValue<'a>),
    Object(ObjectValue<'a>),
    /// Representing JSON-like `null` values or the absence of a value
    Null,
}

impl<'a> Value<'a> {
    pub fn is_truthy(&self, variables: Option<&Variables<'a>>) -> bool {
        match self {
            Value::Null => false,
            Value::Boolean(BooleanValue { value }) => *value,
            Value::Int(IntValue { value }) => {
                let int = value.parse::<i32>().unwrap_or(0);
                int != 0
            }
            Value::Float(FloatValue { value }) => {
                let float = value.parse::<f64>().unwrap_or(0.0);
                float != 0.0
            }
            Value::String(StringValue { value }) => !value.is_empty(),
            Value::List(_) | Value::Object(_) | Value::Enum(_) => true,
            Value::Variable(var) => variables
                .and_then(|vars| vars.get(var.name))
                .map(|value| value.is_truthy(None))
                .unwrap_or(false),
        }
    }
}

/// AST Node for a List of values.
///
/// Lists in GraphQL are ordered sequences and serialize to JSON arrays. Its
/// contents may be any arbitrary value literal or variable.
/// [Reference](https://spec.graphql.org/October2021/#sec-List-Value)
#[derive(Debug, PartialEq, Clone)]
pub struct ListValue<'a> {
    pub children: bumpalo::collections::Vec<'a, Value<'a>>,
}

impl<'a> ListValue<'a> {
    /// Checks whether this List contains any values.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.children.is_empty()
    }
}

/// AST Node for a field of an Object value.
///
/// Objects in GraphQL are unordered lists of keyed input values and serialize to JSON objects.
/// An Object literal's contents may be any arbitrary value literal or variable.
/// [Reference](https://spec.graphql.org/October2021/#ObjectField)
#[derive(Debug, PartialEq, Clone)]
pub struct ObjectField<'a> {
    pub name: &'a str,
    pub value: Value<'a>,
}

/// AST Node for an Object value, which is a list of Object fields.
///
/// Objects in GraphQL are unordered lists of keyed input values and serialize to JSON objects.
/// An Object literal's contents may be any arbitrary value literal or variable.
/// [Reference](https://spec.graphql.org/October2021/#sec-Input-Object-Values)
#[derive(Debug, PartialEq, Clone)]
pub struct ObjectValue<'a> {
    pub children: bumpalo::collections::Vec<'a, ObjectField<'a>>,
}

impl<'a> ObjectValue<'a> {
    /// Checks whether this Object contains any fields.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.children.is_empty()
    }

    /// Returns a `Map` keyed by all object field's names mapped to their values.
    pub fn as_map(
        &'a self,
        ctx: &'a ASTContext,
    ) -> HashMap<&str, &Value<'a>, DefaultHashBuilder, &'a bumpalo::Bump> {
        let mut map = HashMap::new_in(&ctx.arena);
        for field in self.children.iter() {
            map.insert(field.name, &field.value);
        }
        map
    }
}

/// AST Node for an Argument, which carries a name and a value.
///
/// Arguments in GraphQL are unordered lists of inputs to a field's or directive's arguments.
/// [Reference](https://spec.graphql.org/October2021/#Argument)
#[derive(Debug, PartialEq, Clone)]
pub struct Argument<'a> {
    pub name: &'a str,
    pub value: Value<'a>,
}

/// AST Node for a list of Arguments, which are similar to parameterized inputs to a function.
///
/// Arguments in GraphQL are unordered lists of inputs to a field's or directive's arguments.
/// [Reference](https://spec.graphql.org/October2021/#Arguments)
#[derive(Debug, PartialEq, Clone)]
pub struct Arguments<'a> {
    pub children: bumpalo::collections::Vec<'a, Argument<'a>>,
}

impl<'a> Arguments<'a> {
    /// Checks whether this list of Arguments contains any values.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.children.is_empty()
    }

    /// Converts `Arguments` into an `ObjectValue`
    #[inline]
    pub fn as_object_value(&'a self, ctx: &'a ASTContext) -> ObjectValue<'a> {
        let new_children = self
            .children
            .iter()
            .map(|arg| ObjectField {
                name: arg.name,
                value: arg.value.clone(),
            })
            .collect_in(&ctx.arena);

        ObjectValue {
            children: new_children,
        }
    }

    /// Returns a `Map` keyed by all arguments' names mapped to their values.
    pub fn as_map(
        &'a self,
        ctx: &'a ASTContext,
    ) -> HashMap<&str, &Value<'a>, DefaultHashBuilder, &'a bumpalo::Bump> {
        let mut map = HashMap::new_in(&ctx.arena);
        for argument in self.children.iter() {
            map.insert(argument.name, &argument.value);
        }
        map
    }
}

/// AST Node for GraphQL Directives, which provide a way to describe alternate behavior in GraphQL.
///
/// Typical directives that occur in queries are for example `@skip`, @include`, and `@defer`.
/// [Reference](https://spec.graphql.org/October2021/#sec-Language.Directives)
#[derive(Debug, PartialEq, Clone)]
pub struct Directive<'a> {
    pub name: &'a str,
    pub arguments: Arguments<'a>,
}

/// AST Node for lists of GraphQL Directives, which provide a way to describe alternate behavior in GraphQL.
///
/// Typical directives that occur in queries are for example `@skip`, @include`, and `@defer`.
/// [Reference](https://spec.graphql.org/October2021/#sec-Language.Directives)
#[derive(Debug, PartialEq, Clone)]
pub struct Directives<'a> {
    pub children: bumpalo::collections::Vec<'a, Directive<'a>>,
}

impl<'a> Directives<'a> {
    /// Checks whether this list of Directives contains any values.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.children.is_empty()
    }
}

/// AST Node for Selection Sets, which provide a way to select more information on a given parent.
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Selection-Sets)
#[derive(Debug, PartialEq, Clone)]
pub struct SelectionSet<'a> {
    pub selections: bumpalo::collections::Vec<'a, Selection<'a>>,
}

impl<'a> SelectionSet<'a> {
    /// Checks whether this Selection Set contains any selections.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.selections.is_empty()
    }
}

/// AST Node for Fields, which can be likened to functions or properties on a parent object.
///
/// In JSON this would represent a property in a JSON object.
/// [Reference](https://spec.graphql.org/October2021/#sec-Language.Fields)
#[derive(Debug, PartialEq, Clone)]
pub struct Field<'a> {
    /// A Field's `alias`, which is used to request information under a different name than the
    /// Field's `name`.
    /// [Reference](https://spec.graphql.org/October2021/#sec-Field-Alias)
    pub alias: Option<&'a str>,
    /// A Field's `name`, which represents a resolver on a GraphQL schema's object type.
    pub name: &'a str,
    /// Arguments that are passed to a Field.
    ///
    /// When no Arguments are passed, this will be an empty
    /// list, as can be checked using `Arguments::is_empty`.
    /// See: [Arguments]
    pub arguments: Arguments<'a>,
    /// Directives that are annotating this Field.
    ///
    /// When no Directives are present, this will be an empty
    /// list, as can be checked using `Directives::is_empty`.
    /// See: [Directives]
    pub directives: Directives<'a>,
    /// A sub-Selection Set that is passed below this field to add selections to this field's
    /// returned GraphQL object type.
    ///
    /// When no selections are present, this will be an empty
    /// list, as can be checked using `SelectionSet::is_empty`.
    /// See: [SelectionSet]
    pub selection_set: SelectionSet<'a>,
}

impl<'a> Field<'a> {
    /// Get the alias of the field, if present, otherwise get the name.
    #[inline]
    pub fn alias_or_name(&self) -> &'a str {
        self.alias.unwrap_or(self.name)
    }

    /// Creates a new leaf field with the given `name`.
    ///
    /// All sub-lists, like `arguments`, `directives` and `selection_set` will be created as empty
    /// defaults.
    #[inline]
    pub fn new_leaf(ctx: &'a ASTContext, name: &'a str) -> Self {
        Field {
            alias: None,
            name,
            arguments: Arguments::default_in(&ctx.arena),
            directives: Directives::default_in(&ctx.arena),
            selection_set: SelectionSet::default_in(&ctx.arena),
        }
    }

    /// Creates a new leaf field with the given `name` and `alias`.
    ///
    /// All sub-lists, like `arguments`, `directives` and `selection_set` will be created as empty
    /// defaults.
    #[inline]
    pub fn new_aliased_leaf(ctx: &'a ASTContext, alias: &'a str, name: &'a str) -> Self {
        Field {
            // NOTE: Optimise future cases of checking for aliases by pointer
            alias: Some(alias), //if alias == name { name } else { alias },
            name,
            arguments: Arguments::default_in(&ctx.arena),
            directives: Directives::default_in(&ctx.arena),
            selection_set: SelectionSet::default_in(&ctx.arena),
        }
    }
}

/// AST Node for a Fragment Spread, which refers to a [`FragmentDefinition`] with an additional
/// [`SelectionSet`].
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Language.Fragments)
#[derive(Debug, PartialEq, Clone)]
pub struct FragmentSpread<'a> {
    /// A given name of the [FragmentDefinition] that must be spread in place of this Fragment
    /// Spread on a GraphQL API.
    pub name: NamedType<'a>,
    /// Directives that are annotating this Fragment Spread.
    ///
    /// When no Directives are present, this will be an empty
    /// list, as can be checked using `Directives::is_empty`.
    /// See: [Directives]
    pub directives: Directives<'a>,
}

/// AST Node for an inline Fragment definition with an additional [`SelectionSet`].
/// This may only be applied when the type condition matches or when no type condition is present.
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Language.Fragments)
#[derive(Debug, PartialEq, Clone)]
pub struct InlineFragment<'a> {
    /// A given type condition's type name that must match before this fragment is applied on a
    /// GraphQL API. On inline fragments this is optional and no type condition has to be passed.
    pub type_condition: Option<NamedType<'a>>,
    /// Directives that are annotating this Inline Fragment.
    ///
    /// When no Directives are present, this will be an empty
    /// list, as can be checked using `Directives::is_empty`.
    /// See: [Directives]
    pub directives: Directives<'a>,
    /// A sub-Selection Set that is applied when this Fragment is applied to the parent
    /// Selection Set.
    /// See: [SelectionSet]
    pub selection_set: SelectionSet<'a>,
}

/// AST Node of a selection as contained inside a [`SelectionSet`].
///
/// Any given Selection Set may contain fields, fragment spread, and inline fragments.
/// [Reference](https://spec.graphql.org/October2021/#Selection)
#[derive(Debug, PartialEq, Clone)]
pub enum Selection<'a> {
    Field(Field<'a>),
    FragmentSpread(FragmentSpread<'a>),
    InlineFragment(InlineFragment<'a>),
}

impl<'a> Selection<'a> {
    /// Helper method to return the [`Field`] if the Selection is a `Field`.
    #[inline]
    pub fn field(&'a self) -> Option<&'a Field<'a>> {
        match self {
            Selection::Field(field) => Some(&field),
            Selection::FragmentSpread(_) => None,
            Selection::InlineFragment(_) => None,
        }
    }

    /// Helper method to return the [`FragmentSpread`] if the Selection is a `FragmentSpread`.
    #[inline]
    pub fn fragment_spread(&'a self) -> Option<&'a FragmentSpread<'a>> {
        match self {
            Selection::FragmentSpread(spread) => Some(&spread),
            Selection::Field(_) => None,
            Selection::InlineFragment(_) => None,
        }
    }

    /// Helper method to return the [`InlineFragment`] if the Selection is an `InlineFragment`.
    #[inline]
    pub fn inline_fragment(&'a self) -> Option<&'a InlineFragment<'a>> {
        match self {
            Selection::InlineFragment(fragment) => Some(&fragment),
            Selection::FragmentSpread(_) => None,
            Selection::Field(_) => None,
        }
    }
}

/// AST Node for a type name.
///
/// This AST uses this reference instead of a raw `&str`.
/// slice whenever the AST refers to a concrete object type, input type, fragment
/// name, or operation name.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct NamedType<'a> {
    pub name: &'a str,
}

/// AST Node for a type reference.
///
/// [`VariableDefinitions`] must describe their type when they're defined, including whether they expect
/// lists, non-null values, or a type reference, which is a recursive type definition.
/// [Reference](https://spec.graphql.org/October2021/#sec-Type-References)
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Type<'a> {
    /// A reference to a named input type, which is a leaf node of a [Type].
    NamedType(NamedType<'a>),
    /// A list node wrapper for a Type, which indicates that a GraphQL API will always pass a list of the
    /// contained type in place.
    ListType(&'a Type<'a>),
    /// A non-null node wrapper for a Type, which indicates that a GraphQL API may not pass `null` instead
    /// of the conained type.
    NonNullType(&'a Type<'a>),
}

impl<'a> Type<'a> {
    /// Wraps this type in a list, indicating that it expects the current Type to be a list of
    /// itself instead.
    #[inline]
    pub fn into_list(self, ctx: &'a ASTContext) -> Type<'a> {
        Type::ListType(ctx.alloc(self))
    }

    /// A non-null node wrapper for a Type, indicating that a GraphQL API may not pass `null` instead
    /// of the conained type.
    #[inline]
    pub fn into_nonnull(self, ctx: &'a ASTContext) -> Type<'a> {
        Type::NonNullType(ctx.alloc(self))
    }

    /// Unwraps a Type recursively and returns the `NamedType` that is contained within its
    /// wrappers.
    #[inline]
    pub fn of_type(&'a self) -> &'a NamedType<'a> {
        match self {
            Type::NamedType(of_type) => of_type,
            Type::ListType(_) => self.of_type(),
            Type::NonNullType(_) => self.of_type(),
        }
    }
}

/// AST Node for a variable definition.
///
/// A variable definition defines multiple [Variable]
/// identifiers that can be used in place of any other non-static [Value] throughout the
/// document.
///
/// [Reference](https://spec.graphql.org/October2021/#VariableDefinition)
#[derive(Debug, PartialEq, Clone)]
pub struct VariableDefinition<'a> {
    /// The variable's name, as in, its identifier, which is prefixed with a `$` sign in the
    /// document.
    pub variable: Variable<'a>,
    /// Annotation of the type of a given variable, which ultimately leads to a type reference of
    /// an input type, as defined on a GraphQL schema.
    pub of_type: Type<'a>,
    /// A GraphQL variable may be replaced by a default value, when it's not passed or `null`
    /// is passed for a non-null variable. When this definition doesn't contain any default value
    /// this property is set to `Value::Null`.
    pub default_value: Value<'a>,
    /// Directives that are annotating this Variable Definition.
    ///
    /// When no Directives are present, this will be an empty
    /// list, as can be checked using `Directives::is_empty`.
    /// See: [Directives]
    pub directives: Directives<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDefinitions<'a> {
    pub children: bumpalo::collections::Vec<'a, VariableDefinition<'a>>,
}

impl<'a> VariableDefinitions<'a> {
    /// Checks whether the list of Variable Definitions is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.children.is_empty()
    }

    /// Returns a `Map` keyed by all variable names mapped to their definitions.
    pub fn as_map(
        &'a self,
        ctx: &'a ASTContext,
    ) -> HashMap<&str, &'a VariableDefinition<'a>, DefaultHashBuilder, &'a bumpalo::Bump> {
        let mut map = HashMap::new_in(&ctx.arena);
        for var_def in self.children.iter() {
            map.insert(var_def.variable.name, var_def);
        }
        map
    }
}

/// AST Node for a Fragment definition with an additional Selection Set.
///
/// This may only be applied when the type condition matches or when no type condition is present
/// and extends a Selection Set by being applied using a [`FragmentSpread`] selection.
/// [Reference](https://spec.graphql.org/October2021/#sec-Language.Fragments)
#[derive(Debug, PartialEq, Clone)]
pub struct FragmentDefinition<'a> {
    /// A given name of the Fragment Definition that is used by [FragmentSpread] selections to
    /// refer to this definition.
    pub name: NamedType<'a>,
    /// A given type condition's type name that must match before this fragment is applied on a
    /// GraphQL API. On inline fragments this is optional and no type condition has to be passed.
    pub type_condition: NamedType<'a>,
    /// Directives that are annotating this Fragment.
    ///
    /// When no Directives are present, this will be an empty
    /// list, as can be checked using `Directives::is_empty`.
    /// See: [Directives]
    pub directives: Directives<'a>,
    /// A sub-Selection Set that is applied when this Fragment is applied to the parent
    /// Selection Set.
    /// See: [SelectionSet]
    pub selection_set: SelectionSet<'a>,
}

/// A wrapper around the [`FragmentDefinition`] struct that also contains the index of the fragment
/// definition within the list of definitions in a given [`Document`].
#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) struct FragmentDefinitionWithIndex<'a> {
    pub fragment: &'a FragmentDefinition<'a>,
    pub index: usize,
}

/// AST Node for a kind of operation, as referred to by an [`OperationDefinition`].
///
/// In GraphQL there are three different operations, with each having a unique identifier on
/// Operation Definitions.
/// [Reference](https://spec.graphql.org/October2021/#sec-Language.Operations)
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum OperationKind {
    Query,
    Mutation,
    Subscription,
}

/// AST Node for an Operation Definition, which defines the entrypoint for GraphQL's execution.
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Language.Operations)
#[derive(Debug, PartialEq, Clone)]
pub struct OperationDefinition<'a> {
    /// The kind of operation that this definition specifies
    pub operation: OperationKind,
    // An optional name, as given to the operation definition.
    //
    // A [Document] may contain multiple
    // Operation Definitions from which a single one can be selected during execution. When a
    // Document contains only a single operation, it doesn't have to have a name.
    pub name: Option<NamedType<'a>>,
    /// A list of variables that the operation defines and accepts during execution.
    ///
    /// When an
    /// Operation Definition defines no variables this may be an empty list, as can be checked
    /// using `Directives::is_empty`.
    pub variable_definitions: VariableDefinitions<'a>,
    /// Directives that are annotating this Operation Definition.
    ///
    /// When no Directives are present, this will be an empty
    /// list, as can be checked using `Directives::is_empty`.
    /// See: [Directives]
    pub directives: Directives<'a>,
    /// A sub-Selection Set that is applied when this Operation Definition is executed to the root
    /// type of the specified kind of operation.
    /// See: [SelectionSet]
    pub selection_set: SelectionSet<'a>,
}

/// AST Root Node for a GraphQL query language document. This contains one or more definitions of
/// fragments or operations.
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Document)
#[derive(Debug, PartialEq, Clone)]
pub struct Document<'a> {
    pub definitions: bumpalo::collections::Vec<'a, Definition<'a>>,
    /// A hint on how large the source text was from which this Document was parsed.
    ///
    /// This gives an initial indication of the starting capacity of a `String` that will hold the stringified
    /// document.
    pub size_hint: usize,
}

impl<'a, 'b> Document<'a> {
    /// Checks whether this document contains any definitions.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.definitions.is_empty()
    }

    /// Returns a `Map` keyed by all fragment names mapped to their [`FragmentDefinition`] and the
    /// index of where they appear in the list of definitions of the given [`Document`].
    /// This is useful for manually traversing the document and resolving [`FragmentSpread`] nodes to
    /// their definitions.
    pub(crate) fn fragments_with_index(
        &'a self,
        ctx: &'a ASTContext,
    ) -> hashbrown::HashMap<&str, FragmentDefinitionWithIndex<'a>, DefaultHashBuilder, &bumpalo::Bump>
    {
        let mut map = hashbrown::HashMap::new_in(&ctx.arena);
        for (index, definition) in self.definitions.iter().enumerate() {
            if let Definition::Fragment(fragment) = definition {
                map.insert(
                    fragment.name.name,
                    FragmentDefinitionWithIndex { fragment, index },
                );
            }
        }
        map
    }

    /// Returns a `Map` keyed by all fragment names mapped to their fragment definitions.
    /// This is useful for manually traversing the document and resolving [`FragmentSpread`] nodes to
    /// their definitions.
    pub fn fragments(
        &'a self,
        ctx: &'a ASTContext,
    ) -> HashMap<&str, &'a FragmentDefinition<'a>, DefaultHashBuilder, &'a bumpalo::Bump> {
        let mut map = HashMap::new_in(&ctx.arena);
        for definition in self.definitions.iter() {
            if let Definition::Fragment(fragment) = definition {
                map.insert(fragment.name.name, fragment);
            }
        }
        map
    }

    pub(crate) fn operation_with_index(
        &'a self,
        by_name: Option<&'b str>,
    ) -> Result<(&'a OperationDefinition<'a>, usize)> {
        if let Some(by_name) = by_name {
            self.definitions
                .iter()
                .enumerate()
                .find_map(|(index, definition)| match definition {
                    Definition::Operation(
                        operation @ OperationDefinition {
                            name: Some(NamedType { name }),
                            ..
                        },
                    ) if *name == by_name => Some((operation, index)),
                    _ => None,
                })
                .ok_or(Error::new(
                    format!("Operation with name {by_name} does not exist"),
                    Some(ErrorType::GraphQL),
                ))
        } else {
            let operations = self
                .definitions
                .iter()
                .enumerate()
                .filter_map(|(index, definition)| {
                    definition.operation().map(|operation| (operation, index))
                })
                .collect::<std::vec::Vec<(&'a OperationDefinition, usize)>>();
            match operations.len() {
                0 => Err(Error::new(
                    "Document does not contain any operations",
                    Some(ErrorType::GraphQL),
                )),
                1 => Ok(operations[0]),
                _ => Err(Error::new(
                    "Document contains more than one operation, missing operation name",
                    Some(ErrorType::GraphQL),
                )),
            }
        }
    }

    /// Finds an operation definition by name or the single operation contained in the document
    /// when `None` is passed.
    ///
    /// [Reference](https://spec.graphql.org/October2021/#GetOperation())
    pub fn operation(&'a self, by_name: Option<&'b str>) -> Result<&'a OperationDefinition<'a>> {
        Ok(self.operation_with_index(by_name)?.0)
    }
}

/// AST Node for a Definition inside a query language document, which may either be an Operation
/// Definition or a Fragment Definition.
///
/// [Reference](https://spec.graphql.org/October2021/#sec-Document)
#[derive(Debug, PartialEq, Clone)]
pub enum Definition<'a> {
    Operation(OperationDefinition<'a>),
    Fragment(FragmentDefinition<'a>),
}

impl<'a> Definition<'a> {
    /// Helper method to return the [`OperationDefinition`] if the Definition is an `OperationDefinition`.
    #[inline]
    pub fn operation(&'a self) -> Option<&'a OperationDefinition<'a>> {
        match self {
            Definition::Operation(operation) => Some(operation),
            Definition::Fragment(_) => None,
        }
    }

    /// Helper method to return the [`FragmentDefinition`] if the Definition is a `FragmentDefinition`.
    #[inline]
    pub fn fragment(&'a self) -> Option<&'a FragmentDefinition<'a>> {
        match self {
            Definition::Fragment(fragment) => Some(fragment),
            Definition::Operation(_) => None,
        }
    }
}

/// Trait implemented by all ast nodes that can have directives attached.
pub trait WithDirectives<'arena> {
    fn directives(&self) -> &Directives<'arena>;
}

macro_rules! with_directives {
    ($($for_type:ident),+) => {
        $(
            impl<'arena> WithDirectives<'arena> for $for_type<'arena> {
                #[inline]
                fn directives(&self) -> &Directives<'arena> {
                    &self.directives
                }
            }
        )+
    };
}

with_directives!(
    Field,
    FragmentSpread,
    InlineFragment,
    OperationDefinition,
    FragmentDefinition,
    VariableDefinition
);

impl<'arena> WithDirectives<'arena> for Selection<'arena> {
    /// Helper method to get all Directives for a given selection directly.
    ///
    /// Any selection AST node may carry Directives, so when those are checked
    /// it's unnecessary to first match the type of selection.
    fn directives(&self) -> &Directives<'arena> {
        match self {
            Selection::Field(field) => &field.directives,
            Selection::FragmentSpread(spread) => &spread.directives,
            Selection::InlineFragment(fragment) => &fragment.directives,
        }
    }
}

impl<'arena> WithDirectives<'arena> for Definition<'arena> {
    /// Helper method to get all Directives for a given definition directly.
    ///
    /// Any Definition AST node may carry Directives, so when those are checked
    /// it's unnecessary to first match the type of Definition.
    #[inline]
    fn directives(&self) -> &Directives<'arena> {
        match self {
            Definition::Operation(operation) => &operation.directives,
            Definition::Fragment(fragment) => &fragment.directives,
        }
    }
}

/// Trait implemented by all AST nodes that can be skipped via standard skip/include directives.
pub trait Skippable<'arena>: WithDirectives<'arena> {
    /// Resolves @include and @skip directives to a flag on whether an AST node must be
    /// skipped during execution.
    ///
    /// [Reference](https://spec.graphql.org/October2021/#sec--skip)
    #[inline]
    fn should_include(&'arena self, variables: Option<&Variables<'arena>>) -> bool {
        for directive in self.directives().children.iter() {
            if directive.name != "skip" && directive.name != "include" {
                continue;
            }

            let if_arg = directive
                .arguments
                .children
                .iter()
                .find(|arg| arg.name == "if")
                .map(|arg| arg.value.is_truthy(variables));

            if let Some(if_arg) = if_arg {
                return (directive.name == "include") == if_arg;
            }
        }

        true
    }
}

impl<'arena> Skippable<'arena> for Selection<'arena> {}
impl<'arena> Skippable<'arena> for Field<'arena> {}
impl<'arena> Skippable<'arena> for InlineFragment<'arena> {}
impl<'arena> Skippable<'arena> for FragmentSpread<'arena> {}

#[cfg(test)]
mod tests {
    use super::{ASTContext, Document};
    use crate::ast::{ParseNode, PrintNode};

    #[test]
    fn operation_no_operations() {
        let ctx = ASTContext::new();
        let ast = Document::parse(&ctx, r#"fragment Foo on Query { hello }"#).unwrap();
        assert_eq!(
            ast.operation(Some("queryName")).unwrap_err().message,
            "Operation with name queryName does not exist"
        );
        assert_eq!(
            ast.operation(None).unwrap_err().message,
            "Document does not contain any operations"
        );
    }

    #[test]
    fn operation_one_operation() {
        let ctx = ASTContext::new();
        let ast = Document::parse(&ctx, r#"query queryName { hello }"#).unwrap();
        assert_eq!(
            ast.operation(Some("queryName")).unwrap().print(),
            "query queryName {\n  hello\n}"
        );
        assert_eq!(
            ast.operation(None).unwrap().print(),
            "query queryName {\n  hello\n}"
        );
    }

    #[test]
    fn operation_one_operation_anonymous() {
        let ctx = ASTContext::new();
        let ast = Document::parse(&ctx, r#"{ hello }"#).unwrap();
        assert_eq!(
            ast.operation(Some("queryName")).unwrap_err().message,
            "Operation with name queryName does not exist"
        );
        assert_eq!(ast.operation(None).unwrap().print(), "{\n  hello\n}");
    }

    #[test]
    fn operation_two_operations() {
        let ctx = ASTContext::new();
        let ast = Document::parse(
            &ctx,
            r#"query queryName { hello } query otherName { world }"#,
        )
        .unwrap();
        assert_eq!(
            ast.operation(Some("queryName")).unwrap().print(),
            "query queryName {\n  hello\n}"
        );
        assert_eq!(
            ast.operation(Some("otherName")).unwrap().print(),
            "query otherName {\n  world\n}"
        );
        assert_eq!(
            ast.operation(Some("badName")).unwrap_err().message,
            "Operation with name badName does not exist"
        );
        assert_eq!(
            ast.operation(None).unwrap_err().message,
            "Document contains more than one operation, missing operation name"
        );
    }

    #[test]
    fn operation_two_operations_one_anonymous() {
        let ctx = ASTContext::new();
        let ast = Document::parse(&ctx, r#"{ hello } query otherName { world }"#).unwrap();
        assert_eq!(
            ast.operation(Some("queryName")).unwrap_err().message,
            "Operation with name queryName does not exist"
        );
        assert_eq!(
            ast.operation(Some("otherName")).unwrap().print(),
            "query otherName {\n  world\n}"
        );
        assert_eq!(
            ast.operation(None).unwrap_err().message,
            "Document contains more than one operation, missing operation name"
        );
    }
}
