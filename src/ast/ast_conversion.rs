// #![allow(clippy::needless_update)]
use super::ast::*;
use bumpalo::collections::{vec::IntoIter, Vec};

// TODO: from_iter_in could be a good helper here as well

impl<'a> IntoIterator for ListValue<'a> {
    type Item = Value<'a>;
    type IntoIter = IntoIter<'a, Value<'a>>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.children.into_iter()
    }
}

impl<'a> IntoIterator for ObjectValue<'a> {
    type Item = ObjectField<'a>;
    type IntoIter = IntoIter<'a, ObjectField<'a>>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.children.into_iter()
    }
}

impl<'a> IntoIterator for Arguments<'a> {
    type Item = Argument<'a>;
    type IntoIter = IntoIter<'a, Argument<'a>>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.children.into_iter()
    }
}

impl<'a> IntoIterator for Directives<'a> {
    type Item = Directive<'a>;
    type IntoIter = IntoIter<'a, Directive<'a>>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.children.into_iter()
    }
}

impl<'a> IntoIterator for VariableDefinitions<'a> {
    type Item = VariableDefinition<'a>;
    type IntoIter = IntoIter<'a, VariableDefinition<'a>>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.children.into_iter()
    }
}

impl<'a> IntoIterator for SelectionSet<'a> {
    type Item = Selection<'a>;
    type IntoIter = IntoIter<'a, Selection<'a>>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.selections.into_iter()
    }
}

pub trait DefaultIn<'a> {
    fn default_in(arena: &'a bumpalo::Bump) -> Self;
}

impl<'a, T> DefaultIn<'a> for T
where
    T: Default,
{
    fn default_in(_ctx: &'a bumpalo::Bump) -> Self {
        Self::default()
    }
}

impl<'a> DefaultIn<'a> for Document<'a> {
    fn default_in(arena: &'a bumpalo::Bump) -> Self {
        Document {
            definitions: Vec::new_in(&arena),
            size_hint: 0,
        }
    }
}

impl<'a> DefaultIn<'a> for VariableDefinitions<'a> {
    fn default_in(arena: &'a bumpalo::Bump) -> Self {
        VariableDefinitions {
            children: Vec::new_in(&arena),
        }
    }
}

impl<'a> DefaultIn<'a> for ObjectValue<'a> {
    fn default_in(arena: &'a bumpalo::Bump) -> Self {
        ObjectValue {
            children: Vec::new_in(&arena),
        }
    }
}

impl<'a> DefaultIn<'a> for ListValue<'a> {
    fn default_in(arena: &'a bumpalo::Bump) -> Self {
        ListValue {
            children: Vec::new_in(&arena),
        }
    }
}

impl<'a> DefaultIn<'a> for Arguments<'a> {
    fn default_in(arena: &'a bumpalo::Bump) -> Self {
        Arguments {
            children: Vec::new_in(&arena),
        }
    }
}

impl<'a> DefaultIn<'a> for Directives<'a> {
    fn default_in(arena: &'a bumpalo::Bump) -> Self {
        Directives {
            children: Vec::new_in(&arena),
        }
    }
}

impl<'a> DefaultIn<'a> for SelectionSet<'a> {
    fn default_in(arena: &'a bumpalo::Bump) -> Self {
        SelectionSet {
            selections: Vec::new_in(&arena),
        }
    }
}

impl<'a> From<&'a str> for NamedType<'a> {
    #[inline]
    fn from(name: &'a str) -> Self {
        NamedType { name }
    }
}

impl<'a> From<&'a str> for Variable<'a> {
    #[inline]
    fn from(name: &'a str) -> Variable<'a> {
        Variable { name }
    }
}

impl From<bool> for BooleanValue {
    #[inline]
    fn from(value: bool) -> Self {
        BooleanValue { value }
    }
}

impl<'a> From<&'a str> for StringValue<'a> {
    #[inline]
    fn from(value: &'a str) -> Self {
        StringValue { value }
    }
}

impl<'a> From<Variable<'a>> for Value<'a> {
    #[inline]
    fn from(x: Variable<'a>) -> Self {
        Value::Variable(x)
    }
}

impl<'a> From<StringValue<'a>> for Value<'a> {
    #[inline]
    fn from(x: StringValue<'a>) -> Self {
        Value::String(x)
    }
}

impl<'a> From<FloatValue<'a>> for Value<'a> {
    #[inline]
    fn from(x: FloatValue<'a>) -> Self {
        Value::Float(x)
    }
}

impl<'a> From<IntValue<'a>> for Value<'a> {
    #[inline]
    fn from(x: IntValue<'a>) -> Self {
        Value::Int(x)
    }
}

impl<'a> From<BooleanValue> for Value<'a> {
    #[inline]
    fn from(x: BooleanValue) -> Self {
        Value::Boolean(x)
    }
}

impl<'a> From<EnumValue<'a>> for Value<'a> {
    #[inline]
    fn from(x: EnumValue<'a>) -> Self {
        Value::Enum(x)
    }
}

impl<'a> From<ListValue<'a>> for Value<'a> {
    #[inline]
    fn from(x: ListValue<'a>) -> Self {
        Value::List(x)
    }
}

impl<'a> From<ObjectValue<'a>> for Value<'a> {
    #[inline]
    fn from(x: ObjectValue<'a>) -> Self {
        Value::Object(x)
    }
}

impl<'a> From<NamedType<'a>> for Type<'a> {
    #[inline]
    fn from(x: NamedType<'a>) -> Self {
        Type::NamedType(x)
    }
}

impl<'a> From<Field<'a>> for Selection<'a> {
    #[inline]
    fn from(x: Field<'a>) -> Self {
        Selection::Field(x)
    }
}

impl<'a> From<FragmentSpread<'a>> for Selection<'a> {
    #[inline]
    fn from(x: FragmentSpread<'a>) -> Self {
        Selection::FragmentSpread(x)
    }
}

impl<'a> From<InlineFragment<'a>> for Selection<'a> {
    #[inline]
    fn from(x: InlineFragment<'a>) -> Self {
        Selection::InlineFragment(x)
    }
}

impl<'a> From<OperationDefinition<'a>> for Definition<'a> {
    #[inline]
    fn from(x: OperationDefinition<'a>) -> Self {
        Definition::Operation(x)
    }
}

impl<'a> From<FragmentDefinition<'a>> for Definition<'a> {
    #[inline]
    fn from(x: FragmentDefinition<'a>) -> Self {
        Definition::Fragment(x)
    }
}
