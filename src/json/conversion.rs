use crate::ast::*;
use serde_json::{Map as JSMap, Value as JSValue};

/// Trait for convertin AST Value Nodes of a GraphQL language document to [serde_json::Value]s.
pub trait ValueFromNode<'a>: Sized {
    /// Convert current AST Value Node to a [serde_json::Value] with given [Variables].
    fn to_json(self, variables: Option<&Variables<'a>>) -> JSValue;
}

impl<'a> ValueFromNode<'a> for Value<'a> {
    #[inline]
    fn to_json(self, variables: Option<&Variables<'a>>) -> JSValue {
        match self {
            Value::Variable(var) => var.to_json(variables),
            Value::List(list) => list.to_json(variables),
            Value::Object(obj) => obj.to_json(variables),
            Value::Int(node) => node.to_json(variables),
            Value::Float(node) => node.to_json(variables),
            Value::Boolean(node) => node.to_json(variables),
            Value::String(node) => node.to_json(variables),
            Value::Enum(node) => node.to_json(variables),
            Value::Null => JSValue::Null,
        }
    }
}

impl<'a> ValueFromNode<'a> for IntValue<'a> {
    /// Convert current IntValue Node to a [serde_json::Value].
    #[inline]
    fn to_json(self, _variables: Option<&Variables<'a>>) -> JSValue {
        let int = self.value.parse::<i32>();
        match int {
            Ok(value) => value.into(),
            Err(_) => self.value.into(),
        }
    }
}

impl<'a> ValueFromNode<'a> for FloatValue<'a> {
    /// Convert current FloatValue Node to a [serde_json::Value].
    #[inline]
    fn to_json(self, _variables: Option<&Variables<'a>>) -> JSValue {
        let int = self.value.parse::<f64>();
        match int {
            Ok(value) => value.into(),
            Err(_) => self.value.into(),
        }
    }
}

impl<'a> ValueFromNode<'a> for BooleanValue {
    /// Convert current BooleanValue Node to a [serde_json::Value].
    #[inline]
    fn to_json(self, _variables: Option<&Variables<'a>>) -> JSValue {
        self.value.into()
    }
}

impl<'a> ValueFromNode<'a> for StringValue<'a> {
    /// Convert current StringValue Node to a [serde_json::Value].
    #[inline]
    fn to_json(self, _variables: Option<&Variables<'a>>) -> JSValue {
        self.value.into()
    }
}

impl<'a> ValueFromNode<'a> for EnumValue<'a> {
    /// Convert current EnumValue Node to a [serde_json::Value].
    #[inline]
    fn to_json(self, _variables: Option<&Variables<'a>>) -> JSValue {
        self.value.into()
    }
}

impl<'a> ValueFromNode<'a> for Variable<'a> {
    /// Convert current Variable Node to a [serde_json::Value] with given [Variables].
    #[inline]
    fn to_json(self, variables: Option<&Variables<'a>>) -> JSValue {
        variables
            .and_then(|vars| vars.get(self.name))
            .map(|value| value.clone().to_json(None))
            .unwrap_or(JSValue::Null)
    }
}

impl<'a> ValueFromNode<'a> for ListValue<'a> {
    /// Convert current ListValue Node to a [serde_json::Value] with given [Variables].
    #[inline]
    fn to_json(self, variables: Option<&Variables<'a>>) -> JSValue {
        self.into_iter()
            .map(|value| value.to_json(variables))
            .collect::<Vec<JSValue>>()
            .into()
    }
}

impl<'a> ValueFromNode<'a> for ObjectValue<'a> {
    /// Convert current ObjectValue Node to a [serde_json::Value] with given [Variables].
    #[inline]
    fn to_json(self, variables: Option<&Variables<'a>>) -> JSValue {
        self.into_iter()
            .map(|field| (field.name.to_string(), field.value.to_json(variables)))
            .collect::<JSMap<String, JSValue>>()
            .into()
    }
}

/// Convert AST Value Node to a [serde_json::Value] with given [Variables].
pub fn value_from_ast_untyped<'a>(value: Value<'a>, variables: Option<&Variables<'a>>) -> JSValue {
    value.to_json(variables)
}
