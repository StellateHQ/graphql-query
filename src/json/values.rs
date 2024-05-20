use bumpalo::collections::Vec;
use hashbrown::HashMap;
use serde_json::{json, map::Map as JSMap, Value as JSValue};

use super::ValueFromNode;
use crate::ast::*;
use crate::error::{Error, Result};

/// Convert [serde_json::Value] to [Variables] given [VariableDefinitions].
///
/// This may be used to accept JSON values as variables for a given operation definition, which
/// contains the necessary types to cast JSON values to variables.
pub fn ast_variables_from_value<'a>(
    ctx: &'a ASTContext,
    input: &JSValue,
    var_defs: &'a VariableDefinitions<'a>,
) -> Result<Variables<'a>> {
    let mut vars = HashMap::new_in(&ctx.arena);
    if var_defs.is_empty() {
        Ok(vars)
    } else if let JSValue::Object(obj) = input {
        for var_def in var_defs.children.iter() {
            let value = match obj.get(var_def.variable.name) {
                Some(value) => ctx.alloc(ast_from_value(ctx, value, ctx.alloc(var_def.of_type))?),
                None => match (var_def.default_value.clone(), var_def.of_type) {
                    (Value::List(_), Type::ListType(_)) => &var_def.default_value,
                    (Value::Null, Type::ListType(_)) => &var_def.default_value,
                    (default_value, Type::ListType(_)) => {
                        let mut builder: Vec<'_, _> = Vec::new_in(&ctx.arena);
                        builder.push(default_value);
                        ctx.alloc(Value::List(ListValue { children: builder }))
                    }
                    _ => &var_def.default_value,
                },
            };
            vars.insert(var_def.variable.name, value.to_owned());
        }
        Ok(vars)
    } else {
        Err(Error::new(
            "Variables expected but received non-object value",
            None,
        ))
    }
}

/// Convert [serde_json::Value] to an AST Value Node given a [Type] definition.
pub fn ast_from_value<'a>(
    ctx: &'a ASTContext,
    value: &JSValue,
    of_type: &'a Type<'a>,
) -> Result<Value<'a>> {
    match (of_type, value) {
        (Type::ListType(of_type), JSValue::Array(list)) => {
            let new_list = list.iter().map(|value| ast_from_value(ctx, value, of_type));

            let mut new_list_children = Vec::new_in(&ctx.arena);
            for item in new_list {
                new_list_children.push(item?);
            }

            return Ok(Value::List(ListValue {
                children: new_list_children,
            }));
        }
        (Type::ListType(of_type), value) => {
            if matches!(value, JSValue::Null) {
                return Ok(Value::Null);
            }

            let child = ast_from_value(ctx, value, of_type)?;
            let mut new_list_children = Vec::new_in(&ctx.arena);
            new_list_children.push(child);
            return Ok(Value::List(ListValue {
                children: new_list_children,
            }));
        }

        (Type::NonNullType(_), JSValue::Null) => {
            Err(Error::new("Received null for non-nullable type", None))
        }

        (_, JSValue::Null) => Ok(Value::Null),

        (Type::NonNullType(of_type), value) => ast_from_value(ctx, value, of_type),

        (Type::NamedType(NamedType { name: "Boolean" }), JSValue::Bool(x)) => {
            Ok(Value::Boolean((*x).into()))
        }

        (Type::NamedType(NamedType { name: "Boolean" }), JSValue::Number(num)) => {
            Ok(Value::Boolean((num.as_u64().unwrap_or(0) != 0).into()))
        }

        (Type::NamedType(NamedType { name: "Int" }), JSValue::Number(num)) => num
            .as_i64()
            .map(|x| {
                Value::Int(IntValue {
                    value: ctx.alloc_str(&x.to_string()),
                })
            })
            .ok_or_else(|| Error::new("Received Float for Int type", None)),

        (Type::NamedType(NamedType { name: "Float" }), JSValue::Number(num)) => {
            let num = num.as_f64().unwrap_or(0.0);
            if num.is_finite() {
                Ok(Value::Float(FloatValue {
                    value: ctx.alloc_str(&num.to_string()),
                }))
            } else {
                Err(Error::new("Received non-finite Float for Float type", None))
            }
        }

        (
            Type::NamedType(NamedType {
                name: "ID" | "String",
            }),
            JSValue::String(str),
        ) => Ok(Value::String(ctx.alloc_str(str).into())),

        (
            Type::NamedType(NamedType {
                name: "ID" | "String",
            }),
            JSValue::Number(num),
        ) => Ok(Value::String(ctx.alloc_string(num.to_string()).into())),

        (Type::NamedType(NamedType { name: _ }), value) => Ok(ast_from_value_untyped(ctx, value)),
    }
}

/// Convert [serde_json::Value] to an AST Value Node without casting the JSON value to a type.
pub fn ast_from_value_untyped<'a>(ctx: &'a ASTContext, value: &JSValue) -> Value<'a> {
    match value {
        JSValue::Array(list) => {
            let new_list = list.iter().map(|value| ast_from_value_untyped(ctx, value));

            let mut new_list_children = Vec::new_in(&ctx.arena);
            for item in new_list {
                new_list_children.push(item);
            }

            return Value::List(ListValue {
                children: new_list_children,
            });
        }
        JSValue::Object(map) => {
            let new_object_children_iter = map.iter().map(|(key, value)| ObjectField {
                name: ctx.alloc_str(key),
                value: ast_from_value_untyped(ctx, value),
            });
            let mut new_object_children = Vec::new_in(&ctx.arena);
            for item in new_object_children_iter {
                new_object_children.push(item);
            }
            return Value::Object(ObjectValue {
                children: new_object_children,
            });
        }
        JSValue::Number(num) => num
            .as_i64()
            .map(|x| {
                Value::Int(IntValue {
                    value: ctx.alloc_str(&x.to_string()),
                })
            })
            .unwrap_or_else(|| {
                let float = num.as_f64().filter(|x| x.is_finite()).unwrap_or(0.0);
                Value::Float(FloatValue {
                    value: ctx.alloc_str(&float.to_string()),
                })
            }),
        JSValue::Bool(x) => Value::Boolean((*x).into()),
        JSValue::String(str) => Value::String(ctx.alloc_str(str).into()),
        JSValue::Null => Value::Null,
    }
}

/// Convert [Variables] back to a [serde_json::Value].
pub fn value_from_ast_variables<'a>(variable: &'a Variables<'a>) -> JSMap<String, JSValue> {
    let mut map = JSMap::new();
    for (key, value) in variable.iter() {
        map.insert(key.to_string(), value.clone().to_json(None));
    }
    map
}

/// Convert AST Value Node back to a [serde_json::Value] given a [Type] definition.
pub fn value_from_ast<'a>(
    value: &Value<'a>,
    of_type: &'a Type<'a>,
    variables: Option<&'a Variables<'a>>,
) -> Result<JSValue> {
    match (of_type, value) {
        (of_type, Value::Variable(var)) => variables
            .and_then(|vars| vars.get(var.name))
            .ok_or_else(|| Error::new("Invalid variable reference when casting to value", None))
            .and_then(|value| value_from_ast(value, of_type, None)),

        (Type::ListType(of_type), Value::List(list)) => {
            let new_list_children_iter = list
                .children
                .iter()
                .map(|value| value_from_ast(value, of_type, variables));
            let mut new_children = vec![];
            for item in new_list_children_iter {
                new_children.push(item?);
            }
            Ok(JSValue::Array(new_children))
        }

        (Type::ListType(of_type), value) => {
            let child = value_from_ast(value, of_type, variables)?;
            Ok(JSValue::Array(vec![child]))
        }

        (Type::NonNullType(_), Value::Null) => {
            Err(Error::new("Received null for non-nullable type", None))
        }

        (_, Value::Null) => Ok(JSValue::Null),

        (Type::NonNullType(of_type), value) => value_from_ast(value, of_type, variables),

        (Type::NamedType(NamedType { name: "Boolean" }), Value::Boolean(x)) => {
            Ok(JSValue::Bool(x.value))
        }
        (Type::NamedType(NamedType { name: "Boolean" }), Value::Int(x)) => {
            let res = x.value.parse::<i32>();
            match res {
                Ok(int) => Ok(JSValue::Bool(int != 0)),
                Err(_) => Err(Error::new(
                    format!("Got invalid Int {} expected Boolean type.", x.value),
                    None,
                )),
            }
        }

        (Type::NamedType(NamedType { name: "Int" }), Value::Int(x)) => {
            let res = x.value.parse::<i32>();
            match res {
                Ok(int) => Ok(JSValue::Number(int.into())),
                Err(_) => Err(Error::new(format!("Got invalid Int {}.", x.value), None)),
            }
        }
        (Type::NamedType(NamedType { name: "Float" }), Value::Float(x)) => Ok(json!(x.value)),

        (
            Type::NamedType(NamedType {
                name: "ID" | "String",
            }),
            Value::Int(num),
        ) => Ok(JSValue::String(num.value.to_string())),

        (
            Type::NamedType(NamedType {
                name: "ID" | "String",
            }),
            Value::String(str),
        ) => Ok(JSValue::String(str.value.into())),

        (Type::NamedType(NamedType { name: _ }), value) => Ok(value.to_owned().to_json(variables)),
    }
}

#[cfg(test)]
mod tests {
    use super::{ast_variables_from_value, DefaultIn};
    use crate::ast::{
        ASTContext, Directives, NamedType, Type, Value, Variable, VariableDefinition,
        VariableDefinitions,
    };
    use bumpalo::collections::Vec;
    use serde_json::{json, Value as JsValue};

    #[test]
    fn nullable_list() {
        let ctx = ASTContext::new();
        let input = json!({
            "list": JsValue::Null,
        });

        let var = vec![VariableDefinition {
            variable: Variable { name: "list" },
            of_type: Type::ListType(&Type::NonNullType(&Type::NamedType(NamedType {
                name: "Int",
            }))),
            default_value: Value::Null,
            directives: Directives::default_in(&ctx.arena)
        }];

        let var_defs = VariableDefinitions {
            children: Vec::from_iter_in(var, &ctx.arena),
        };
        let _ = ast_variables_from_value(&ctx, &input, &var_defs).unwrap();
    }

    #[test]
    fn object_list() {
        let ctx = ASTContext::new();
        let input = json!({
            "orderBys": [{
                "equals": {
                    "value": 5
                }
            }],
        });

        let var = vec![VariableDefinition {
            variable: Variable { name: "orderBys" },
            of_type: Type::ListType(&Type::NamedType(NamedType {
                name: "orderByInput",
            })),
            default_value: Value::Null,
            directives: Directives::default_in(&ctx.arena)
        }];

        let var_defs = VariableDefinitions {
            children: Vec::from_iter_in(var, &ctx.arena),
        };
        let _ = ast_variables_from_value(&ctx, &input, &var_defs).unwrap();
    }
}
