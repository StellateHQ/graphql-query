use crate::error::Error;
use std::fmt;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

impl Path {
    pub fn push(&mut self, segment: PathSegment) {
        self.segments.push(segment)
    }

    pub fn pop(&mut self) -> Option<PathSegment> {
        self.segments.pop()
    }
}

impl TryFrom<&str> for Path {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let segments = value
            .split('.')
            .map(PathSegment::try_from)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self { segments })
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.segments
                .iter()
                .map(|segment| segment.to_string())
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PathSegment {
    Index(usize),

    Arguments,
    Directives,
    Name,
    SelectionSet,
    Type,
    Value,
    Variable,
    VariableDefinitions,
}

impl TryFrom<&str> for PathSegment {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value.parse::<usize>() {
            Ok(index) => Ok(Self::Index(index)),
            Err(_) => match value {
                "arguments" => Ok(PathSegment::Arguments),
                "directives" => Ok(PathSegment::Directives),
                "name" => Ok(PathSegment::Name),
                "selectionSet" => Ok(PathSegment::SelectionSet),
                "type" => Ok(PathSegment::Type),
                "value" => Ok(PathSegment::Value),
                "variable" => Ok(PathSegment::Variable),
                "variableDefinitions" => Ok(PathSegment::VariableDefinitions),
                _ => Err(Error::new(format!("Invalid path segment {value}"), None)),
            },
        }
    }
}

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PathSegment::Index(index) => index.to_string(),
                PathSegment::Arguments => String::from("arguments"),
                PathSegment::Directives => String::from("directives"),
                PathSegment::Name => String::from("name"),
                PathSegment::SelectionSet => String::from("selectionSet"),
                PathSegment::Type => String::from("type"),
                PathSegment::Value => String::from("value"),
                PathSegment::Variable => String::from("variable"),
                PathSegment::VariableDefinitions => String::from("variableDefinitions"),
            }
        )
    }
}
