use bumpalo::collections::Vec;
use bumpalo::Bump;

use crate::error::Error;
use crate::{ast::ASTContext, error::ErrorType};

/// The `ValidationContext` carrying a reference to the AST Context's arena and a list of errors.
///
/// New errors are added to the list as validation continues to issue error messages if validation
/// has failed on a document.
pub struct ValidationContext<'a> {
    pub arena: &'a Bump,
    pub errors: Vec<'a, &'a str>,
}

impl<'a> ValidationContext<'a> {
    /// Create a new `ValidationContext` given an AST Context.
    pub fn new(ctx: &'a ASTContext) -> Self {
        ValidationContext {
            arena: &ctx.arena,
            errors: Vec::new_in(&ctx.arena),
        }
    }

    /// Add an error to the list of errors in the `ValidationContext`.
    ///
    /// This is called inside of validation rules to add an error to the list and fail validation
    /// of the currently validating document.
    pub fn add_error<S: AsRef<str>>(&mut self, str: S) {
        self.errors.push(self.arena.alloc_str(str.as_ref()));
    }

    /// Convert the context into a result which carries an error if validation has failed on the
    /// current document.
    ///
    /// This is used by `ValidateNode` and the `ValidationRule`'s `validate` method to convert the
    /// context into a result.
    pub fn to_result(self) -> Result<(), Error> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            let mut context = String::new();
            let mut is_first = true;
            for error in self.errors {
                if is_first {
                    is_first = false;
                } else {
                    context.push('\n');
                }
                context.push_str("- ");
                context.push_str(error.as_ref());
            }
            Err(Error::new_with_context(
                "Document failed validation",
                None,
                &context,
                Some(ErrorType::GraphQL),
            ))
        }
    }
}
