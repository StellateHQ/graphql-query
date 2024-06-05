use logos::{internal::LexerInternal, Lexer, Logos};

#[derive(Clone)]
pub struct Extras<'a> {
    pub arena: &'a bumpalo::Bump,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = Extras<'s>)]
pub enum Token<'a> {
    #[token("[")]
    BracketOpen,

    #[token("]")]
    BracketClose,

    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClose,

    #[token("(")]
    ParenOpen,

    #[token(")")]
    ParenClose,

    #[token(":")]
    Colon,

    #[token("=")]
    Equal,

    #[token("!")]
    Exclam,

    #[token("&")]
    Ampersand,

    #[token("|")]
    Pipe,

    #[token(",")]
    Comma,

    #[regex(r"@[_a-zA-Z][_0-9a-zA-Z]*", |lex| &lex.slice()[1..])]
    DirectiveName(&'a str),

    #[regex(r"[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice())]
    Name(&'a str),

    #[regex(r"-?([1-9][0-9]*|0)[.][0-9]+([eE][+-]?[0-9]+)?")]
    #[regex("-?([1-9][0-9]*|0)[eE][+-]?[0-9]+")]
    Float(&'a str),

    #[regex(r"-?([1-9][0-9]*|0)")]
    Integer(&'a str),

    #[regex(r#"""?"?"#, parse_string)]
    String(&'a str),

    #[error]
    #[regex(r"([ ,\t\n\r\f]+|#[^\n\r]*)+", logos::skip)]
    Error,

    /// Token indicates the end of the input
    End,
}

#[derive(Logos, Debug, PartialEq)]
pub(crate) enum BlockPart {
    #[regex(r#"[^"\\\r\n]+"#)]
    #[regex(r#""+"#)]
    Text,

    #[regex(r"(\r|\n|\r\n)[\t ]*")]
    Newline,

    #[regex(r#"\\""""#)]
    EscapedEndBlock,

    #[regex(r#"\\."#)]
    EscapedSequence,

    #[token(r#"""""#)]
    EndBlock,

    #[error]
    Error,
}

#[derive(Logos, Debug, PartialEq)]
pub(crate) enum StringPart {
    #[regex(r#"[^\n\r\\"]+"#)]
    Text,

    #[regex(r"\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]")]
    EscapedCodepoint,

    #[token(r#"\""#)]
    EscapedQuote,
    #[token(r#"\\"#)]
    EscapedBackslash,
    #[token(r#"\/"#)]
    EscapedSlash,
    #[token(r#"\b"#)]
    EscapedBackspace,
    #[token(r#"\f"#)]
    EscapedLinefeed,
    #[token(r#"\n"#)]
    EscapedNewline,
    #[token(r#"\r"#)]
    EscapedReturn,
    #[token(r#"\t"#)]
    EscapedTab,

    #[token("\"")]
    EndString,

    #[error]
    Error,
}

#[inline]
fn lex_string<'a>(lex: &mut Lexer<'a, Token<'a>>, mut output: String) -> Option<&'a str> {
    let mut sublex = StringPart::lexer(lex.remainder());
    while let Some(token) = sublex.next() {
        match token {
            StringPart::Error => break,
            StringPart::Text => output.push_str(sublex.slice()),
            StringPart::EscapedQuote => output.push('"'),
            StringPart::EscapedBackslash => output.push('\\'),
            StringPart::EscapedSlash => output.push('/'),
            StringPart::EscapedBackspace => output.push(8 as char),
            StringPart::EscapedLinefeed => output.push(12 as char),
            StringPart::EscapedNewline => output.push('\n'),
            StringPart::EscapedReturn => output.push('\r'),
            StringPart::EscapedTab => output.push('\t'),
            StringPart::EscapedCodepoint => {
                use lexical_core::*;
                const FORMAT: u128 = NumberFormatBuilder::hexadecimal();
                const OPTIONS: ParseIntegerOptions = ParseIntegerOptions::new();
                output.push(
                    parse_with_options::<_, FORMAT>(sublex.slice()[2..].as_bytes(), &OPTIONS)
                        .ok()
                        .and_then(std::char::from_u32)?,
                );
            }
            StringPart::EndString => {
                lex.bump_unchecked(sublex.span().end);
                return Some(lex.extras.arena.alloc(output));
            }
        }
    }
    None
}

#[inline]
fn lex_block_string<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
    let mut output = String::new();
    let mut sublex = BlockPart::lexer(lex.remainder());
    let mut min_indent: usize = usize::MAX;
    while let Some(token) = sublex.next() {
        match token {
            BlockPart::EscapedSequence | BlockPart::Text => output.push_str(sublex.slice()),
            BlockPart::EscapedEndBlock => output.push_str("\"\"\""),
            BlockPart::Newline => {
                let mut slice = &sublex.slice()[1..];
                if !slice.is_empty() && &slice[0..1] == "\n" {
                    slice = &slice[1..];
                };
                let indent = slice.len();
                if indent > 0 && indent < min_indent {
                    min_indent = indent;
                }
                output.push('\n');
                output.push_str(slice);
            }
            BlockPart::EndBlock => {
                lex.bump_unchecked(sublex.span().end);
                if min_indent == usize::MAX {
                    min_indent = 0;
                }
                let mut lines = output.lines();
                let mut output = String::with_capacity(output.len());
                if let Some(first) = lines.next() {
                    let stripped = first.trim();
                    if !stripped.is_empty() {
                        output.push_str(stripped);
                        output.push('\n');
                    }
                }
                let mut last_line = 0;
                for line in lines {
                    last_line = output.len();
                    if line.len() > min_indent {
                        output.push_str(&line[min_indent..]);
                    }
                    output.push('\n');
                }
                if output[last_line..].trim().is_empty() {
                    output.truncate(last_line);
                }
                return Some(lex.extras.arena.alloc(output));
            }
            BlockPart::Error => break,
        }
    }
    None
}

fn parse_string<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
    match lex.slice() {
        r#""""# => Some(""),
        r#"""""# => lex_block_string(lex),
        "\"" => {
            // We can optimize lexing strings by avoiding the full StringPart lexer
            // for when we encounter "complex strings" by scanning for escape codes
            let remainder = lex.remainder();
            for (i, c) in remainder.char_indices() {
                match c {
                    '\n' | '\r' => return None,
                    '\\' => {
                        lex.bump_unchecked(i);
                        return lex_string(lex, remainder[0..i].to_string());
                    }
                    '"' => {
                        lex.bump_unchecked(i + 1);
                        return Some(&remainder[0..i]);
                    }
                    _ => {}
                }
            }
            None
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{Extras, Token};
    use logos::Logos;
    use bumpalo::Bump;

    #[test]
    fn empty() {
        let arena = Bump::new();
        let mut lex = Token::lexer_with_extras("", Extras { arena: &arena });
        assert_eq!(lex.next(), None);
        let mut lex = Token::lexer_with_extras(",,       # comment\n", Extras { arena: &arena });
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn symbols() {
        let arena = Bump::new();
        let extras = Extras { arena: &arena };
        let mut lex = Token::lexer_with_extras("[]{}()=:!&,", extras);
        assert_eq!(lex.next(), Some(Token::BracketOpen));
        assert_eq!(lex.next(), Some(Token::BracketClose));
        assert_eq!(lex.next(), Some(Token::BraceOpen));
        assert_eq!(lex.next(), Some(Token::BraceClose));
        assert_eq!(lex.next(), Some(Token::ParenOpen));
        assert_eq!(lex.next(), Some(Token::ParenClose));
        assert_eq!(lex.next(), Some(Token::Equal));
        assert_eq!(lex.next(), Some(Token::Colon));
        assert_eq!(lex.next(), Some(Token::Exclam));
        assert_eq!(lex.next(), Some(Token::Ampersand));
        assert_eq!(lex.next(), Some(Token::Comma));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn names() {
        let arena = Bump::new();
        let mut lex = Token::lexer_with_extras("hello world", Extras { arena: &arena });
        assert_eq!(lex.next(), Some(Token::Name("hello")));
        assert_eq!(lex.next(), Some(Token::Name("world")));
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer_with_extras("# comment\n hello", Extras { arena: &arena });
        assert_eq!(lex.next(), Some(Token::Name("hello")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn directives() {
        let arena = Bump::new();
        let mut lex = Token::lexer_with_extras("@directive", Extras { arena: &arena });
        assert_eq!(lex.next(), Some(Token::DirectiveName("directive")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn integers() {
        let arena = Bump::new();
        let mut lex = Token::lexer_with_extras("1 -1 123 -123 0", Extras { arena: &arena });
        assert_eq!(lex.next(), Some(Token::Integer("1")));
        assert_eq!(lex.next(), Some(Token::Integer("-1")));
        assert_eq!(lex.next(), Some(Token::Integer("123")));
        assert_eq!(lex.next(), Some(Token::Integer("-123")));
        assert_eq!(lex.next(), Some(Token::Integer("0")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn floats() {
        let arena = Bump::new();
        let mut lex = Token::lexer_with_extras(
            "1.0 -10.10 -10.10E10 1.1e-1 1e1 0.0",
            Extras { arena: &arena },
        );
        assert_eq!(lex.next(), Some(Token::Float("1.0")));
        assert_eq!(lex.next(), Some(Token::Float("-10.10")));
        // TODO: is this acceptable? verify with gql.js
        assert_eq!(lex.next(), Some(Token::Float("-10.10E10")));
        assert_eq!(lex.next(), Some(Token::Float("1.1e-1")));
        assert_eq!(lex.next(), Some(Token::Float("1e1")));
        assert_eq!(lex.next(), Some(Token::Float("0.0")));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn strings() {
        let arena = Bump::new();
        let mut lex = Token::lexer_with_extras("\"hello world\"", Extras { arena: &arena });
        assert_eq!(lex.next(), Some(Token::String("hello world")));
        assert_eq!(lex.next(), None);
        let mut lex = Token::lexer_with_extras("\"\"", Extras { arena: &arena });
        assert_eq!(lex.next(), Some(Token::String("")));
        assert_eq!(lex.next(), None);
        let mut lex =
            Token::lexer_with_extras("\"hello \\\" \\n world\"", Extras { arena: &arena });
        assert_eq!(lex.next(), Some(Token::String("hello \" \n world")));
        assert_eq!(lex.next(), None);
        let mut lex = Token::lexer_with_extras("\"\"\"hello block\"\"\"", Extras { arena: &arena });
        assert_eq!(lex.next(), Some(Token::String("hello block\n")));
        assert_eq!(lex.next(), None);
        let mut lex = Token::lexer_with_extras("\"\"\"\"\"\"", Extras { arena: &arena });
        assert_eq!(lex.next(), Some(Token::String("")));
        assert_eq!(lex.next(), None);
        let mut lex = Token::lexer_with_extras(
            "\"\"\"hello\n\r\t #test\n \\\"\"\" block\"\"\"",
            Extras { arena: &arena },
        );
        assert_eq!(
            lex.next(),
            Some(Token::String("hello\n\n #test\n\"\"\" block\n"))
        );
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn bad_strings() {
        let arena = Bump::new();
        let mut lex = Token::lexer_with_extras("\"\\ \"", Extras { arena: &arena });
        assert_eq!(lex.next(), Some(Token::Error));
        let mut lex = Token::lexer_with_extras("\"\n\"", Extras { arena: &arena });
        assert_eq!(lex.next(), Some(Token::Error));
        let mut lex = Token::lexer_with_extras("\"\r\"", Extras { arena: &arena });
        assert_eq!(lex.next(), Some(Token::Error));
    }
}
