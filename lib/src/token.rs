use crate::error::NiceError;
use logos::Lexer;
use logos::Logos;
use std::ops::Range;
use std::str::FromStr;

#[derive(Debug)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Range<usize>,
}

impl SpannedToken {
    pub fn as_error(&self, source: &str, message: impl Into<String>) -> NiceError {
        let mut line = 0;
        let mut line_idx = 0;
        for (idx, char) in source.char_indices() {
            if idx == self.span.start {
                break;
            }

            if char == '\n' {
                line_idx = idx + 1;
                line += 1
            }
        }

        NiceError {
            message: message.into(),
            line_str: source.lines().nth(line).unwrap().to_owned(),
            line_num: line + 1,
            span: Range {
                start: self.span.start - line_idx,
                end: self.span.end - line_idx,
            },
        }
    }
}

#[derive(Debug, Logos, PartialEq)]
pub enum Token {
    #[token("fn")]
    KeywordFn,

    #[token("if")]
    KeywordIf,

    #[token("else")]
    KeywordElse,

    #[token("let")]
    KeywordLet,

    #[token("mut")]
    KeywordMut,

    #[token("answer")]
    KeywordAnswer,

    #[token(",")]
    Comma,

    #[token(":")]
    Colon,

    #[token("=")]
    Equals,

    #[token("->")]
    Arrow,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[regex(r"[-+]?[0-9]+[.][0-9]*([eE][-+]?[0-9]+)?", parse)]
    FloatLiteral(f32),

    #[regex(r"[-+]?[0-9]+", parse)]
    IntLiteral(i32),

    #[regex("true|false", parse)]
    BoolLiteral(bool),

    #[regex(r#""([^"\\]|\\[\s\S])*""#, unescape)]
    StringLiteral(String),

    #[regex(r"'([^'\\]|\\[\s\S])'", |lex| unescape(lex).and_then(|s| s.chars().next()))]
    CharLiteral(char),

    #[regex("[a-zA-Z][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Ident(String),

    #[error]
    #[regex(r"[ \t\f\n]+", logos::skip)]
    Error,
}

fn unescape(lex: &mut Lexer<'_, Token>) -> Option<String> {
    let mut result = String::new();
    let slice = lex.slice();
    let mut chars = slice.chars().skip(1).take(slice.len() - 2);

    loop {
        match chars.next() {
            Some('\\') => match chars.next()? {
                '"' => result.push('"'),
                '\\' => result.push('\\'),
                'r' => result.push('\r'),
                'n' => result.push('\n'),
                't' => result.push('\t'),
                '0' => result.push('\0'),
                '\'' => result.push('\''),
                'x' => {
                    let string: String = chars.clone().take(2).collect();

                    if string.len() != 2 {
                        return None;
                    }

                    for _ in 0..string.len() {
                        chars.next()?;
                    }

                    let value = u8::from_str_radix(&string, 16).ok()?;

                    result.push(char::from(value));
                }
                'u' => {
                    if chars.next()? != '{' {
                        return None;
                    }

                    let string: String = chars.clone().take_while(|c| *c != '}').collect();

                    if string.len() < 2 || 6 < string.len() {
                        return None;
                    }

                    for _ in 0..string.len() + 1 {
                        chars.next()?;
                    }

                    let value = u32::from_str_radix(&string, 16).ok()?;

                    result.push(char::from_u32(value)?);
                }
                _ => return None,
            },
            Some(c) => result.push(c),
            None => return Some(result),
        };
    }
}

fn parse<T, E>(lex: &mut Lexer<'_, Token>) -> Result<T, E>
where
    T: FromStr<Err = E>,
{
    lex.slice().parse()
}
