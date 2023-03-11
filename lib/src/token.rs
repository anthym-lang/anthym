use crate::error::Error;
use crate::error::ErrorKind;
use logos::Logos;
use std::fmt::Display;
use std::ops::Range;

#[derive(Debug)]
pub(crate) struct Token {
    pub(crate) token: TokenType,
    pub(crate) raw: String,
    pub(crate) span: Range<usize>,
}

impl Token {
    pub(crate) fn into_error(self, source: &str, kind: ErrorKind) -> Error {
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

        Error {
            token: match kind {
                ErrorKind::Expected { .. } => Some(self.token),
                _ => None,
            },
            kind,
            line_str: source.lines().nth(line).unwrap_or_default().to_owned(),
            line_num: line + 1,
            span: Range {
                start: self.span.start - line_idx,
                end: self.span.end - line_idx,
            },
        }
    }
}

#[derive(Debug, Logos, PartialEq, Clone, Copy)]
pub(crate) enum TokenType {
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

    #[token("==")]
    Equality,

    #[token("!=")]
    Inequality,

    #[token("<")]
    LessThan,

    #[token("<=")]
    LessThanEqualTo,

    #[token(">")]
    GreaterThan,

    #[token(">=")]
    GreaterThanEqualTo,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Asterisk,

    #[token("/")]
    Slash,

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

    #[regex(r"[-+]?[0-9]+[.][0-9]*([eE][-+]?[0-9]+)?")]
    FloatLiteral,

    #[regex(r"[-+]?[0-9]+")]
    IntLiteral,

    #[regex("true|false")]
    BoolLiteral,

    #[regex(r#""([^"\\]|\\[\s\S])*""#)]
    StringLiteral,

    #[regex(r"'([^'\\]|\\[\s\S])'")]
    CharLiteral,

    #[regex("[a-zA-Z][a-zA-Z0-9_]*")]
    Ident,

    #[error]
    #[regex(r"[ \t\f\n]+", logos::skip)]
    Error,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenType::*;

        write!(
            f,
            "{}",
            match self {
                KeywordFn => "`fn`",
                KeywordIf => "`if`",
                KeywordElse => "`else`",
                KeywordLet => "`let`",
                KeywordMut => "`mut`",
                KeywordAnswer => "`answer`",
                Comma => "a comma",
                Colon => "a colon",
                Equals => "an equals sign",
                Equality => "equality (`==`)",
                Inequality => "inequality (`!=`)",
                LessThan => "a less than sign (`<`)",
                LessThanEqualTo => "a less than or equal to sign (`<=`)",
                GreaterThan => "a greater than sign (`>`)",
                GreaterThanEqualTo => "a greater than or equal to sign (`>=`)",
                Plus => "a plus sign (`+`)",
                Minus => "a minus sign (`-`)",
                Asterisk => "an asterisk (`*`)",
                Slash => "a forward slash (`/`)",
                Arrow => "a left-pointing arrow (`->`)",
                LParen => "a left parenthesis (`(`)",
                RParen => "a right parenthesis (`)`)",
                LBrace => "a left brace (`{`)",
                RBrace => "a right brace (`}`)",
                FloatLiteral => "a float literal (eg. `314.1e-2`)",
                IntLiteral => "an int literal (eg. `-5`)",
                BoolLiteral => "a boolean literal (`true` or `false`)",
                StringLiteral => "a string literal (eg. `\"Hello, World!\\n\"`)",
                CharLiteral => "a character literal (eg. `'\\n'`)",
                Ident => "an identifier (eg. `variable`)",
                Error => "an error",
            }
        )
    }
}
