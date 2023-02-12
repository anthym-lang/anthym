use crate::token::TokenType;
use std::error::Error as ErrorTrait;
use std::fmt::Display;
use std::iter::repeat;
use std::ops::Range;
use textwrap::wrap;

pub(crate) type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub(crate) enum ErrorKind {
    Eof { expected: Box<[TokenType]> },
    Expected { expected: Box<[TokenType]> },
    Unclosed { unclosed: TokenType },
    Unknown,
}

fn listify(list: &[impl Display]) -> String {
    let until_last = &list[..list.len() - 1]
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    let last = list.last().map(ToString::to_string);
    if let Some(last) = last {
        format!("{until_last}, or {last}")
    } else {
        until_last.into()
    }
}

impl ErrorKind {
    fn display(&self, got: &Option<TokenType>) -> String {
        match self {
            Self::Eof { expected } => format!("expected {}, got EOF", listify(expected)),
            Self::Expected { expected } => format!(
                "expected {}{}",
                listify(expected),
                match got {
                    Some(got) => format!(", got {got}"),
                    None => Default::default(),
                }
            ),
            Self::Unclosed { unclosed } => format!("unclosed {unclosed}"),
            Self::Unknown => "unknown token".into(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Error {
    pub(crate) token: Option<TokenType>,
    pub(crate) kind: ErrorKind,
    pub(crate) line_str: String,
    pub(crate) line_num: usize,
    pub(crate) span: Range<usize>,
}

impl Error {
    pub(crate) fn eof(source: &str, expected: Box<[TokenType]>) -> Self {
        let mut lines = source.lines();
        let line_str = lines.next_back().unwrap_or_default();
        Self {
            kind: ErrorKind::Eof { expected },
            token: None,
            line_str: line_str.into(),
            line_num: source.lines().count() + 1,
            span: Range {
                start: line_str.len() - 1,
                end: line_str.len(),
            },
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let space = " ".repeat(self.line_num.to_string().len());
        let arrows = wrap(
            format!(
                "{arrows}-- {message}",
                arrows = repeat(' ')
                    .enumerate()
                    .map(|(idx, ch)| if self.span.contains(&idx) { '^' } else { ch })
                    .take(self.line_str.len())
                    .collect::<String>()
                    .trim_end(),
                message = self.kind.display(&self.token)
            )
            .as_str(),
            100,
        )
        .join(format!("\n {space} . {}", " ".repeat(self.span.end + 3)).as_str());
        write!(
            f,
            r"On line {line_num}, column {span_start}:
 {space} |
 {line_num} | {line}
 {space} | {arrows}
 {space} |",
            span_start = self.span.start + 1,
            line_num = self.line_num,
            line = self.line_str,
        )
    }
}

impl ErrorTrait for Error {}
