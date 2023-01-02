use std::error::Error;
use std::fmt::Display;
use std::iter::repeat;
use std::ops::Range;
pub type Result<T> = std::result::Result<T, NiceError>;

#[derive(Debug, PartialEq)]
pub struct NiceError {
    pub message: String,
    pub line_str: String,
    pub line_num: usize,
    pub span: Range<usize>,
}

impl NiceError {
    pub fn eof(source: &str, message: impl Into<String>) -> Self {
        let mut lines = source.lines();
        let line_str = lines.next_back().unwrap();
        Self {
            message: message.into(),
            line_str: line_str.into(),
            line_num: source.lines().count() + 1,
            span: Range {
                start: line_str.len() - 1,
                end: line_str.len(),
            },
        }
    }

    pub fn msg(mut self, new: impl Into<String>) -> Self {
        self.message = new.into();
        self
    }
}

impl Display for NiceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let space = " ".repeat(self.line_num.to_string().len());
        let arrows = format!(
            "{arrows}-- {message}",
            arrows = repeat(' ')
                .enumerate()
                .map(|(idx, ch)| if self.span.contains(&idx) { '^' } else { ch })
                .take(self.line_str.len())
                .collect::<String>()
                .trim_end(),
            message = self.message
        );
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

impl Error for NiceError {}
