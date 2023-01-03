use std::error::Error;
use std::fmt::Display;
use std::iter::repeat;
use std::ops::Range;
pub(crate) type Result<T> = std::result::Result<T, NiceError>;

#[derive(Debug, PartialEq)]
pub(crate) struct NiceError {
    pub(crate) message: String,
    pub(crate) line_str: String,
    pub(crate) line_num: usize,
    pub(crate) span: Range<usize>,
}

impl NiceError {
    pub(crate) fn eof(source: &str, message: impl Into<String>) -> Self {
        let mut lines = source.lines();
        let line_str = lines.next_back().unwrap_or_default();
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

    pub(crate) fn msg(mut self, new: impl Into<String>) -> Self {
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
