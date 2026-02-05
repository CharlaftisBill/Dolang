use serde::Serialize;

#[derive(Serialize, Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub line: usize,
    pub start: usize,
    pub end: usize,
    pub current_line_start: usize,
}

impl Span {
    pub fn new(line: usize, start: usize, end: usize, current_line_start: usize) -> Self {
        Self {
            line,
            start,
            end,
            current_line_start,
        }
    }

    // pub fn as_str<'a>(&self, source: &'a str) -> &'a str {
    //     &source[self.start..self.end]
    // }
}
