use crate::lexing::lexer::Lexer;

#[derive(Debug)]
pub enum Tag {
    INVALID(String),
    NEWLINE(String),
    IDENT(String),
    KEYWORD(String),
    COMMENT(String),
    SYMBOL(String),
    INTEGER(String),
    FLOAT(String),
    BOOL(String),
    STRING(String),
    CHARACTER(String),
}

impl Tag {
    pub fn as_str(&self) -> &str {
        match self {
            Tag::INVALID(s)
            | Tag::NEWLINE(s)
            | Tag::IDENT(s)
            | Tag::KEYWORD(s)
            | Tag::COMMENT(s)
            | Tag::SYMBOL(s)
            | Tag::INTEGER(s)
            | Tag::FLOAT(s)
            | Tag::BOOL(s)
            | Tag::STRING(s)
            | Tag::CHARACTER(s) => s,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Tag::INVALID(s)
            | Tag::NEWLINE(s)
            | Tag::IDENT(s)
            | Tag::KEYWORD(s)
            | Tag::COMMENT(s)
            | Tag::SYMBOL(s)
            | Tag::INTEGER(s)
            | Tag::FLOAT(s)
            | Tag::BOOL(s)
            | Tag::STRING(s)
            | Tag::CHARACTER(s) => s.len(),
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub tag: Tag,
    pub line: usize,
    pub current_line_start: usize,
    pub starts: usize,
    pub stops: usize,
}

impl Token {
    pub fn new(lexer_state: &Lexer, tag: Tag) -> Self {

        let stops = lexer_state.at;
        let starts = stops - tag.len();

        Self {
            tag,
            line: lexer_state.line,
            current_line_start: lexer_state.last_new_line_at,
            starts,
            stops,
        }
    }

    pub fn new_invalid(lexer_state: &mut Lexer, error_msg: String) -> Self {
        let starts = lexer_state.at;
        lexer_state.at = lexer_state.src.len(); // stop lexer

        return Token {
            tag: super::token::Tag::INVALID(error_msg),
            line: lexer_state.line,
            current_line_start: lexer_state.last_new_line_at,
            starts,
            stops: starts,
        };
    }

    pub fn to_string(&self) -> String {
        format!(
            "Tag: {:?} Value: {:?}, L{}:{}-{} ({})",
            self.tag,
            self.tag.as_str(),
            self.line + 1,
            self.starts,
            self.stops,
            self.current_line_start,
        )
    }
}
