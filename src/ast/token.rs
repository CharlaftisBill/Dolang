use std::fmt::Write;

use crate::ast::{Span, lexer::Lexer};

#[derive(Debug, PartialEq)]
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
    EOF,
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
            Tag::EOF => "<EOF>",
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
            Tag::EOF => 0,
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: Tag,
    pub span: Span,
}

impl Token {
    pub fn new(lexer_state: &Lexer, tag: Tag) -> Self {
        let stops = lexer_state.at;
        let starts = stops - tag.len();

        Self {
            kind: tag,
            span: Span {
                line: lexer_state.line,
                start: starts,
                end: stops,
                current_line_start: lexer_state.last_new_line_at,
            },
        }
    }

    pub fn new_invalid(lexer_state: &mut Lexer, error_msg: String) -> Self {
        let starts = lexer_state.at;
        lexer_state.at = lexer_state.src.len(); // stop lexer

        return Token {
            kind: super::token::Tag::INVALID(error_msg),
            span: Span::new(
                lexer_state.line,
                lexer_state.last_new_line_at,
                starts + 1,
                lexer_state.last_new_line_at,
            ),
        };
    }

    pub fn panic_message(&self, src: &Vec<char>, args: std::fmt::Arguments) -> String {
        let mut index = self.span.current_line_start;

        let mut sb: String = String::with_capacity(256);

        write!(
            &mut sb,
            "\nLn:'{}:{}-{}': ",
            self.span.line + 1,
            self.span.start,
            self.span.end,
        )
        .unwrap();
        sb.write_fmt(args).unwrap();
        sb.push('\n');
        sb.push('\n');

        while index + 1 < src.len() {
            sb.push(src[index]);
            index += 1;
            if src[index] == '\n' {
                sb.push('\n');
                break;
            }
        }

        // println!("'{}'", sb);
        // println!("{} - {}", self.span.end, self.span.current_line_start -1);
        for _ in 0..(self.span.end - (self.span.current_line_start - 1)) {
            sb.push(' ');
        }

        for _ in 0..(self.span.end - self.span.start) {
            sb.push('▔');
        }

        sb
    }

    pub fn is_terminator(&self) -> bool {
        return matches!(self.kind, Tag::NEWLINE(_))
            || matches!(self.kind, Tag::COMMENT(_))
            || matches!(self.kind, Tag::EOF);
    }

    // pub fn to_string(&self) -> String {
    //     format!(
    //         "Tag: {:?} Value: {:?}, L{}:{}-{} ({})",
    //         self.kind,
    //         self.kind.as_str(),
    //         self.span.line + 1,
    //         self.span.start,
    //         self.span.end,
    //         self.span.current_line_start,
    //     )
    // }
}

#[macro_export]
macro_rules! token_panic {
    ($token:expr, $src:expr, $($arg:tt)*) => {
        panic!(
            "{}",
            $token.panic_message($src, format_args!($($arg)*))
        )
    };
}
