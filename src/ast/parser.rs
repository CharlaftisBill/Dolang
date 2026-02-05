use crate::{
    ast::{
        self, Ast, Node, NodeId,
        nodes::{BinaryOp, UnaryOp},
        token::{Tag, Token},
    },
    token_panic,
};

pub struct Parser<'a> {
    tokens: &'a [Token],
    src: &'a Vec<char>,
    at: usize,
    ast: &'a mut Ast,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], src: &'a Vec<char>, ast: &'a mut Ast) -> Self {
        Self {
            tokens,
            src,
            at: 0,
            ast,
        }
    }

    fn advance(&mut self) {
        if self.at < self.tokens.len() - 1 {
            self.at += 1;
        }
    }

    fn expect(&mut self, expected: &str) {
        if self.at >= self.tokens.len() {
            panic!("Expected '{expected}', but reached end of file.")
        }

        let current = self.tokens[self.at].kind.as_str();
        if current != expected {
            panic!("Expected, '{:?}' but instead got '{:?}'.", expected, current)
        }
        
        self.advance();
    }

    pub fn parse(&mut self, src: &Vec<char>) -> NodeId {
        let mut statements = Vec::new();

        while self.tokens[self.at].kind != ast::token::Tag::EOF {
            match &self.tokens[self.at].kind {
                ast::token::Tag::IDENT(_) => statements.push(self.parse_declaration(src)),
                ast::token::Tag::COMMENT(_) | ast::token::Tag::NEWLINE(_) => self.advance(),
                ast::token::Tag::INVALID(description) => {
                    token_panic!(
                        self.tokens[self.at],
                        src,
                        "INVALID token tag '{}'",
                        description
                    );
                }
                _ => token_panic!(
                    self.tokens[self.at],
                    src,
                    "In this level only comments and declarations are permitted."
                ),
            }
        }

        self.ast
            .add(Node::BLOCK(statements), self.tokens[self.at].span)
    }

    fn parse_declaration(&mut self, src: &Vec<char>) -> NodeId {
        let name = match self.tokens[self.at].kind {
            ast::token::Tag::IDENT(ref s) => s.clone(),
            _ => {
                token_panic!(self.tokens[self.at], src, "Expected variable name.");
            }
        };
        self.advance();

        let is_public = match name.chars().next() {
            Some(character) => character.is_ascii_uppercase(),
            None => {
                token_panic!(
                    self.tokens[self.at],
                    src,
                    "Expected variable name not being an empty string"
                );
            }
        };

        let type_name = match self.tokens[self.at].kind {
            ast::token::Tag::IDENT(ref s) => s.clone(),
            _ => {
                token_panic!(self.tokens[self.at], src, "Expected type name");
            }
        };
        self.advance();

        let mut is_const = false;
        let value = match &self.tokens[self.at].kind {
            ast::token::Tag::SYMBOL(symbol) => {
                is_const = symbol == ":";
                self.advance();
                let x = self.parse_expr(0);
                Some(x)
            }
            _ => None,
        };

        self.ast.add(
            Node::VAR {
                name,
                kind: type_name,
                public: is_public,
                constant: is_const,
                value,
            },
            self.tokens[self.at].span,
        )
    }

    fn parse_expr(&mut self, min_bp: u8) -> NodeId {
        let mut lhs = self.parse_prefix();

        loop {
            let start_token = &self.tokens[self.at];
            if start_token.is_terminator() {
                return lhs;
            }

            if start_token.kind.as_str() == "++"
                || start_token.kind.as_str() == "--"
                || start_token.kind.as_str() == "="
            {
                break;
            }

            let (l_bp, r_bp) = self.infix_binding_power(start_token.kind.as_str());
            // println!("  ---> {}: ({l_bp}, {r_bp})", start_token.kind.as_str());
            println!("Token: '{}', BP: {}, MinBP: {}", start_token.kind.as_str(), l_bp, min_bp);

            if l_bp == 0 {
                break;
            }

            if l_bp < min_bp {
                break;
            }

            self.advance();
            if self.tokens[self.at].kind.as_str() == "("
                && matches!(start_token.kind, Tag::IDENT(_))
            {
                lhs = self.parse_call(lhs);
                continue;
            }

            if self.tokens[self.at].is_terminator() {
                return lhs;
            }

            let rhs = self.parse_expr(r_bp);
            lhs = self.ast.add(
                Node::BINARY(self.token_to_binary_op(start_token.kind.as_str(), lhs, rhs)),
                start_token.span,
            );
        }

        lhs
    }

    fn parse_prefix(&mut self) -> NodeId {
        let current_token: &Token = &self.tokens[self.at];
        self.advance();

        match &current_token.kind {
            Tag::INTEGER(i) => self
                .ast
                .add(Node::I32(i.parse().unwrap()), current_token.span),
            Tag::FLOAT(f) => self
                .ast
                .add(Node::F32(f.parse().unwrap()), current_token.span),
            Tag::STRING(s) => self.ast.add(Node::STR(s.to_string()), current_token.span),
            Tag::CHARACTER(c) => self
                .ast
                .add(Node::CHAR(c.chars().next().unwrap()), current_token.span),
            Tag::BOOL(b) => self.ast.add(Node::BOOL(b == "true"), current_token.span),

            Tag::IDENT(s) => self
                .ast
                .add(Node::IDENTIFIER(s.to_string()), current_token.span),

            Tag::SYMBOL(s) => match s.as_str() {
                "(" => {
                    let expr = self.parse_expr(0);
                    self.expect(")");
                    expr
                }
                "+" | "-" | "!" | "^" => {
                    let r_bp = 11;
                    let rhs = self.parse_expr(r_bp);

                    let op = match current_token.kind.as_str() {
                        "+" => UnaryOp::POS(rhs),
                        "-" => UnaryOp::NEG(rhs),
                        "!" => UnaryOp::NOT(rhs),
                        "^" => UnaryOp::BITNOT(rhs),
                        _ => unreachable!(),
                    };

                    self.ast.add(Node::UNARY(op), current_token.span)
                }
                _ => token_panic!(current_token, self.src, "Unexpected token symbol: '{}'", s),
            },
            _ => panic!("Unexpected token in expression: {:?}", current_token),
        }
    }

    fn parse_call(&mut self, func: NodeId) -> NodeId {
        let current_token = &self.tokens[self.at];

        let mut args = Vec::new();
        if current_token.kind.as_str() != ")" {
            loop {
                args.push(self.parse_expr(0));
                if current_token.kind.as_str() == "," {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(")");
        self.ast.add(Node::CALL { func, args }, current_token.span)
    }

    fn token_to_binary_op(&self, kind: &str, lhs: NodeId, rhs: NodeId) -> BinaryOp {
        match kind {
            "+" => BinaryOp::ADD(lhs, rhs),
            "-" => BinaryOp::SUB(lhs, rhs),
            "*" => BinaryOp::MUL(lhs, rhs),
            "/" => BinaryOp::DIV(lhs, rhs),
            "%" => BinaryOp::REM(lhs, rhs),

            "&" => BinaryOp::BITAND(lhs, rhs),
            "|" => BinaryOp::BITOR(lhs, rhs),
            "^" => BinaryOp::BITXOR(lhs, rhs),
            "<<" => BinaryOp::SHL(lhs, rhs),
            ">>" => BinaryOp::SHR(lhs, rhs),

            "==" => BinaryOp::EQ(lhs, rhs),
            "!=" => BinaryOp::NEQ(lhs, rhs),
            "<" => BinaryOp::LT(lhs, rhs),
            "<=" => BinaryOp::LTE(lhs, rhs),
            ">" => BinaryOp::GT(lhs, rhs),
            ">=" => BinaryOp::GTE(lhs, rhs),

            "&&" => BinaryOp::LOGICAND(lhs, rhs),
            "||" => BinaryOp::LOGICOR(lhs, rhs),
            // ... map others ...
            _ => panic!("Unknown binary op"),
        }
    }

    fn infix_binding_power(&self, symbol: &str) -> (u8, u8) {
        match symbol {
            // "(" => (20, 0),
            "*" | "/" | "%" | "<<" | ">>" | "&" | " &^" => (9, 10),
            "+" | "-" | "|" | "^" => (7, 8),
            "==" | "!=" | "<" | "<=" | ">" | ">=" => (5, 6),
            "&&" => (3, 4),
            "||" => (1, 2),
            _ => (0, 0),
        }
    }
}
