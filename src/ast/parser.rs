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

    fn expect_str(&mut self, expected: &str) {
        if self.at >= self.tokens.len() {
            panic!("Expected '{expected}', but reached end of file.")
        }

        let current = self.tokens[self.at].kind.as_str();
        if current != expected {
            panic!(
                "Expected, '{:?}' but instead got '{:?}'.",
                expected, current
            )
        }

        self.advance();
    }

    pub fn parse(&mut self, src: &Vec<char>) -> NodeId {
        let mut statements = Vec::new();

        while self.tokens[self.at].kind != ast::token::Tag::EOF {
            match &self.tokens[self.at].kind {
                ast::token::Tag::IDENT(_) => statements.push(self.parse_declaration()),
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

    fn parse_declaration(&mut self) -> NodeId {
        let name = match self.tokens[self.at].kind {
            ast::token::Tag::IDENT(ref s) => s.clone(),
            _ => {
                token_panic!(self.tokens[self.at], self.src, "Expected variable name.");
            }
        };
        self.advance();

        let is_public = match name.chars().next() {
            Some(character) => character.is_ascii_uppercase(),
            None => {
                token_panic!(
                    self.tokens[self.at],
                    self.src,
                    "Expected variable name not being an empty string"
                );
            }
        };

        let (type_name, type_node) = self.parse_type();
        self.advance();

        let mut is_const = false;
        let value = match &self.tokens[self.at].kind {
            ast::token::Tag::SYMBOL(symbol) => {
                is_const = symbol == ":";
                self.advance();

                let x = match type_name.as_str() {
                    "type" | "union" => match self.parse_user_type_or_union() {
                        Some(n) => n,
                        _ => token_panic!(
                            self.tokens[self.at],
                            self.src,
                            "Invalid {type_name} syntax",
                        ),
                    },
                    "enum" => match self.parse_enum() {
                        Some(n) => n,
                        _ => token_panic!(
                            self.tokens[self.at],
                            self.src,
                            "Invalid {type_name} syntax",
                        ),
                    },
                    "func" => match self.parse_code_block() {
                        Some(n) => n,
                        _ => token_panic!(
                            self.tokens[self.at],
                            self.src,
                            "Invalid {type_name} syntax",
                        ),
                    },
                    "interface" => todo!("interface are not yet implemented!"),
                    _ => self.parse_expr(0),
                };

                Some(x)
            }
            _ => None,
        };

        self.ast.add(
            Node::VAR {
                name,
                kind: type_node,
                public: is_public,
                constant: is_const,
                value,
            },
            self.tokens[self.at].span,
        )
    }

    fn parse_user_type_or_union(&mut self) -> Option<NodeId> {
        let start_token = &self.tokens[self.at];

        let mut node_id: Option<NodeId> = None;
        match &start_token.kind {
            Tag::NEWLINE(_) | Tag::COMMENT(_) => self.advance(),
            Tag::IDENT(s) => {
                node_id = Some(
                    self.ast
                        .add(Node::IDENTIFIER(s.to_string()), start_token.span),
                )
            }
            Tag::SYMBOL(_) => {
                let var_list = self.parse_variable_list("{", "}");
                node_id = Some(self.ast.add(Node::BLOCK(var_list), start_token.span));
            }
            _ => node_id = None,
        }
        self.advance();

        node_id
    }

    fn parse_enum(&mut self) -> Option<NodeId> {
        let start_token = &self.tokens[self.at];

        let mut node_id: Option<NodeId> = None;
        match &start_token.kind {
            Tag::NEWLINE(_) | Tag::COMMENT(_) => self.advance(),
            Tag::IDENT(s) => {
                node_id = Some(
                    self.ast
                        .add(Node::IDENTIFIER(s.to_string()), start_token.span),
                )
            }
            Tag::SYMBOL(s) => {
                let ident_list = self.parse_ident_list("{", "}");
                node_id = Some(self.ast.add(Node::BLOCK(ident_list), start_token.span));
            }
            _ => node_id = None,
        }
        self.advance();

        node_id
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

            if l_bp == 0 {
                break;
            }

            if l_bp < min_bp {
                break;
            }

            self.advance();
            if start_token.kind.as_str() == "(" {
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
                    self.expect_str(")");
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

    fn parse_type(&mut self) -> (String, NodeId) {
        let current_token = &self.tokens[self.at];

        let mut node_type = "";
        let node_id = match &current_token.kind {
            Tag::IDENT(s) | Tag::KEYWORD(s) => {
                node_type = s;
                self.ast
                    .add(Node::IDENTIFIER(s.to_string()), self.tokens[self.at].span)
            }
            Tag::SYMBOL(s) => match s.as_str() {
                "(" => {
                    let signature = Node::FUNCSIGNATURE {
                        params: self.parse_ident_list("(", ")"),
                        returns: self.parse_variable_list(")", ":"),
                    };
                    self.ast.add(signature, self.tokens[self.at].span)
                }
                _ => token_panic!(
                    current_token,
                    self.src,
                    "Invalid Type. A type cannot start with symbol '{:?}'",
                    s
                ),
            },
            _ => token_panic!(current_token, self.src, "Invalid Type"),
        };
        // self.advance();

        (node_type.to_string(), node_id)
    }

    fn parse_call(&mut self, func: NodeId) -> NodeId {
        let starting_token = &self.tokens[self.at];

        let mut args = Vec::new();
        if self.tokens[self.at].kind.as_str() != ")" {
            args = self.parse_expression_list();
        }
        self.expect_str(")");

        self.ast.add(Node::CALL { func, args }, starting_token.span)
    }

    fn parse_ident_list(&mut self, starts_with_symbol: &str, ends_to_symbol: &str) -> Vec<NodeId> {
        self.expect_str(starts_with_symbol);

        let mut args = Vec::new();
        if self.tokens[self.at].kind.as_str() == ends_to_symbol {
            return args;
        }
        self.advance();

        loop {            
            let current_token = &self.tokens[self.at];

            if current_token.kind.as_str() == ends_to_symbol {
                break;
            }

            if !matches!(current_token.kind, Tag::IDENT(_)) {
                token_panic!(
                    current_token,
                    self.src,
                    "Expected Identifier, got '{:?}'",
                    current_token.kind.as_str()
                );
            }

            parse var type pairs right
            let var = Node::VAR(current_token.kind.as_str().to_string());
            self.advance();

            let node_id = self.ast.add(var, current_token.span);
            args.push(node_id);
            self.advance();

            if self.tokens[self.at].kind.as_str() == ","
                || matches!(self.tokens[self.at].kind, Tag::NEWLINE(_))
            {
                self.advance();
            }
        }

        args
    }

    fn parse_variable_list(
        &mut self,
        starts_with_symbol: &str,
        ends_to_symbol: &str,
    ) -> Vec<NodeId> {
        self.expect_str(starts_with_symbol);

        let mut variables = Vec::new();
        if self.tokens[self.at].kind.as_str() == ends_to_symbol {
            return variables;
        }
        self.advance();

        loop {
            let current_token = &self.tokens[self.at];

            if current_token.kind.as_str() == ends_to_symbol {
                break;
            }

            if !matches!(current_token.kind, Tag::IDENT(_)) {
                token_panic!(
                    current_token,
                    self.src,
                    "Expected 'Variable name', got '{:?}'",
                    current_token.kind.as_str()
                );
            }
            let var_name = current_token.kind.as_str();
            self.advance();

            let (_, var_type) = self.parse_type();
            self.advance();

            let var = Node::VAR {
                name: var_name.to_string(),
                kind: var_type,
                public: false,
                constant: false,
                value: None,
            };
            let node_id = self.ast.add(var, current_token.span);
            variables.push(node_id);

            self.advance();

            if self.tokens[self.at].kind.as_str() == ","
                || matches!(self.tokens[self.at].kind, Tag::NEWLINE(_))
            {
                self.advance();
            }
        }
        variables
    }

    fn parse_expression_list(&mut self) -> Vec<NodeId> {
        let mut expressions = Vec::new();
        loop {
            expressions.push(self.parse_expr(0));
            if self.tokens[self.at].kind.as_str() == "," {
                self.advance();

                if matches!(self.tokens[self.at].kind, Tag::NEWLINE(_))
                    || matches!(self.tokens[self.at].kind, Tag::COMMENT(_))
                {
                    self.advance();
                }
            } else {
                break;
            }
        }
        expressions
    }

    fn parse_code_block(&mut self) -> Option<NodeId> {
        let start_token = &self.tokens[self.at];

        let mut block = vec![];
        loop {
            let mut node_id: Option<NodeId> = None;

            println!(
                "      ±§ (parse_code_block): {:?}",
                self.tokens[self.at].kind
            );

            match &self.tokens[self.at].kind {
                Tag::NEWLINE(_) | Tag::COMMENT(_) => self.advance(),
                Tag::IDENT(s) => {
                    node_id = match &self.tokens[self.at + 1].kind {
                        Tag::IDENT(_) | Tag::KEYWORD(_) => Some(self.parse_declaration()),
                        Tag::SYMBOL(_) => {
                            self.advance();
                            self.expect_str("(");

                            let func_node_id = self
                                .ast
                                .add(Node::IDENTIFIER(s.to_string()), start_token.span);

                            Some(self.parse_call(func_node_id))
                        }
                        _ => token_panic!(start_token, self.src, "Invalid code in block."),
                    }
                }
                Tag::SYMBOL(s) => {
                    if s == "}" {
                        break;
                    }
                    token_panic!(
                        start_token,
                        self.src,
                        "Invalid code in block. The only symbol can be '}}'"
                    )
                }
                Tag::KEYWORD(s) => match s.as_str() {
                    "success" => {
                        self.advance();
                        let return_values = self.parse_expression_list();
                        node_id = Some(
                            self.ast
                                .add(Node::SUCCESS { return_values }, start_token.span),
                        );
                    }
                    "failure" => {
                        let next_token = &self.tokens[self.at + 1];
                        if !matches!(next_token.kind, Tag::IDENT(_)) {
                            token_panic!(
                                next_token,
                                self.src,
                                "Expected failure reason, instead got '{:?}'",
                                next_token.kind.as_str()
                            )
                        }

                        self.advance();
                        let reason = next_token.kind.as_str();
                        self.advance();

                        let return_values = self.parse_expression_list();
                        node_id = Some(self.ast.add(
                            Node::FAILURE {
                                reason: reason.to_string(),
                                return_values,
                            },
                            start_token.span,
                        ));
                    }
                    "if" => {
                        self.advance();

                        let condition = self.parse_expr(0);
                        self.expect_str("{");
                        let body = self.parse_code_block();

                        node_id =
                            Some(self.ast.add(Node::IF { condition, body }, start_token.span));
                    }
                    "else" => {
                        self.advance();

                        let condition = match self.tokens[self.at + 1].kind.as_str() {
                            "{" => None,
                            _ => Some(self.parse_expr(0)),
                        };
                        self.expect_str("{");
                        let body = self.parse_code_block();

                        node_id = Some(
                            self.ast
                                .add(Node::ELSE { condition, body }, start_token.span),
                        );
                    }
                    "while" => {
                        self.advance();

                        let condition = self.parse_expr(0);
                        self.expect_str("{");
                        let body = self.parse_code_block();

                        node_id = Some(
                            self.ast
                                .add(Node::WHILE { condition, body }, start_token.span),
                        );
                    }
                    "loop" => {
                        self.advance();

                        let body = self.parse_code_block();

                        node_id = Some(self.ast.add(Node::LOOP { body }, start_token.span));
                    }
                    _ => unreachable!("No Keyword '{:?}', is recognizable", s),
                },
                _ => token_panic!(
                    start_token,
                    self.src,
                    "Invalid code in block. Not supported token {:?}",
                    start_token.kind.as_str()
                ),
            }

            if matches!(node_id, Some(_)) {
                block.push(node_id?);
            }
        }

        self.advance();

        Some(self.ast.add(Node::BLOCK(block), start_token.span))
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

            _ => panic!("Unknown binary op: '{kind}'"),
        }
    }

    fn infix_binding_power(&self, symbol: &str) -> (u8, u8) {
        match symbol {
            "(" => (20, 0),
            "*" | "/" | "%" | "<<" | ">>" | "&" | " &^" => (9, 10),
            "+" | "-" | "|" | "^" => (7, 8),
            "==" | "!=" | "<" | "<=" | ">" | ">=" => (5, 6),
            "&&" => (3, 4),
            "||" => (1, 2),
            _ => (0, 0),
        }
    }
}
