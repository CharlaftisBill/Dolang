use serde::Serialize;

use crate::{
    ast::{
        self, Ast, Node, NodeId,
        nodes::{BinaryOp, UnaryOp},
        token::{Tag, Token},
    },
    token_panic,
};
use std::collections::HashMap;

#[derive(Serialize, Debug, Clone)]
struct DeclarationDetails {
    node: NodeId,
    kind: String,
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    src: &'a Vec<char>,
    at: usize,
    ast: &'a mut Ast,
    declaration_context: HashMap<String, DeclarationDetails>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], src: &'a Vec<char>, ast: &'a mut Ast) -> Self {
        Self {
            tokens,
            src,
            at: 0,
            ast,
            declaration_context: HashMap::new(),
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
            token_panic!(
                self.tokens[self.at],
                self.src,
                "Expected, '{:?}' but instead got '{:?}'.",
                expected,
                current
            )
        }

        self.advance();
    }

    fn print_head(&self, caller: &str) {
        println!(
            "=================================================================================="
        );
        println!("========== {} ==========", caller);
        println!(
            "=================================================================================="
        );
        println!(
            "  =>  Before  : '{:?}'",
            self.tokens[self.at - 1].kind.as_str()
        );
        println!("  =>  Now     : '{:?}'", self.tokens[self.at].kind.as_str());
        println!(
            "  =>  Next    : '{:?}'",
            self.tokens[self.at + 1].kind.as_str()
        );
        println!(
            "=================================================================================="
        );
    }

    pub fn parse(&mut self, src: &Vec<char>) -> NodeId {
        let mut statements = Vec::new();

        while self.tokens[self.at].kind != ast::token::Tag::EOF {
            match &self.tokens[self.at].kind {
                ast::token::Tag::IDENT(_) => {
                    statements.push(self.parse_declaration_or_assignment())
                }
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

    fn parse_declaration_or_assignment(&mut self) -> NodeId {
        let name = match self.tokens[self.at].kind {
            ast::token::Tag::IDENT(ref s) => s.clone(),
            _ => {
                token_panic!(self.tokens[self.at], self.src, "Expected variable name.");
            }
        };

        if self.declaration_context.contains_key(&name) {
            return self.parse_assignment(&name);
        }

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
        if type_name != "func" {
            self.advance();
        }

        let (is_const, value) = self.parse_value(&type_name);
        let declaration = self.ast.add(
            Node::DECLARATION {
                name: name.to_string(),
                kind: type_node,
                public: is_public,
                constant: is_const,
            },
            self.tokens[self.at].span,
        );

        self.declaration_context.insert(
            name,
            DeclarationDetails {
                node: declaration,
                kind: type_name.clone(),
            },
        );

        match value {
            Some(v) => {
                let op = self.ast.add(
                    Node::IDENTIFIER((if is_const { ":" } else { "=" }).to_string()),
                    self.tokens[self.at].span,
                );
                self.ast.add(
                    Node::ASSIGNMENT {
                        declaration,
                        operator: op,
                        value: v,
                    },
                    self.tokens[self.at].span,
                )
            }
            _ => declaration,
        }
    }

    fn parse_assignment(&mut self, name: &String) -> NodeId {
        if !self.tokens[self.at + 1].kind.as_str().ends_with("=") {
            token_panic!(
                self.tokens[self.at + 1],
                self.src,
                "'{:?}' is already declared. If you are trying to assign a value '= <value>' is expected.",
                name
            );
        }

        let declaration = match self.declaration_context.get(name) {
            Some(decl) => decl.clone(),
            _ => token_panic!(
                self.tokens[self.at],
                self.src,
                "Trying to assign value to variable that is not yet declared."
            ),
        };

        self.advance();

        let op = self.ast.add(
            Node::IDENTIFIER(self.tokens[self.at].kind.as_str().to_string()),
            self.tokens[self.at].span,
        );

        let (_, value) = self.parse_value(&declaration.kind);

        match value {
            Some(v) => self.ast.add(
                Node::ASSIGNMENT {
                    declaration: declaration.node,
                    operator: op,
                    value: v,
                },
                self.tokens[self.at].span,
            ),
            _ => token_panic!(
                self.tokens[self.at],
                self.src,
                "An assignment should always be assigned a value."
            ),
        }
    }

    fn parse_value(&mut self, type_name: &String) -> (bool, Option<NodeId>) {
        let mut is_const = true;

        let value = match &self.tokens[self.at].kind {
            ast::token::Tag::SYMBOL(symbol) => {
                if type_name.as_str() == "func" {
                    is_const = self.tokens[self.at - 1].kind.as_str() == ":";
                } else {
                    is_const = symbol == ":";
                }

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
                    "array" => self.parse_array(),
                    "interface" => todo!("interfaces are not yet implemented!"),
                    _ => self.parse_expr(0),
                };

                Some(x)
            }
            _ => None,
        };

        (is_const, value)
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
                let ident_list = self.parse_identifier_list("{", "}");
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
                    node_type = "func";

                    let params = self.parse_variable_list("(", ")");
                    // hot fix
                    self.at -= 1;
                    let returns = self.parse_identifier_list(")", ":");

                    // other hot fix
                    if returns.len() == 0 {
                        self.advance();
                    }

                    let signature = Node::FUNCSIGNATURE { params, returns };
                    self.ast.add(signature, self.tokens[self.at - 1].span)
                }
                "[" => {
                    node_type = "array";
                    self.advance();
                    let size = match &self.tokens[self.at].kind {
                        Tag::INTEGER(i) => i.parse::<usize>().unwrap(),
                        _ => token_panic!(
                            current_token,
                            self.src,
                            "Invalid array size: Array size should always be an integer.",
                        ),
                    };
                    self.advance();
                    self.expect_str("]");

                    let kind = match &self.tokens[self.at].kind {
                        Tag::IDENT(s) => self
                            .ast
                            .add(Node::IDENTIFIER(s.to_string()), self.tokens[self.at].span),
                        _ => token_panic!(current_token, self.src, "Invalid array type.",),
                    };

                    self.ast
                        .add(Node::ARRAYKIND { kind, size }, self.tokens[self.at].span)
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

    fn parse_identifier_list(
        &mut self,
        starts_with_symbol: &str,
        ends_to_symbol: &str,
    ) -> Vec<NodeId> {
        self.expect_str(starts_with_symbol);

        let mut idents = Vec::new();
        if self.tokens[self.at].kind.as_str() == ends_to_symbol {
            return idents;
        }

        loop {
            let current_token = &self.tokens[self.at];
            if self.tokens[self.at].kind.as_str() == ends_to_symbol {
                break;
            }

            if !matches!(current_token.kind, Tag::IDENT(_)) {
                token_panic!(
                    current_token,
                    self.src,
                    "Expected 'identifier', got '{:?}'",
                    current_token.kind.as_str()
                );
            }

            let ident = Node::IDENTIFIER(current_token.kind.as_str().to_string());
            self.advance();

            idents.push(self.ast.add(ident, current_token.span));

            if self.tokens[self.at].kind.as_str() != ends_to_symbol {
                self.expect_str(",");
            }

            if matches!(self.tokens[self.at].kind, Tag::NEWLINE(_))
            {
                self.advance();
            }
        }

        self.expect_str(ends_to_symbol);

        idents
    }

    fn parse_variable_list(
        &mut self,
        starts_with_symbol: &str,
        ends_to_symbol: &str,
    ) -> Vec<NodeId> {
        self.expect_str(starts_with_symbol);

        let mut variables = Vec::new();
        if self.tokens[self.at].kind.as_str() == ends_to_symbol {
            self.advance();
            return variables;
        }

        if matches!(self.tokens[self.at].kind, Tag::NEWLINE(_)) {
            self.advance();
        }

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

            let node_id = self.ast.add(
                Node::DECLARATION {
                    name: var_name.to_string(),
                    kind: var_type,
                    public: false,
                    constant: false,
                },
                current_token.span,
            );
            variables.push(node_id);

            if self.tokens[self.at].kind.as_str() == ends_to_symbol {
                break;
            }

            self.expect_str(",");

            if matches!(self.tokens[self.at].kind, Tag::NEWLINE(_)) {
                self.advance();
            }
        }

        self.expect_str(ends_to_symbol);

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

            match &self.tokens[self.at].kind {
                Tag::NEWLINE(_) | Tag::COMMENT(_) => self.advance(),
                Tag::IDENT(s) => {
                    node_id = match &self.tokens[self.at + 1].kind {
                        Tag::IDENT(_)=> Some(self.parse_declaration_or_assignment()),
                        Tag::SYMBOL(sym) => {
                            if sym.contains("=") {
                                return Some(self.parse_assignment(s));
                            }else if  sym == "["{
                                return Some(self.parse_declaration_or_assignment());
                            }

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
                    "success" => node_id = self.parse_success(start_token),
                    "failure" => node_id = self.parse_failure(start_token),
                    "if" => node_id = self.parse_if(start_token),
                    "else" => node_id = self.parse_else(start_token),
                    "while" => node_id = self.parse_while(start_token),
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

    fn parse_success(&mut self, start_token: &Token) -> Option<NodeId> {
        self.advance();
        let return_values = self.parse_expression_list();
        Some(
            self.ast
                .add(Node::SUCCESS { return_values }, start_token.span),
        )
    }

    fn parse_failure(&mut self, start_token: &Token) -> Option<NodeId> {
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
        Some(self.ast.add(
            Node::FAILURE {
                reason: reason.to_string(),
                return_values,
            },
            start_token.span,
        ))
    }

    fn parse_if(&mut self, start_token: &Token) -> Option<NodeId> {
        self.advance();

        let condition = self.parse_expr(0);
        self.expect_str("{");
        let body = self.parse_code_block();

        Some(self.ast.add(Node::IF { condition, body }, start_token.span))
    }

    fn parse_else(&mut self, start_token: &Token) -> Option<NodeId> {
        self.advance();

        let condition = match self.tokens[self.at].kind.as_str() {
            "{" => None,
            _ => Some(self.parse_expr(0)),
        };
        self.expect_str("{");
        let body = self.parse_code_block();

        Some(
            self.ast
                .add(Node::ELSE { condition, body }, start_token.span),
        )
    }

    fn parse_while(&mut self, start_token: &Token) -> Option<NodeId> {
        self.advance();

        let condition = self.parse_expr(0);
        self.expect_str("{");
        let body = self.parse_code_block();

        Some(
            self.ast
                .add(Node::WHILE { condition, body }, start_token.span),
        )
    }

    fn parse_array(&mut self) -> NodeId {
        self.expect_str("[");

        let kind = match &self.tokens[self.at].kind {
            Tag::IDENT(i) => self
                .ast
                .add(Node::IDENTIFIER(i.to_string()), self.tokens[self.at].span),
            _ => token_panic!(self.tokens[self.at], self.src, "Expected array type."),
        };
        self.advance();

        self.expect_str(":");
        let value = self.parse_expression_list();

        self.expect_str("]");

        self.ast
            .add(Node::ARRAYVALUE { kind, value }, self.tokens[self.at].span)
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
