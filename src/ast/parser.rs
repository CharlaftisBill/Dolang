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

macro_rules! print_head {
    ($parser:expr, $caller:expr) => {
        println!(
            "=================================================================================="
        );
        println!("========== {} ==========", $caller);
        println!(
            "=================================================================================="
        );
        if $parser.at > 0 {
            println!(
                "  =>  Before  : '{:?}'",
                $parser.tokens[$parser.at - 1].kind.as_str()
            );
        }
        println!(
            "  =>  Now     : '{:?}'",
            $parser.tokens[$parser.at].kind.as_str()
        );

        if $parser.at + 1 < $parser.tokens.len() {
            println!(
                "  =>  Next    : '{:?}'",
                $parser.tokens[$parser.at + 1].kind.as_str()
            );
        }
        println!(
            "=================================================================================="
        );
    };
}

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

    fn current_token(&self) -> &'a Token {
        &self.tokens[self.at]
    }

    fn current_str(&self) -> &'a str {
        self.tokens[self.at].kind.as_str()
    }

    fn advance(&mut self) {
        if self.at < self.tokens.len() - 1 {
            self.at += 1;
        }
    }

    fn skip_newlines_and_comments(&mut self) {
        while self.at < self.tokens.len()
            && (matches!(self.tokens[self.at].kind, Tag::NEWLINE(_))
                || matches!(self.tokens[self.at].kind, Tag::COMMENT(_)))
        {
            self.advance();
        }
    }

    fn expect_str(&mut self, expected: &str) {
        if self.at >= self.tokens.len() {
            panic!("Expected '{expected}', but reached end of file.")
        }

        let current = self.current_str();
        if current != expected {
            token_panic!(
                self.current_token(),
                self.src,
                "Expected '{:?}', but instead got '{:?}'.",
                expected,
                current
            )
        }

        self.advance();
    }

    pub fn parse(&mut self, src: &Vec<char>) -> NodeId {
        let mut statements = Vec::new();

        while self.current_token().kind != ast::token::Tag::EOF {
            match &self.current_token().kind {
                ast::token::Tag::IDENT(_) => {
                    statements.push(self.parse_declaration_or_assignment())
                }
                ast::token::Tag::COMMENT(_) | ast::token::Tag::NEWLINE(_) => self.advance(),
                ast::token::Tag::INVALID(description) => {
                    token_panic!(
                        self.current_token(),
                        src,
                        "INVALID token tag '{}'",
                        description
                    );
                }
                _ => token_panic!(
                    self.current_token(),
                    src,
                    "Invalid token '{:?}'. In this level only comments and declarations are permitted.",
                    self.current_token().kind,
                ),
            }
        }

        self.ast
            .add(Node::BLOCK(statements), self.current_token().span)
    }

    fn parse_declaration_or_assignment(&mut self) -> NodeId {
        let name = match self.current_token().kind {
            ast::token::Tag::IDENT(ref s) => s.clone(),
            _ => {
                token_panic!(self.current_token(), self.src, "Expected variable name.");
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
                    self.current_token(),
                    self.src,
                    "Expected variable name not being an empty string"
                );
            }
        };

        let (type_name, type_node) = self.parse_type();

        print_head!(self, "- [0] parse_declaration_or_assignment [0] -");
        if self.current_str() != ":" && self.current_str() != "=" {
            token_panic!(
                self.current_token(),
                self.src,
                "Expected '=' or ':' but instead got '{:?}'.",
                self.current_str(),
            );
        }
        let is_const = self.current_str() == ":";
        self.advance();
        print_head!(self, "-[1] parse_declaration_or_assignment [1] -");

        let value = self.parse_value(&type_name);
        let declaration = self.ast.add(
            Node::DECLARATION {
                name: name.to_string(),
                kind: type_node,
                public: is_public,
                constant: is_const,
            },
            self.current_token().span,
        );

        self.declaration_context.insert(
            name,
            DeclarationDetails {
                node: declaration,
                kind: type_name.clone(),
            },
        );

        let op = self.ast.add(
            Node::IDENTIFIER((if is_const { ":" } else { "=" }).to_string()),
            self.current_token().span,
        );

        self.ast.add(
            Node::ASSIGNMENT {
                declaration,
                operator: op,
                value,
            },
            self.current_token().span,
        )
    }

    fn parse_assignment(&mut self, name: &String) -> NodeId {
        let declaration = match self.declaration_context.get(name) {
            Some(decl) => decl.clone(),
            _ => token_panic!(
                self.current_token(),
                self.src,
                "Trying to assign value to variable that is not yet declared."
            ),
        };

        print_head!(self, "- [0] parse_assignment [0] -");
        self.advance();
        print_head!(self, "- [1] parse_assignment [1] -");

        if !self.current_str().ends_with("=") {
            token_panic!(
                self.current_token(),
                self.src,
                "'{:?}' is already declared. If you are trying to assign a value '= <value>' is expected.",
                name
            );
        }

        let op = self.ast.add(
            Node::IDENTIFIER(self.current_str().to_string()),
            self.current_token().span,
        );
        self.advance();

        print_head!(self, "- [2] parse_assignment [2] -");
        let value = self.parse_value(&declaration.kind);
        print_head!(self, "- [3] parse_assignment [3] -");

        self.ast.add(
            Node::ASSIGNMENT {
                declaration: declaration.node,
                operator: op,
                value,
            },
            self.current_token().span,
        )
    }

    fn parse_value(&mut self, type_name: &String) -> NodeId {
        print_head!(self, "-[0] parse_value [0] -");

        println!(
            "          ---------------------------------->>>>>> {:?}",
            type_name
        );

        print_head!(self, "- [1] parse_value[1] -");
        let value = match type_name.as_str() {
            "type" | "union" => match self.parse_custom_data_structure(false) {
                Some(n) => n,
                _ => token_panic!(self.current_token(), self.src, "Invalid {type_name} syntax",),
            },
            "enum" => match self.parse_custom_data_structure(true) {
                Some(n) => n,
                _ => token_panic!(self.current_token(), self.src, "Invalid {type_name} syntax",),
            },
            "func" => match self.parse_code_block() {
                Some(n) => n,
                _ => token_panic!(self.current_token(), self.src, "Invalid {type_name} syntax",),
            },
            "array" => self.parse_array(),
            "interface" => todo!("interfaces are not yet implemented!"),
            _ => self.parse_expr(0),
        };
        print_head!(self, "- [2] parse_value [2] -");

        value
    }

    fn parse_custom_data_structure(&mut self, is_enum: bool) -> Option<NodeId> {
        let start_span = self.current_token().span;
        let mut node_id: Option<NodeId> = None;

        self.skip_newlines_and_comments();

        match &self.current_token().kind {
            Tag::IDENT(s) => {
                node_id = Some(self.ast.add(Node::IDENTIFIER(s.to_string()), start_span))
            }
            Tag::SYMBOL(_) => {
                let block_nodes = if is_enum {
                    self.parse_identifier_list("{", "}")
                } else {
                    self.parse_variable_list("{", "}")
                };
                node_id = Some(self.ast.add(Node::BLOCK(block_nodes), start_span));
            }
            _ => node_id = None,
        }
        self.advance();

        node_id
    }

    fn parse_expr(&mut self, min_bp: u8) -> NodeId {
        let mut lhs = self.parse_prefix();

        loop {
            let start_token = self.current_token();

            if start_token.is_terminator() {
                return lhs;
            }

            let op_str = start_token.kind.as_str();

            if op_str == "++" || op_str == "--" || op_str == "=" {
                break;
            }

            let (l_bp, r_bp) = self.infix_binding_power(op_str);

            if l_bp == 0 || l_bp < min_bp {
                break;
            }

            self.advance();

            if op_str == "(" {
                lhs = self.parse_call(lhs);
                continue;
            }

            if self.current_token().is_terminator() {
                return lhs;
            }

            let rhs = self.parse_expr(r_bp);

            lhs = self.ast.add(
                Node::BINARY(self.token_to_binary_op(op_str, lhs, rhs)),
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
        print_head!(self, "- [0] parse_type [0] -");
        let current_token = &self.tokens[self.at];

        let mut node_type = "";
        let node_id = match &current_token.kind {
            Tag::IDENT(s) | Tag::KEYWORD(s) => {
                node_type = s;
                self.advance();
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

                    // hot fix
                    if returns.len() > 0 {
                        self.at -= 1;
                    }
                    print_head!(self, "- [1] parse_type [1] -");

                    let signature = Node::FUNCSIGNATURE { params, returns };
                    self.ast.add(signature, self.tokens[self.at - 1].span)
                }
                "[" => {
                    node_type = "array";
                    self.advance();
                    let size = match &self.tokens[self.at].kind {
                        Tag::INTEGER(i) => i.parse::<usize>().unwrap(),
                        _ => token_panic!(
                            &self.tokens[self.at],
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
                        _ => token_panic!(&self.tokens[self.at], self.src, "Invalid array type.",),
                    };
                    self.advance();

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
        if self.current_str() != ")" {
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

        print_head!(self, "- [0] parse_identifier_list [0] -");

        let mut idents = Vec::new();
        if self.current_str() == ends_to_symbol {
            return idents;
        }

        loop {
            print_head!(self, "- [1] parse_identifier_list [1] -");

            let current_token = self.current_token();

            if !matches!(current_token.kind, Tag::IDENT(_)) {
                token_panic!(
                    current_token,
                    self.src,
                    "Expected 'identifier', got '{:?}'",
                    current_token.kind.as_str()
                );
            }

            idents.push(self.ast.add(
                Node::IDENTIFIER(current_token.kind.as_str().to_string()),
                current_token.span,
            ));

            print_head!(self, "-[2] parse_identifier_list [2] -");
            self.advance();
            print_head!(self, "- [3] parse_identifier_list [3] -");

            if self.current_str() == ends_to_symbol {
                break;
            }

            self.expect_str(",");
            self.skip_newlines_and_comments();
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
        if self.current_str() == ends_to_symbol {
            print_head!(self, "- [0] parse_variable_list [0] -");
            self.advance();
            print_head!(self, "- [1] parse_variable_list [1] -");
            return variables;
        }

        self.skip_newlines_and_comments();

        loop {
            let current_token = self.current_token();

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
            let var_name = current_token.kind.as_str().to_string();
            let var_span = current_token.span;

            print_head!(self, "- [4] parse_variable_list [4] -");
            self.advance();
            print_head!(self, "- [5] parse_variable_list [5] -");

            let (_, var_type) = self.parse_type();

            let node_id = self.ast.add(
                Node::DECLARATION {
                    name: var_name,
                    kind: var_type,
                    public: false,
                    constant: false,
                },
                var_span,
            );
            variables.push(node_id);

            if self.current_str() == ends_to_symbol {
                break;
            }

            print_head!(self, "- [6] parse_variable_list [6] -");
            self.expect_str(",");
            print_head!(self, "-[7] parse_variable_list [7] -");

            self.skip_newlines_and_comments();
        }

        self.expect_str(ends_to_symbol);

        variables
    }

    fn parse_expression_list(&mut self) -> Vec<NodeId> {
        let mut expressions = Vec::new();
        loop {
            expressions.push(self.parse_expr(0));
            if self.current_str() == "," {
                self.advance();
                self.skip_newlines_and_comments();
            } else {
                break;
            }
        }
        expressions
    }

    fn parse_code_block(&mut self) -> Option<NodeId> {
        print_head!(self, "- [0] parse_code_block [0] -");

        self.expect_str("{");
        let start_token = &self.tokens[self.at];

        let mut block = vec![];
        loop {
            let mut node_id: Option<NodeId> = None;

            print_head!(self, "- [1] parse_code_block [1] -");

            match &self.current_token().kind {
                Tag::NEWLINE(_) | Tag::COMMENT(_) => self.advance(),
                Tag::IDENT(s) => {
                    node_id = match &self.tokens[self.at + 1].kind {
                        Tag::IDENT(_) => Some(self.parse_declaration_or_assignment()),
                        Tag::SYMBOL(sym) => {
                            if sym.ends_with("=") {
                                Some(self.parse_assignment(s))
                            } else if sym == "[" {
                                Some(self.parse_declaration_or_assignment())
                            } else {
                                print_head!(self, "-[2] parse_code_block [2] -");
                                self.advance();
                                self.expect_str("(");
                                print_head!(self, "- [3] parse_code_block [3] -");

                                let func_node_id = self
                                    .ast
                                    .add(Node::IDENTIFIER(s.to_string()), start_token.span);

                                Some(self.parse_call(func_node_id))
                            }
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
                    "match" => node_id = self.parse_match(start_token),
                    "catch" => node_id = self.parse_catch(start_token),
                    "while" => node_id = self.parse_while(start_token),
                    "for" => node_id = self.parse_for(start_token),
                    "continue" => {
                        node_id = Some(self.ast.add(Node::CONTINUE, self.current_token().span));
                        self.advance();
                    }
                    "break" => {
                        node_id = Some(self.ast.add(Node::BREAK, self.current_token().span));
                        self.advance();
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

            print_head!(self, "- [4] parse_code_block[4] -");
            if matches!(node_id, Some(_)) {
                block.push(node_id?);
            }
        }

        print_head!(self, "- [5] parse_code_block [5] -");
        self.expect_str("}");
        print_head!(self, "- [6] parse_code_block [6] -");

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
        self.expect_str("if");

        print_head!(self, "- [0] parse_if [0] -");
        let condition = self.parse_expr(0);
        let body = self.parse_code_block();
        print_head!(self, "- [1] parse_if [1] -");

        Some(self.ast.add(Node::IF { condition, body }, start_token.span))
    }

    fn parse_else(&mut self, start_token: &Token) -> Option<NodeId> {
        self.expect_str("else");

        print_head!(self, "- [0] parse_else [0] -");
        let condition = match self.current_str() {
            "{" => None,
            _ => Some(self.parse_expr(0)),
        };
        let body = self.parse_code_block();
        print_head!(self, "- [1] parse_else [1] -");

        Some(
            self.ast
                .add(Node::ELSE { condition, body }, start_token.span),
        )
    }

    fn parse_while(&mut self, start_token: &Token) -> Option<NodeId> {
        self.expect_str("while");

        print_head!(self, "- [0] parse_while [0] -");
        let condition = self.parse_expr(0);
        let body = self.parse_code_block();
        print_head!(self, "-[1] parse_while [1] -");

        Some(
            self.ast
                .add(Node::WHILE { condition, body }, start_token.span),
        )
    }

    fn parse_for(&mut self, start_token: &Token) -> Option<NodeId> {
        self.expect_str("for");

        print_head!(self, "- [0] parse_for[0] -");
        if !matches!(self.current_token().kind, Tag::IDENT(_)) {
            token_panic!(
                self.current_token(),
                self.src,
                "Expected Index 'identifier' variable name, got '{:?}'",
                self.current_str()
            );
        }

        let index = self.ast.add(
            Node::IDENTIFIER(self.current_str().to_string()),
            self.current_token().span,
        );
        self.advance();
        print_head!(self, "- [1] parse_for [1] -");

        self.expect_str(",");
        print_head!(self, "- [2] parse_for [2] -");

        let value = self.ast.add(
            Node::IDENTIFIER(self.current_str().to_string()),
            self.current_token().span,
        );
        self.advance();
        print_head!(self, "- [3] parse_for [3] -");

        self.expect_str("in");

        let range = match &self.current_token().kind {
            Tag::SYMBOL(_) => self.parse_array(),
            Tag::IDENT(s) => {
                let declaration = match self.declaration_context.get(s) {
                    Some(decl) => decl.clone(),
                    _ => token_panic!(
                        self.current_token(),
                        self.src,
                        "Trying to use a variable '{}' as for's range, that is not declared before.",
                        s
                    ),
                };
                self.advance();

                declaration.node
            }
            _ => token_panic!(
                self.current_token(),
                self.src,
                "Expected Index 'identifier' variable name or an array, got '{:?}'",
                self.current_str()
            ),
        };
        print_head!(self, "- [4] parse_for [4] -");

        let body = self.parse_code_block();
        print_head!(self, "- [5] parse_for [5] -");

        Some(self.ast.add(
            Node::FOR {
                index,
                value,
                range,
                body,
            },
            start_token.span,
        ))
    }

    fn parse_case_blocks(&mut self, is_catch: bool) -> Vec<NodeId> {
        self.expect_str("{");
        print_head!(self, "- [1] parse_case_blocks [01] -");

        let mut case_nodes = vec![];
        loop {
            let mut matching_values_nodes = vec![];

            self.skip_newlines_and_comments();

            // parse cases values
            let mut is_default_case_already_meet = false;
            loop {
                let start_span = self.current_token().span;

                let matching_value = if is_catch {
                    match &self.current_token().kind {
                        Tag::IDENT(s) => self.ast.add(Node::IDENTIFIER(s.to_string()), start_span),
                        Tag::SYMBOL(s) => {
                            if s != "*" {
                                token_panic!(
                                    self.current_token(),
                                    self.src,
                                    "Unexpected identifier '{}'! The only identifier allowed as catch case is '*' for marking the default.",
                                    self.current_str(),
                                );
                            }

                            if is_default_case_already_meet {
                                token_panic!(
                                    self.current_token(),
                                    self.src,
                                    "Match can only have a single default case."
                                );
                            }
                            is_default_case_already_meet = true;

                            self.ast.add(Node::IDENTIFIER(s.to_string()), start_span)
                        }
                        _ => token_panic!(
                            self.current_token(),
                            self.src,
                            "Invalid catch case value type '{:?}'.",
                            self.current_token().kind
                        ),
                    }
                } else {
                    match &self.current_token().kind {
                        Tag::STRING(s) => self.ast.add(Node::STR(s.to_string()), start_span),
                        Tag::INTEGER(i) => self
                            .ast
                            .add(Node::I32(i.parse::<i32>().unwrap()), start_span),
                        Tag::FLOAT(f) => self
                            .ast
                            .add(Node::F32(f.parse::<f32>().unwrap()), start_span),
                        Tag::BOOL(b) => self.ast.add(Node::BOOL(b == &"true"), start_span),
                        Tag::CHARACTER(c) => self
                            .ast
                            .add(Node::CHAR(c.chars().nth(0).unwrap()), start_span),
                        Tag::SYMBOL(s) => {
                            if s != "*" {
                                token_panic!(
                                    self.current_token(),
                                    self.src,
                                    "Unexpected identifier '{}'! The only identifier allowed as match case is '*' for marking the default.",
                                    self.current_str(),
                                );
                            }

                            if is_default_case_already_meet {
                                token_panic!(
                                    self.current_token(),
                                    self.src,
                                    "Match can only have a single default case."
                                );
                            }
                            is_default_case_already_meet = true;

                            self.ast.add(Node::IDENTIFIER(s.to_string()), start_span)
                        }
                        _ => token_panic!(
                            self.current_token(),
                            self.src,
                            "Invalid match case value type '{:?}'.",
                            self.current_token().kind
                        ),
                    }
                };
                matching_values_nodes.push(matching_value);

                print_head!(self, "- [2] parse_case_blocks [2] -");
                self.advance();
                if self.current_str() == "{" {
                    break;
                }
                print_head!(self, "- [3] parse_case_blocks [3] -");

                self.expect_str(",");
            }

            let case_body = self.parse_code_block();
            case_nodes.push(self.ast.add(
                Node::CASE {
                    matching_values: matching_values_nodes,
                    body: case_body,
                },
                self.current_token().span,
            ));

            print_head!(self, "- [4] parse_case_blocks [4] -");
            self.expect_str(",");

            print_head!(self, "- [5] parse_case_blocks [5] -");
            self.skip_newlines_and_comments();

            print_head!(self, "- [6] parse_case_blocks [6] -");
            if self.current_str() == "}" {
                break;
            }
        }

        self.expect_str("}");
        print_head!(self, "- [7] parse_case_blocks[7] -");

        case_nodes
    }

    fn parse_match(&mut self, start_token: &Token) -> Option<NodeId> {
        self.expect_str("match");

        print_head!(self, "- [0] parse_match [0] -");
        let expr = self.parse_expr(0);

        let case_nodes = self.parse_case_blocks(false);

        Some(self.ast.add(
            Node::MATCH {
                expression: expr,
                body: case_nodes,
            },
            start_token.span,
        ))
    }

    fn parse_catch(&mut self, start_token: &Token) -> Option<NodeId> {
        if matches!(self.tokens[self.at - 1].kind, Tag::NEWLINE(_)) {
            token_panic!(
                self.current_token(),
                self.src,
                "Catch must be in same line as the function that traps.",
            )
        }

        self.expect_str("catch");

        print_head!(self, "- [0] parse_catch [0] -");
        let case_nodes = self.parse_case_blocks(true);

        Some(
            self.ast
                .add(Node::CATCH { body: case_nodes }, start_token.span),
        )
    }

    fn parse_array(&mut self) -> NodeId {
        self.expect_str("[");

        let kind = match &self.current_token().kind {
            Tag::IDENT(i) => self
                .ast
                .add(Node::IDENTIFIER(i.to_string()), self.current_token().span),
            _ => token_panic!(self.current_token(), self.src, "Expected array type."),
        };
        self.advance();

        self.expect_str(":");
        let value = self.parse_expression_list();

        self.expect_str("]");

        self.ast
            .add(Node::ARRAYVALUE { kind, value }, self.current_token().span)
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