use core::panic;
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    vec,
};

use crate::{
    analysis::do_types::DoTypes,
    ast::{Ast, BinaryOp, Node, NodeId},
};

#[derive(Debug, Clone)]
struct VariableContext {
    kind: DoTypes,
    kind_len: Vec<usize>,
    public: bool,
    is_immutable: bool,
}

impl VariableContext {
    pub fn new(kind: DoTypes, kind_len: Vec<usize>, public: bool, is_immutable: bool) -> Self {
        VariableContext {
            kind,
            kind_len,
            public,
            is_immutable,
        }
    }
}

pub struct TypeChecker<'a> {
    ast: &'a Ast,
    calc_type_map: HashMap<NodeId, DoTypes>,
    declaration_context: HashMap<String, VariableContext>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self {
            ast,
            calc_type_map: HashMap::new(),
            declaration_context: HashMap::new(),
        }
    }

    fn node_id_to_node(&self, node_id: &NodeId) -> &'a Node {
        &self.ast.nodes[node_id.to_index()]
    }

    fn cache_kind_of_node_id(&mut self, node_id: NodeId, kind: DoTypes) {
        println!(
            "       ---> Type '{:?}' CACHED for index '{:?}'",
            kind, node_id
        );
        self.calc_type_map.insert(node_id, kind);
    }

    pub fn scan(&mut self) {
        println!("--------");

        let mut fn_signature_stack = vec![];
        let mut fn_return_nodes = vec![];
        for (index, node) in self.ast.nodes.iter().enumerate() {
            println!(" {index}  ---> {:?}", node);

            match node {
                Node::Declaration {
                    name,
                    kind,
                    public,
                    constant,
                } => {
                    let entry_kind: DoTypes = self.get_kind_from_node_id(&kind);
                    let entry_kind_len;

                    let node = self.node_id_to_node(kind).clone();
                    match node {
                        Node::ArrayKind { kind: _, sizes } => {
                            entry_kind_len = sizes.to_vec();
                        }
                        _ => {
                            entry_kind_len = vec![1];
                        }
                    }

                    self.cache_kind_of_node_id(NodeId(index), entry_kind.clone());

                    let entry =
                        VariableContext::new(entry_kind, entry_kind_len, *public, *constant);

                    self.declaration_context.insert(name.to_string(), entry);
                }

                Node::Assignment {
                    operator: _,
                    declaration,
                    value,
                } => {
                    // let dec_name = match self.node_id_to_node(declaration) {
                    //     Node::Declaration { name, .. } => name,
                    //     _ => {
                    //         unreachable!()
                    //     }
                    // };

                    // let decl_context = self.declaration_context.get(dec_name).unwrap();
                    // let value_node = self.node_id_to_node(value);

                    // // ArrayValue type
                    // if let Node::ArrayValue { kind, value } = value_node {
                    //     self.type_check_array_value(decl_context, kind, value);
                    //     continue;
                    // }

                    let decl_kind = self.get_kind_from_node_id(declaration);
                    let value_kind = self.get_kind_from_node_id(value);

                    // comparing types for function
                    let are_matching = match (decl_kind.clone(), value_kind.clone()) {
                        (DoTypes::Closure { returns, .. }, DoTypes::Tuple(tuple_types)) => {
                            fn_signature_stack.pop();
                            returns == tuple_types
                        }
                        _ => false,
                    };

                    if decl_kind != value_kind && !are_matching {
                        panic!(
                            "\n > Cannot assign '{:?}' value to '{:?}' variable",
                            value_kind, decl_kind
                        )
                    }

                    self.cache_kind_of_node_id(NodeId(index), decl_kind.clone());
                }

                Node::FuncSignature { params, returns } => {
                    let mut param_kinds = vec![];
                    for par in params {
                        param_kinds.push(self.get_kind_from_node_id(par));
                    }

                    let mut ret_kinds = vec![];
                    for ret in returns {
                        ret_kinds.push(self.get_kind_from_node_id(ret));
                    }

                    self.cache_kind_of_node_id(
                        NodeId(index),
                        DoTypes::Closure {
                            params: param_kinds,
                            returns: ret_kinds,
                            errors: Vec::new(),
                        },
                    );

                    fn_signature_stack.push(NodeId(index));
                }
                Node::Success { return_values } => {
                    if fn_signature_stack.len() == 0 {
                        panic!("Cannot have 'success' out of a function block.");
                    }

                    let current_fn_node_id = &fn_signature_stack.pop().unwrap();
                    let fn_type = self.get_kind_from_node_id(current_fn_node_id).clone();

                    let mut success_type = vec![];

                    if let DoTypes::Closure {
                        params: _,
                        returns,
                        errors: _,
                    } = fn_type
                    {
                        for success_ret_val_node_id in return_values {
                            let success_ret_val: DoTypes =
                                self.get_kind_from_node_id(success_ret_val_node_id);

                            for fn_sig_ret_val in &returns {
                                if success_ret_val != *fn_sig_ret_val {
                                    panic!(
                                        "'success' return type is not matching the one of 'Function Signature' ('{}' != '{}').",
                                        success_ret_val.as_string(),
                                        fn_sig_ret_val.as_string(),
                                    )
                                }
                            }

                            if return_values.len() > returns.len() {
                                panic!(
                                    "'success' returns more values than the number described in 'Function Signature' ('{}' != '{}').",
                                    return_values.len(),
                                    returns.len(),
                                )
                            }

                            if return_values.len() < returns.len() {
                                panic!(
                                    "'success' returns less values than the number described in 'Function Signature' ('{}' != '{}').",
                                    return_values.len(),
                                    returns.len(),
                                )
                            }
                            success_type.push(success_ret_val);
                        }
                    } else {
                        unreachable!()
                    }

                    self.cache_kind_of_node_id(NodeId(index), DoTypes::Tuple(success_type));

                    fn_return_nodes.push(NodeId(index));
                }
                Node::Failure { reason, return_values } =>{
                    todo!("That's the next step!")
                },
                Node::Block(block) => {
                    for block_node_id in block {
                        for return_node_id in &fn_return_nodes {
                            if block_node_id == return_node_id {
                                let kind = self.get_kind_from_node_id(block_node_id);
                                self.cache_kind_of_node_id(NodeId(index), kind);
                            }
                        }
                    }
                }

                Node::Identifier(ident) => {
                    if let Some(vc) = self.declaration_context.get(ident) {
                        self.cache_kind_of_node_id(NodeId(index), vc.kind.clone());
                    }
                }

                Node::ValueBool(_) => self.cache_kind_of_node_id(NodeId(index), DoTypes::Bool),
                Node::ValueChar(_) => self.cache_kind_of_node_id(NodeId(index), DoTypes::Char),
                Node::ValueFlt(_) => self.cache_kind_of_node_id(NodeId(index), DoTypes::F32),
                Node::ValueInt(_) => self.cache_kind_of_node_id(NodeId(index), DoTypes::I32),
                Node::ValueStr(_) => self.cache_kind_of_node_id(NodeId(index), DoTypes::Str),

                Node::BinaryOp(..) => {
                    let kind = self.get_kind_from_node(node);
                    self.cache_kind_of_node_id(NodeId(index), kind);
                }

                _ => println!("       ---> Not yet implemented {:?}", node),
            }
        }
    }

    fn type_check_array_value(
        &self,
        decl_context: &VariableContext,
        kind: &NodeId,
        value: &NodeId,
    ) {
        let mut sizes: Vec<usize> = vec![];

        let mut node = self.node_id_to_node(value);
        while let Node::ExpressionList { values } = node {
            sizes.push(values.len());
            node = &self.node_id_to_node(&values[0]);
        }

        println!("      ---> len: {:?}", sizes,);

        if !self.compare_do_to_node_type(&decl_context.kind, self.node_id_to_node(kind))
            && decl_context.kind_len != sizes
        {
            panic!(
                "ArrayValue: Cannot assign '{}' value to '{}' variable",
                self.get_kind_as_str(value),
                decl_context.kind.as_string()
            );
        }
    }

    // Helpers
    fn get_kind_as_str(&self, index: &NodeId) -> String {
        match &self.node_id_to_node(index) {
            Node::Identifier(ident) => ident.to_string(),

            Node::ValueBool(_) => "bool".to_string(),
            Node::ValueChar(_) => "char".to_string(),
            Node::ValueFlt(_) => "f32".to_string(),
            Node::ValueInt(_) => "i32".to_string(),
            Node::ValueStr(_) => "str".to_string(),

            Node::ArrayKind { kind, sizes } => self.get_array_kind_as_str(kind, sizes),
            Node::ArrayValue { kind, value } => self.get_array_value_as_str(kind, value),

            Node::FuncSignature { params, returns } => {
                let mut signature = String::with_capacity(100);

                signature.push_str("[FUNCTION params: (");

                for (index, param) in params.iter().enumerate() {
                    if index != 0 {
                        signature.push_str(" ,");
                    }
                    write!(&mut signature, "{}", self.get_kind_as_str(&param)).unwrap();
                }

                signature.push_str(") returns: (");
                for (index, ret) in returns.iter().enumerate() {
                    if index != 0 {
                        signature.push_str(" ,");
                    }
                    write!(&mut signature, "{}", self.get_kind_as_str(&ret)).unwrap();
                }
                signature.push_str(")]");

                signature
            }
            Node::Block(stmts) => {
                let mut types: HashSet<String> = HashSet::new();
                let mut errors: HashSet<String> = HashSet::new();

                for stmn in stmts {
                    match self.node_id_to_node(stmn) {
                        Node::Success { return_values } => {
                            for ret in return_values {
                                types.insert(self.get_kind_as_str(ret));
                            }
                        }
                        Node::Failure {
                            reason,
                            return_values,
                        } => {
                            errors.insert(reason.to_string());
                            for ret in return_values {
                                types.insert(self.get_kind_as_str(ret));
                            }
                        }
                        _ => continue,
                    }
                }

                let mut block_type = String::with_capacity(100);
                block_type.push_str("[BLOCK returns: (");

                for (index, typ) in types.iter().enumerate() {
                    if index != 0 {
                        block_type.push_str(" ,");
                    }
                    block_type.push_str(typ)
                }

                block_type.push_str("), causes: (");

                for (index, ret) in errors.iter().enumerate() {
                    if index != 0 {
                        block_type.push_str(" ,");
                    }
                    block_type.push_str(ret)
                }
                block_type.push_str(")]");

                block_type
            }

            _ => panic!(
                "\n > Expected the kind to point to a typed Value, but instead points to '{:?}'.\n",
                self.node_id_to_node(index)
            ),
        }
    }

    fn get_array_kind_as_str(&self, kind: &NodeId, sizes: &Vec<usize>) -> String {
        let mut array_kinds_as_str = String::new();
        for size in sizes {
            array_kinds_as_str.push_str(&size.to_string());
            array_kinds_as_str.push_str("x");
        }
        array_kinds_as_str.push_str(&self.get_kind_as_str(kind));

        array_kinds_as_str
    }

    fn get_array_value_as_str(&self, kind: &NodeId, value: &NodeId) -> String {
        let mut array_kinds_as_str = String::new();
        let mut node = self.node_id_to_node(value);

        while let Node::ExpressionList { values } = node {
            array_kinds_as_str.push_str(&values.len().to_string());
            array_kinds_as_str.push_str("x");

            node = &self.node_id_to_node(&values[0]);
        }
        array_kinds_as_str.push_str(&self.get_kind_as_str(kind));

        array_kinds_as_str
    }

    fn compare_do_to_node_type(&self, do_type: &DoTypes, node_type: &Node) -> bool {
        println!("\nComparing '{:?}' to '{:?}'", do_type, node_type);

        match do_type {
            DoTypes::Bool => matches!(node_type, Node::ValueBool(_)),
            DoTypes::Char => matches!(node_type, Node::ValueChar(_)),
            DoTypes::I8 | DoTypes::I16 | DoTypes::I32 | DoTypes::I64 => {
                matches!(node_type, Node::ValueInt(_))
            }
            DoTypes::Size | DoTypes::U8 | DoTypes::U16 | DoTypes::U32 | DoTypes::U64 => {
                matches!(node_type, Node::ValueInt(_))
            }
            DoTypes::F32 | DoTypes::F64 => matches!(node_type, Node::ValueFlt(_)),
            DoTypes::Str => matches!(node_type, Node::ValueStr(_)),

            DoTypes::Closure {
                params, returns, ..
            } => {
                if let Node::FuncSignature {
                    params: sig_params,
                    returns: sig_returns,
                } = node_type
                {
                    if params.len() != sig_params.len() {
                        return false;
                    }

                    for (index, param) in params.iter().enumerate() {
                        if self.compare_do_to_node_type(
                            param,
                            self.node_id_to_node(&sig_params[index]),
                        ) {
                            return false;
                        }
                    }

                    if returns.len() != sig_returns.len() {
                        return false;
                    }

                    for (index, ret) in returns.iter().enumerate() {
                        if self
                            .compare_do_to_node_type(ret, self.node_id_to_node(&sig_params[index]))
                        {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }

            DoTypes::UserDefinedType => todo!("Assignment of Structs not yet implemented!"),

            DoTypes::Tuple(_) => todo!("Tuples are not yet comparable!"),

            DoTypes::Unknown => panic!("'DoType' is yet unknown"),
        }
    }

    fn compare_do_to_node_id(&mut self, do_type: &DoTypes, node_id: &NodeId) -> bool {
        self.compare_do_to_node_type(do_type, self.node_id_to_node(node_id))
    }

    fn get_kind_from_node(&mut self, kind_node: &Node) -> DoTypes {
        // println!("\n get_kind_from_node {:?}", kind_node);

        match kind_node {
            Node::Identifier(kind_str) => {
                let mut ident_actual_type = self::DoTypes::match_from_string(kind_str);

                if matches!(ident_actual_type, DoTypes::UserDefinedType) {
                    ident_actual_type = match self.declaration_context.get(kind_str) {
                        Some(v) => v.kind.clone(),
                        None => ident_actual_type,
                    }
                }

                ident_actual_type
            }
            Node::Declaration {
                name: _,
                kind,
                public: _,
                constant: _,
            } => self.get_kind_from_node(self.node_id_to_node(kind)),
            Node::FuncSignature { params, returns } => {
                let mut ret_params = vec![];
                let mut ret_returns = vec![];

                for param in params {
                    ret_params.push(self.get_kind_from_node(self.node_id_to_node(param)));
                }

                for ret in returns {
                    ret_returns.push(self.get_kind_from_node(self.node_id_to_node(ret)));
                }

                DoTypes::Closure {
                    params: ret_params,
                    returns: ret_returns,
                    errors: vec![],
                }
            }

            Node::ValueBool(_) => DoTypes::Bool,
            Node::ValueChar(_) => DoTypes::Char,
            Node::ValueInt(_) => DoTypes::I32,
            Node::ValueFlt(_) => DoTypes::F32,
            Node::ValueStr(_) => DoTypes::Str,

            Node::BinaryOp(binary_op) => match binary_op {
                BinaryOp::ADD(lhs, rhs)
                | BinaryOp::SUB(lhs, rhs)
                | BinaryOp::MUL(lhs, rhs)
                | BinaryOp::DIV(lhs, rhs)
                | BinaryOp::REM(lhs, rhs) => {
                    let lhs_kind = self.get_kind_from_node_id(lhs);
                    let rhs_kind = self.get_kind_from_node_id(rhs);

                    if lhs_kind != rhs_kind {
                        panic!(
                            "Not compatible types. The 'Left Hand Side' (:{:?}) type is '{}', but the 'Right Hand Side' (:{:?}) type is '{}'",
                            self.node_id_to_node(lhs),
                            lhs_kind.as_string(),
                            self.node_id_to_node(rhs),
                            rhs_kind.as_string()
                        )
                    }

                    rhs_kind
                }

                BinaryOp::LT(lhs, rhs)
                | BinaryOp::GT(lhs, rhs)
                | BinaryOp::LTE(lhs, rhs)
                | BinaryOp::GTE(lhs, rhs) => {
                    let lhs_kind = self.get_kind_from_node_id(lhs);
                    let rhs_kind = self.get_kind_from_node_id(rhs);

                    if !lhs_kind.is_numeric() || !rhs_kind.is_numeric() {
                        panic!("Comparisons is supported only for operands of numeric type");
                    }

                    DoTypes::Bool
                }

                BinaryOp::EQ(lhs, rhs) => {
                    let lhs_kind = self.get_kind_from_node_id(lhs);
                    let rhs_kind = self.get_kind_from_node_id(rhs);

                    if lhs_kind != rhs_kind {
                        panic!(
                            "Not compatible types. The 'Left Hand Side' (:{:?}) type is '{}', but the 'Right Hand Side' (:{:?}) type is '{}'",
                            self.node_id_to_node(lhs),
                            lhs_kind.as_string(),
                            self.node_id_to_node(rhs),
                            rhs_kind.as_string()
                        )
                    }

                    DoTypes::Bool
                }

                _ => unimplemented!(),
            },
            _ => panic!(
                "'get_kind_from_node' expects a 'Node::Identifier', but got '{:?}'.",
                kind_node
            ),
        }
    }

    fn get_kind_from_node_id(&mut self, kind_node_id: &NodeId) -> DoTypes {
        let kind = match self.calc_type_map.get(kind_node_id) {
            Some(kind) => kind.clone(),
            None => {
                let kind = &self.get_kind_from_node(self.node_id_to_node(kind_node_id));
                self.cache_kind_of_node_id(*kind_node_id, kind.clone());

                kind.clone()
            }
        };
        kind
    }
}
