use core::panic;
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

use crate::{
    analysis::do_types::DoTypes,
    ast::{Ast, Node, NodeId},
};

#[derive(Debug)]
struct VariableContext {
    name: String,
    kind: DoTypes,
    kind_len: Vec<usize>,
    public: bool,
    is_immutable: bool,
}

pub struct TypeChecker<'a> {
    ast: &'a Ast,
    declaration_context: HashMap<usize, VariableContext>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self {
            ast,
            declaration_context: HashMap::new(),
        }
    }

    fn node_id_to_node(&self, node_id: &NodeId) -> &Node {
        &self.ast.nodes[node_id.to_index()]
    }

    pub fn scan(&mut self) {
        println!("");

        for (index, node) in self.ast.nodes.iter().enumerate() {
            println!(" {index}  ---> {:?}", node);

            match node {
                Node::Declaration {
                    name,
                    kind,
                    public,
                    constant,
                } => {
                    let entry_kind: DoTypes;
                    let entry_kind_len;

                    let node = self.node_id_to_node(kind);

                    match node {
                        Node::ArrayKind { kind, sizes } => {
                            entry_kind = self.match_from_node(&self.node_id_to_node(kind));
                            entry_kind_len = sizes.to_vec();
                        }
                        _ => {
                            entry_kind = self.match_from_node(node);
                            entry_kind_len = vec![1];
                        }
                    }

                    let entry = VariableContext {
                        name: name.to_string(),
                        kind: entry_kind,
                        kind_len: entry_kind_len,
                        public: *public,
                        is_immutable: *constant,
                    };

                    println!(
                        " {}                     ---> (Declaration) {:?} - {:?}",
                        index,
                        kind.to_index(),
                        entry
                    );

                    self.declaration_context.insert(index, entry);
                }

                Node::Assignment {
                    operator: _,
                    declaration,
                    value,
                } => {
                    // println!(
                    //     " {index}      ---> (Assignment) OP: {:?} - Decl: {:?} - Val: {:?}",
                    //     operator, declaration, value
                    // );

                    let decl_context = self
                        .declaration_context
                        .get(&declaration.to_index())
                        .unwrap();

                    let value_node = self.node_id_to_node(value);

                    // ArrayValue type
                    if let Node::ArrayValue { kind, value } = value_node {
                        self.type_check_array_value(decl_context, kind, value);
                        continue;
                    } else if let Node::Block(stmts) = value_node {
                        let mut types = vec![];
                        let mut errors: HashSet<String> = HashSet::new();

                        for stmn in stmts {
                            match self.node_id_to_node(stmn) {
                                Node::Success { return_values } => {
                                    for (index, ret) in return_values.iter().enumerate() {
                                        if types.len() == 0 {
                                            types.push(DoTypes::match_from_string(
                                                &self.get_kind_as_str(ret),
                                            ));
                                            continue;
                                        }

                                        if index < types.len() {
                                            if types.get(index)
                                        }
                                    }
                                }
                                Node::Failure {
                                    reason,
                                    return_values,
                                } => {
                                    errors.insert(reason.to_string());
                                    for ret in return_values {
                                        types.insert(DoTypes::match_from_string(
                                            &self.get_kind_as_str(ret),
                                        ));
                                    }
                                }
                                _ => continue,
                            }
                        }

                        if decl_context.kind == types.{

                        }
                    }

                    if !self.compare_do_to_node_type(&decl_context.kind, value_node) {
                        panic!(
                            "Cannot assign '{}' value to '{}' variable",
                            self.get_kind_as_str(value),
                            decl_context.kind.as_string()
                        )
                    }
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
        // println!(
        //     "      ---> ArrayValue: kind: {:?}, value: {:?}",
        //     self.node_id_to_node(kind),
        //     self.node_id_to_node(value),
        // );

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
        println!("\nComparing {:?} to {:?}", do_type, node_type);

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
                params,
                returns,
                errors,
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
        }
    }

    fn match_from_node(&self, kind_node: &Node) -> DoTypes {
        // println!("\n match_from_node {:?}", kind_node);

        match kind_node {
            Node::Identifier(kind_str) => self::DoTypes::match_from_string(kind_str),
            Node::Declaration {
                name: _,
                kind,
                public: _,
                constant: _,
            } => self.match_from_node(self.node_id_to_node(kind)),
            Node::FuncSignature { params, returns } => {
                let mut ret_params = vec![];
                let mut ret_returns = vec![];

                for param in params {
                    ret_params.push(self.match_from_node(self.node_id_to_node(param)));
                }

                for ret in returns {
                    ret_returns.push(self.match_from_node(self.node_id_to_node(ret)));
                }

                DoTypes::Closure {
                    params: ret_params,
                    returns: ret_returns,
                    errors: vec![],
                }
            }
            _ => panic!(
                "'match_from_node' expects a 'Node::Identifier', but got '{:?}'.",
                kind_node
            ),
        }
    }
}
