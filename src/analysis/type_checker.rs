use std::collections::HashMap;

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

                    if let Node::ArrayKind { kind, sizes } = self.node_id_to_node(kind) {
                        entry_kind = DoTypes::match_from_node(&self.node_id_to_node(kind));
                        entry_kind_len = sizes.to_vec();
                    } else {
                        entry_kind = DoTypes::match_from_node(&self.node_id_to_node(kind));
                        entry_kind_len = vec![1];
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
                    }

                    if !decl_context.kind.compare_with(value_node) {
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

        if !decl_context.kind.compare_with(self.node_id_to_node(kind))
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
            // Node::FuncSignature { params, returns } => {}
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
}
