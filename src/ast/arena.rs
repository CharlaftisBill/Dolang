use serde::Serialize;

use super::id::NodeId;
use super::nodes::Node;
use super::span::Span;

#[derive(Serialize, Debug, Default)]
pub struct Ast {
    pub(crate) nodes: Vec<Node>,
    spans: Vec<Span>
}

impl Ast {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, node: Node, span: Span) -> NodeId {
        let id = NodeId::from_index(self.nodes.len());

        println!("      ({:?})      --- Adding: node -> '{:?}', span -> '{:?}'", self.nodes.len(), node, span);
        
        self.nodes.push(node);
        self.spans.push(span);
        id
    }
    
    // pub fn span(&self, id: NodeId) -> Span {
    //     self.spans[id.index()]
    // }

    // pub fn get(&self, id: NodeId) -> &Node {
    //     &self.nodes[id.index()]
    // }

    // pub fn get_mut(&mut self, id: NodeId) -> &mut Node {
    //     &mut self.nodes[id.index()]
    // }
}