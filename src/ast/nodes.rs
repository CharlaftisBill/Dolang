use super::id::NodeId;
use serde::Serialize;
use crate::analysis::do_types::DoTypes;

#[derive(Serialize, Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    NEG(NodeId),    // -x
    POS(NodeId),    // +x
    NOT(NodeId),    // !x
    BITNOT(NodeId), // ^x
}

#[derive(Serialize, Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    // Arithmetic
    ADD(NodeId, NodeId), // +
    SUB(NodeId, NodeId), // -
    MUL(NodeId, NodeId), // *
    DIV(NodeId, NodeId), // /
    REM(NodeId, NodeId), // %

    // Bitwise
    BITAND(NodeId, NodeId), // &
    BITOR(NodeId, NodeId),  // |
    BITXOR(NodeId, NodeId), // ^
    SHL(NodeId, NodeId),    // <<
    SHR(NodeId, NodeId),    // >>

    // Comparison
    EQ(NodeId, NodeId),  // ==
    NEQ(NodeId, NodeId), // !=
    LT(NodeId, NodeId),  // <
    LTE(NodeId, NodeId), // <=
    GT(NodeId, NodeId),  // >
    GTE(NodeId, NodeId), // >=

    // Logical
    LOGICAND(NodeId, NodeId), // &&
    LOGICOR(NodeId, NodeId),  // ||
}

#[derive(Serialize, Debug, Clone)]
#[serde(tag = "kind", content = "data")]
pub enum Node {
    ValueBool(bool),
    ValueChar(char),
    ValueInt(i32),
    ValueFlt(f32),
    ValueStr(String),

    ArrayKind {
        kind: NodeId,
        sizes: Vec<usize>,
    },
    ArrayValue {
        kind: NodeId,
        value: NodeId,
    },
    ArrayReference {
        lhs: NodeId,
        indices: Vec<NodeId>,
    },
    ExpressionList {
        values: Vec<NodeId>,
    },

    Identifier(String),

    Declaration {
        name: String,
        kind: NodeId,
        public: bool,
        constant: bool,
    },
    Assignment {
        operator: NodeId,
        declaration: NodeId,
        value: NodeId,
    },
    Block(Vec<NodeId>),

    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),

    FuncCall {
        func: NodeId,
        args: Vec<NodeId>,
    },
    FuncSignature {
        params: Vec<NodeId>,
        returns: Vec<NodeId>,
    },

    Success {
        return_values: Vec<NodeId>,
    },
    Failure {
        reason: String,
        return_values: Vec<NodeId>,
    },

    If {
        condition: NodeId,
        body: Option<NodeId>,
    },
    Else {
        condition: Option<NodeId>,
        body: Option<NodeId>,
    },
    Match {
        expression: NodeId,
        body: Vec<NodeId>,
    },
    Catch {
        body: Vec<NodeId>,
    },
    Case {
        matching_values: Vec<NodeId>,
        body: Option<NodeId>,
    },
    For {
        index: NodeId,
        value: NodeId,
        range: NodeId,
        body: Option<NodeId>,
    },
    While {
        condition: NodeId,
        body: Option<NodeId>,
    },
    Continue,
    Break,
}
