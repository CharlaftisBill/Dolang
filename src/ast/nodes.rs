use serde::Serialize;
use super::id::NodeId;

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
    I32(i32),
    F32(f32),
    STR(String),
    CHAR(char),
    BOOL(bool),

    ARRAYKIND {
        kind: NodeId,
        size: usize
    },
    ARRAYVALUE{
        kind: NodeId,
        value: Vec<NodeId>
    },

    IDENTIFIER(String),

    DECLARATION {
        name: String,
        kind: NodeId,
        public: bool,
        constant: bool,
    },
    ASSIGNMENT{
        operator: NodeId,
        declaration: NodeId,
        value: NodeId
    },
    BLOCK(Vec<NodeId>),

    BINARY(BinaryOp),
    UNARY(UnaryOp),

    CALL {
        func: NodeId,
        args: Vec<NodeId>,
    },
    FUNCSIGNATURE {
        params: Vec<NodeId>,
        returns: Vec<NodeId>,
    },

    SUCCESS {
        return_values:  Vec<NodeId>,
    },
    FAILURE {
        reason: String,
        return_values:  Vec<NodeId>,
    },

    IF {
        condition: NodeId,
        body: Option<NodeId>
    },
    ELSE {
        condition: Option<NodeId>,
        body: Option<NodeId>
    },
    FOR {
        index: NodeId,
        value: NodeId,
        range: NodeId,
    },
    WHILE {
        condition: NodeId,
        body: Option<NodeId>
    },
    CONTINUE,
    BREAK,
}
