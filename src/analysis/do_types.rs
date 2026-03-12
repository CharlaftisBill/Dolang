use crate::ast::{Node, NodeId};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DoTypes {
    Bool,
    Char,

    I8,
    I16,
    I32,
    I64,

    Size,
    U8,
    U16,
    U32,
    U64,

    F32,
    F64,

    Str,

    UserDefinedType,
}

impl DoTypes {
    pub fn as_string(&self) -> String {
        match self {
            DoTypes::Bool => "bool".to_string(),
            DoTypes::Char => "char".to_string(),

            DoTypes::I8 => "i8".to_string(),
            DoTypes::I16 => "i16".to_string(),
            DoTypes::I32 => "i32".to_string(),
            DoTypes::I64 => "i64".to_string(),

            DoTypes::Size => "size".to_string(),
            DoTypes::U8 => "u8".to_string(),
            DoTypes::U16 => "u16".to_string(),
            DoTypes::U32 => "u32".to_string(),
            DoTypes::U64 => "u64".to_string(),

            DoTypes::F32 => "f32".to_string(),
            DoTypes::F64 => "f64".to_string(),

            DoTypes::Str => "str".to_string(),

            DoTypes::UserDefinedType => "udt".to_string(),
        }
    }

    pub fn match_from_string(kind_str: &String) -> DoTypes {
        println!("\n match_from_string {:?}", kind_str);

        match kind_str.as_str() {
            "bool" => DoTypes::Bool,
            "char" => DoTypes::Char,

            "i8" => DoTypes::I8,
            "i16" => DoTypes::I16,
            "i32" => DoTypes::I32,
            "i64" => DoTypes::I64,

            "size" => DoTypes::Size,
            "u8" => DoTypes::U8,
            "u16" => DoTypes::U16,
            "u32" => DoTypes::U32,
            "u64" => DoTypes::U64,

            "f32" => DoTypes::F32,
            "f64" => DoTypes::F64,

            "str" => DoTypes::Str,

            _ => DoTypes::UserDefinedType,
        }
    }

    pub fn match_from_node(kind_node: &Node) -> DoTypes {
        println!("\n match_from_node {:?}", kind_node);

        match kind_node {
            Node::Identifier(kind_str) => self::DoTypes::match_from_string(kind_str),
            _ => panic!(
                "'match_from_node' expects a 'Node::Identifier', but got '{:?}'.",
                kind_node
            ),
        }
    }

    pub fn compare_with(self, compare_to: &Node) -> bool {
        println!("\nComparing {:?} to {:?}", compare_to, self);

        match self {
            Self::Bool => matches!(compare_to, Node::ValueBool(_)),
            Self::Char => matches!(compare_to, Node::ValueChar(_)),
            Self::I8 | Self::I16 | Self::I32 | Self::I64 => matches!(compare_to, Node::ValueInt(_)),
            Self::Size | Self::U8 | Self::U16 | Self::U32 | Self::U64 => {
                matches!(compare_to, Node::ValueInt(_))
            }
            Self::F32 | Self::F64 => matches!(compare_to, Node::ValueFlt(_)),
            Self::Str => matches!(compare_to, Node::ValueStr(_)),
            Self::UserDefinedType => todo!("Assignment of Structs not yet implemented!"),
        }
    }
}
