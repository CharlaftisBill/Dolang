use std::fmt::Write;

use crate::ast::{Node, NodeId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    Closure {
        params: Vec<DoTypes>,
        returns: Vec<DoTypes>,
        errors: Vec<String>,
    },

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

            DoTypes::Closure {
                params,
                returns,
                errors,
            } => {
                let mut signature = String::with_capacity(100);

                signature.push_str("[FUNCTION params: (");
                for (index, param) in params.iter().enumerate() {
                    if index != 0 {
                        signature.push_str(" ,");
                    }
                    write!(&mut signature, "{}", param.as_string()).unwrap();
                }

                signature.push_str("), returns: (");
                for (index, ret) in returns.iter().enumerate() {
                    if index != 0 {
                        signature.push_str(" ,");
                    }
                    write!(&mut signature, "{}", ret.as_string()).unwrap();
                }

                signature.push_str("), causes: (");
                for (index, err) in errors.iter().enumerate() {
                    if index != 0 {
                        signature.push_str(" ,");
                    }
                    signature.push_str(err);
                }
                signature.push_str(")]");

                signature
            }

            DoTypes::UserDefinedType => "udt".to_string(),
        }
    }

    pub fn match_from_string(kind_str: &String) -> DoTypes {
        // println!("\n match_from_string {:?}", kind_str);

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
}
