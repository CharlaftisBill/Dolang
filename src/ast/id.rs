use serde::Serialize;


#[derive(Serialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub(crate) usize);

impl NodeId {
    pub(crate) fn from_index(index: usize) -> Self {
        NodeId(index as usize)
    }

    pub(crate) fn to_index(&self) -> usize {
       self.0
    }
}