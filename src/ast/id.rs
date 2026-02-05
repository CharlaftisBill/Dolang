use serde::Serialize;


#[derive(Serialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub(crate) u32);

impl NodeId {
    pub(crate) fn from_index(index: usize) -> Self {
        NodeId(index as u32)
    }

    // pub(crate) fn index(self) -> usize {
    //     self.0 as usize
    // }
}