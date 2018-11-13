use ir::*;
use std::fmt;


#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct StackPointer {
    offset: isize,
    bits: usize
}

impl StackPointer {
    pub fn new(offset: isize, bits: usize) -> StackPointer {
        StackPointer { offset: offset, bits: bits }
    }

    pub fn offset(&self) -> isize {
        self.offset
    }

    pub fn bits(&self) -> usize {
        self.bits
    }
}


impl<V> Into<Expression<V>> for StackPointer {
    fn into(self) -> Expression<V> {
        Expression::StackPointer(self)
    }
}


impl fmt::Display for StackPointer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "sp@{}", ::signed_hex(self.offset()))
    }
}
