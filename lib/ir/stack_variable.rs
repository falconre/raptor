use crate::ir::*;
use serde::{Deserialize, Serialize};
use std::fmt;

/// A stack variable is a variable at a set location on the stack
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct StackVariable {
    offset: isize,
    bits: usize,
    ssa: Option<usize>,
}

impl StackVariable {
    pub fn new(offset: isize, bits: usize) -> StackVariable {
        StackVariable {
            offset,
            bits,
            ssa: None,
        }
    }

    pub fn offset(&self) -> isize {
        self.offset
    }
    pub fn bits(&self) -> usize {
        self.bits
    }
    pub fn ssa(&self) -> Option<usize> {
        self.ssa
    }

    pub fn set_ssa(&mut self, ssa: Option<usize>) {
        self.ssa = ssa;
    }

    pub fn name(&self) -> String {
        let ssa = match self.ssa() {
            Some(ssa) => format!(".{}", ssa),
            None => String::from(""),
        };

        if self.offset() < 0 {
            format!("var_0x{:X}{}", self.offset() * -1, ssa)
        } else {
            format!("arg_0x{:X}{}", self.offset(), ssa)
        }
    }
}

impl Into<Variable> for StackVariable {
    fn into(self) -> Variable {
        Variable::StackVariable(self)
    }
}

impl<V: Value> Into<Expression<V>> for StackVariable {
    fn into(self) -> Expression<V> {
        Expression::LValue(Box::new(LValue::Variable(self.into())))
    }
}

impl fmt::Display for StackVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.name(), self.bits())
    }
}
