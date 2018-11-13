use falcon::il;
use ir::*;
use std::fmt;


/// A stack variable is a variable at a set location on the stack
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct StackVariable {
    offset: isize,
    bits: usize
}


impl StackVariable {
    pub fn new(offset: isize, bits: usize) -> StackVariable {
        StackVariable {
            offset: offset,
            bits: bits
        }
    }

    pub fn offset(&self) -> isize { self.offset }
    pub fn bits(&self) -> usize { self.bits }

    pub fn name(&self) -> String {
        if self.offset() < 0 {
            format!("var_0x{:X}", self.offset() * -1)
        }
        else {
            format!("arg_0x{:X}", self.offset())
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


/// Multiple types of variables
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Variable {
    Scalar(il::Scalar),
    StackVariable(StackVariable)
}


impl Variable {
    pub fn scalar(&self) -> Option<&il::Scalar> {
        match self {
            Variable::Scalar(scalar) => Some(scalar),
            _ => None
        }
    }

    pub fn stack_variable(&self) -> Option<&StackVariable> {
        match self {
            Variable::StackVariable(stack_variable) => Some(stack_variable),
            _ => None
        }
    }

    pub fn bits(&self) -> usize {
        match self {
            Variable::Scalar(scalar) => scalar.bits(),
            Variable::StackVariable(stack_variable) => stack_variable.bits()
        }
    }
}


impl<V: Value> Into<Expression<V>> for Variable {
    fn into(self) -> Expression<V> {
        Expression::LValue(Box::new(LValue::Variable(self)))
    }
}


impl From<il::Scalar> for Variable {
    fn from(scalar: il::Scalar) -> Variable {
        Variable::Scalar(scalar)
    }
}


impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Variable::Scalar(scalar) => scalar.fmt(f),
            Variable::StackVariable(stack_variable) => stack_variable.fmt(f)
        }
    }
}