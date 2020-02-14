use crate::ir::*;
use falcon::il;
use serde::{Deserialize, Serialize};
use std::fmt;

/// Multiple types of variables
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Variable {
    Scalar(Scalar),
    StackVariable(StackVariable),
}

impl Variable {
    pub fn scalar(&self) -> Option<&Scalar> {
        match self {
            Variable::Scalar(scalar) => Some(scalar),
            _ => None,
        }
    }

    pub fn stack_variable(&self) -> Option<&StackVariable> {
        match self {
            Variable::StackVariable(stack_variable) => Some(stack_variable),
            _ => None,
        }
    }

    pub fn bits(&self) -> usize {
        match self {
            Variable::Scalar(scalar) => scalar.bits(),
            Variable::StackVariable(stack_variable) => stack_variable.bits(),
        }
    }

    pub fn ssa(&self) -> Option<usize> {
        match self {
            Variable::Scalar(scalar) => scalar.ssa(),
            Variable::StackVariable(stack_variable) => stack_variable.ssa(),
        }
    }

    pub fn set_ssa(&mut self, ssa: Option<usize>) {
        match self {
            Variable::Scalar(scalar) => scalar.set_ssa(ssa),
            Variable::StackVariable(stack_variable) => stack_variable.set_ssa(ssa),
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
        Variable::Scalar(scalar.into())
    }
}

impl From<Scalar> for Variable {
    fn from(scalar: Scalar) -> Variable {
        Variable::Scalar(scalar)
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Variable::Scalar(scalar) => scalar.fmt(f),
            Variable::StackVariable(stack_variable) => stack_variable.fmt(f),
        }
    }
}
