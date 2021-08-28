use crate::ir::*;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum RValue<V: Value> {
    Value(V),
    Reference(Reference<V>),
}

impl<V: Value> RValue<V> {
    pub fn value(&self) -> Option<&V> {
        match self {
            RValue::Value(value) => Some(value),
            _ => None,
        }
    }

    pub fn reference(&self) -> Option<&Reference<V>> {
        match self {
            RValue::Reference(reference) => Some(reference),
            _ => None,
        }
    }

    /// A stack pointer is a `Reference` that wraps a `StackVariable`
    pub fn stack_pointer(&self) -> Option<&StackVariable> {
        self.reference()
            .and_then(|reference| reference.expression().variable())
            .and_then(|variable| variable.stack_variable())
    }

    pub fn bits(&self) -> usize {
        match self {
            RValue::Value(value) => value.bits(),
            RValue::Reference(reference) => reference.bits(),
        }
    }
}

impl<V: Value> From<RValue<V>> for Expression<V> {
    fn from(rvalue: RValue<V>) -> Expression<V> {
        Expression::RValue(Box::new(rvalue))
    }
}

impl From<Constant> for RValue<Constant> {
    fn from(constant: Constant) -> RValue<Constant> {
        RValue::Value(constant)
    }
}

impl<V: Value> fmt::Display for RValue<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RValue::Value(value) => fmt::Display::fmt(&value, f),
            RValue::Reference(reference) => reference.fmt(f),
        }
    }
}
