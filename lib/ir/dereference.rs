use crate::ir::*;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Dereference<V: Value> {
    expression: Expression<V>,
}

impl<V: Value> Dereference<V> {
    pub fn new(expression: Expression<V>) -> Dereference<V> {
        Dereference { expression }
    }

    pub fn bits(&self) -> usize {
        self.expression.bits()
    }
    pub fn expression(&self) -> &Expression<V> {
        &self.expression
    }
    pub fn into_expression(self) -> Expression<V> {
        self.expression
    }
}

impl<V: Value> From<Dereference<V>> for LValue<V> {
    fn from(dereference: Dereference<V>) -> LValue<V> {
        LValue::Dereference(dereference)
    }
}

impl<V: Value> From<Dereference<V>> for Expression<V> {
    fn from(dereference: Dereference<V>) -> Expression<V> {
        Expression::LValue(Box::new(dereference.into()))
    }
}

impl<V: Value> fmt::Display for Dereference<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "*{}", self.expression())
    }
}
