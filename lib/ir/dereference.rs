use crate::ir::*;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Dereference<V: Value> {
    expression: Expression<V>,
}

impl<V: Value> Dereference<V> {
    pub fn new(expression: Expression<V>) -> Dereference<V> {
        Dereference {
            expression: expression,
        }
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

impl<V: Value> Into<LValue<V>> for Dereference<V> {
    fn into(self) -> LValue<V> {
        LValue::Dereference(self)
    }
}

impl<V: Value> Into<Expression<V>> for Dereference<V> {
    fn into(self) -> Expression<V> {
        Expression::LValue(Box::new(self.into()))
    }
}

impl<V: Value> fmt::Display for Dereference<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "*{}", self.expression())
    }
}
