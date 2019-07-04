use ir::*;
use std::fmt;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Reference<V: Value> {
    expression: Expression<V>,
    bits: usize,
}

impl<V: Value> Reference<V> {
    pub fn new(expression: Expression<V>, bits: usize) -> Reference<V> {
        Reference {
            expression: expression,
            bits: bits,
        }
    }

    pub fn expression(&self) -> &Expression<V> {
        &self.expression
    }
    pub fn bits(&self) -> usize {
        self.bits
    }
    pub fn into_expression(self) -> Expression<V> {
        self.expression
    }
}

impl<V: Value> Into<Expression<V>> for Reference<V> {
    fn into(self) -> Expression<V> {
        Expression::RValue(Box::new(RValue::Reference(self)))
    }
}

impl<V: Value> fmt::Display for Reference<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "&{}", self.expression())
    }
}
