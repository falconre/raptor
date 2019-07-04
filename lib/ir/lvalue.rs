use ir::*;
use std::fmt;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum LValue<V: Value> {
    Variable(Variable),
    Dereference(Dereference<V>),
}

impl<V: Value> LValue<V> {
    pub fn variable(&self) -> Option<&Variable> {
        match self {
            LValue::Variable(variable) => Some(variable),
            _ => None,
        }
    }

    pub fn scalar(&self) -> Option<&Scalar> {
        self.variable().and_then(|variable| variable.scalar())
    }

    pub fn stack_variable(&self) -> Option<&StackVariable> {
        self.variable()
            .and_then(|variable| variable.stack_variable())
    }

    pub fn dereference(&self) -> Option<&Dereference<V>> {
        match self {
            LValue::Dereference(dereference) => Some(dereference),
            _ => None,
        }
    }

    pub fn bits(&self) -> usize {
        match self {
            LValue::Variable(variable) => variable.bits(),
            LValue::Dereference(dereference) => dereference.bits(),
        }
    }
}

impl<V: Value> Into<Expression<V>> for LValue<V> {
    fn into(self) -> Expression<V> {
        Expression::LValue(Box::new(self))
    }
}

impl<V: Value> From<Scalar> for LValue<V> {
    fn from(scalar: Scalar) -> LValue<V> {
        LValue::Variable(scalar.into())
    }
}

impl<V: Value> fmt::Display for LValue<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LValue::Variable(variable) => variable.fmt(f),
            LValue::Dereference(dereference) => dereference.fmt(f),
        }
    }
}
