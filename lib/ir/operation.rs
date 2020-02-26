use crate::ir::*;
use falcon::il;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Operation<V: Value> {
    Assign {
        dst: Variable,
        src: Expression<V>,
    },
    Store {
        index: Expression<V>,
        src: Expression<V>,
    },
    Load {
        dst: Variable,
        index: Expression<V>,
    },
    Branch {
        target: Expression<V>,
    },
    Call(Call<V>),
    Intrinsic(il::Intrinsic),
    Return(Option<Expression<V>>),
    Nop,
}

impl<V: Value> Operation<V> {
    pub fn from_il(operation: &il::Operation) -> Operation<Constant> {
        match operation {
            il::Operation::Assign { dst, src } => Operation::Assign {
                dst: dst.clone().into(),
                src: Expression::from_il(src),
            },
            il::Operation::Store { index, src } => Operation::Store {
                index: Expression::from_il(index),
                src: Expression::from_il(src),
            },
            il::Operation::Load { dst, index } => Operation::Load {
                dst: dst.clone().into(),
                index: Expression::from_il(index),
            },
            il::Operation::Branch { target } => Operation::Branch {
                target: Expression::from_il(target),
            },
            il::Operation::Intrinsic { intrinsic } => Operation::Intrinsic(intrinsic.clone()),
            il::Operation::Nop => Operation::Nop,
        }
    }

    pub fn is_assign(&self) -> bool {
        match self {
            Operation::Assign { .. } => true,
            _ => false,
        }
    }

    pub fn is_load(&self) -> bool {
        match self {
            Operation::Load { .. } => true,
            _ => false,
        }
    }

    pub fn is_return(&self) -> bool {
        match self {
            Operation::Return(_) => true,
            _ => false,
        }
    }

    pub fn is_nop(&self) -> bool {
        match self {
            Operation::Nop => true,
            _ => false,
        }
    }

    pub fn is_branch(&self) -> bool {
        match self {
            Operation::Branch { .. } => true,
            _ => false,
        }
    }

    pub fn is_call(&self) -> bool {
        match self {
            Operation::Call(_) => true,
            _ => false,
        }
    }

    pub fn src(&self) -> Option<&Expression<V>> {
        match self {
            Operation::Assign { src, .. } | Operation::Store { src, .. } => Some(src),
            _ => None,
        }
    }

    pub fn dst(&self) -> Option<&Variable> {
        match self {
            Operation::Assign { dst, .. } | Operation::Load { dst, .. } => Some(dst),
            _ => None,
        }
    }

    pub fn index(&self) -> Option<&Expression<V>> {
        match self {
            Operation::Store { index, .. } | Operation::Load { index, .. } => Some(index),
            _ => None,
        }
    }

    pub fn target(&self) -> Option<&Expression<V>> {
        match self {
            Operation::Branch { target } => Some(target),
            Operation::Call(call) => call.target().expression(),
            _ => None,
        }
    }

    pub fn call(&self) -> Option<&Call<V>> {
        match self {
            Operation::Call(call) => Some(call),
            _ => None,
        }
    }

    pub fn result(&self) -> Option<&Expression<V>> {
        match self {
            Operation::Return(result) => result.as_ref(),
            _ => None,
        }
    }

    pub fn expressions(&self) -> Vec<&Expression<V>> {
        match self {
            Operation::Assign { src, .. } => vec![src],
            Operation::Store { index, src } => vec![index, src],
            Operation::Load { index, .. } => vec![index],
            Operation::Branch { target } => vec![target],
            Operation::Call(call) => call
                .target()
                .expression()
                .map(|e| vec![e])
                .unwrap_or(Vec::new()),
            Operation::Intrinsic(_) | Operation::Return(_) | Operation::Nop => Vec::new(),
        }
    }

    pub fn expressions_mut(&mut self) -> Vec<&mut Expression<V>> {
        match self {
            Operation::Assign { src, .. } => vec![src],
            Operation::Store { index, src } => vec![index, src],
            Operation::Load { index, .. } => vec![index],
            Operation::Branch { target } => vec![target],
            Operation::Call(call) => call
                .target_mut()
                .expression_mut()
                .map(|e| vec![e])
                .unwrap_or(Vec::new()),
            Operation::Intrinsic(_) | Operation::Return(_) | Operation::Nop => Vec::new(),
        }
    }

    pub fn variables_written(&self) -> Option<Vec<&Variable>> {
        match self {
            Operation::Assign { dst, .. } | Operation::Load { dst, .. } => Some(vec![dst]),
            Operation::Call(call) => call.variables_written().map(|vw| vw.iter().collect()),
            Operation::Branch { .. } | Operation::Intrinsic(_) => None,
            Operation::Store { .. } | Operation::Return(_) | Operation::Nop => Some(Vec::new()),
        }
    }

    pub fn variables_read(&self) -> Option<Vec<&Variable>> {
        match self {
            Operation::Assign { src, .. } => Some(src.variables()),
            Operation::Store { index, src } => Some(
                index
                    .variables()
                    .into_iter()
                    .chain(src.variables().into_iter())
                    .collect(),
            ),
            Operation::Load { index, .. } => Some(index.variables()),
            Operation::Call(call) => call.variables_read(),
            Operation::Branch { .. } | Operation::Intrinsic(_) => None,
            Operation::Return(result) => result.as_ref().map(|e| e.variables()),
            Operation::Nop => Some(Vec::new()),
        }
    }

    pub fn variables(&self) -> Option<Vec<&Variable>> {
        let mut variables = self.variables_written()?;
        variables.append(&mut self.variables_read()?);
        variables.sort();
        variables.dedup();
        Some(variables)
    }
}

impl<V: Value> fmt::Display for Operation<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operation::Assign { dst, src } => write!(f, "{} = {}", dst, src),
            Operation::Store { index, src } => write!(f, "[{}] = {}", index, src),
            Operation::Load { dst, index } => write!(f, "{} = [{}]", dst, index),
            Operation::Branch { target } => write!(f, "branch {}", target),
            Operation::Call(call) => call.fmt(f),
            Operation::Intrinsic(intrinsic) => intrinsic.fmt(f),
            Operation::Return(result) => match result {
                Some(result) => write!(f, "return {}", result),
                None => write!(f, "return ???"),
            },
            Operation::Nop => write!(f, "nop"),
        }
    }
}
