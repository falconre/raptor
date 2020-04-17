use crate::error::*;
use crate::ir;
use std::cmp::Ordering;
use std::fmt;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Value {
    Top(usize),
    Value(ir::Constant),
    Bottom(usize),
}

impl Value {
    pub fn is_top(&self) -> bool {
        match self {
            Value::Top(_) => true,
            _ => false,
        }
    }

    pub fn value(&self) -> Option<&ir::Constant> {
        match self {
            Value::Value(c) => Some(c),
            _ => None,
        }
    }

    pub fn bits(&self) -> usize {
        match self {
            Value::Top(bits) => *bits,
            Value::Value(constant) => constant.bits(),
            Value::Bottom(bits) => *bits,
        }
    }

    /// A partial comparison that is greater when the lhs value is less than the
    /// rhs value. Used as the lo value in intervals
    pub fn partial_cmp_lo(&self, other: &Value) -> Result<Option<Ordering>> {
        Ok(match self {
            Value::Top(_) => match other {
                Value::Top(_) => Some(Ordering::Equal),
                Value::Value(_) | Value::Bottom(_) => Some(Ordering::Greater),
            },
            Value::Value(lhs) => match other {
                Value::Top(_) => Some(Ordering::Less),
                Value::Value(rhs) => {
                    if lhs == rhs {
                        Some(Ordering::Equal)
                    } else if lhs.cmpltu(rhs)?.is_one() {
                        Some(Ordering::Greater)
                    } else if rhs.cmpltu(lhs)?.is_one() {
                        Some(Ordering::Less)
                    } else {
                        None
                    }
                }
                Value::Bottom(_) => Some(Ordering::Greater),
            },
            Value::Bottom(_) => match other {
                Value::Top(_) | Value::Value(_) => Some(Ordering::Less),
                Value::Bottom(_) => Some(Ordering::Equal),
            },
        })
    }

    /// A partial comparison that is greater when the lhs value is greater than the
    /// rhs value. Used as the hi value in intervals
    pub fn partial_cmp_hi(&self, other: &Value) -> Result<Option<Ordering>> {
        Ok(match self {
            Value::Top(_) => match other {
                Value::Top(_) => Some(Ordering::Equal),
                Value::Value(_) | Value::Bottom(_) => Some(Ordering::Greater),
            },
            Value::Value(lhs) => match other {
                Value::Top(_) => Some(Ordering::Less),
                Value::Value(rhs) => {
                    if lhs == rhs {
                        Some(Ordering::Equal)
                    } else if lhs.cmpltu(rhs)?.is_one() {
                        Some(Ordering::Less)
                    } else if rhs.cmpltu(lhs)?.is_one() {
                        Some(Ordering::Greater)
                    } else {
                        None
                    }
                }
                Value::Bottom(_) => Some(Ordering::Greater),
            },
            Value::Bottom(_) => match other {
                Value::Top(_) | Value::Value(_) => Some(Ordering::Less),
                Value::Bottom(_) => Some(Ordering::Equal),
            },
        })
    }

    pub(crate) fn max_(&self, other: &Value) -> Result<Value> {
        Ok(match self {
            Value::Top(bits) => Value::Top(*bits),
            Value::Value(lhs) => match other {
                Value::Top(bits) => Value::Top(*bits),
                Value::Value(rhs) => {
                    if lhs.cmpltu(rhs)?.is_one() {
                        other.clone()
                    } else {
                        self.clone()
                    }
                }
                Value::Bottom(_) => self.clone(),
            },
            Value::Bottom(_) => other.clone(),
        })
    }

    pub fn min_(&self, other: &Value) -> Result<Value> {
        Ok(match self {
            Value::Top(bits) => Value::Top(*bits),
            Value::Value(lhs) => match other {
                Value::Top(bits) => Value::Top(*bits),
                Value::Value(rhs) => {
                    if lhs.cmpltu(rhs)?.is_one() {
                        self.clone()
                    } else {
                        other.clone()
                    }
                }
                Value::Bottom(_) => self.clone(),
            },
            Value::Bottom(_) => other.clone(),
        })
    }

    pub fn is_signed(&self) -> Result<bool> {
        Ok(match self {
            Value::Top(_) => false,
            Value::Value(constant) => constant
                .shr(&ir::const_((constant.bits() - 1) as u64, constant.bits()))?
                .is_one(),
            Value::Bottom(_) => false,
        })
    }

    /// Returns true if both contain values, but sign bits differ
    pub fn sign_overflow(&self, other: &Value) -> Result<bool> {
        Ok(match self {
            Value::Top(_) => false,
            Value::Value(lhs) => match other {
                Value::Top(_) | Value::Bottom(_) => false,
                Value::Value(rhs) => {
                    if lhs.bits() != rhs.bits() {
                        return Ok(false);
                    }
                    if lhs.bits() == 1 {
                        return Ok(false);
                    }
                    let shift = ir::const_(lhs.bits() as u64 - 1, lhs.bits());
                    let lhs_bit = lhs.shr(&shift)?.trun(1)?.value_u64().unwrap();
                    let rhs_bit = lhs.shr(&shift)?.trun(1)?.value_u64().unwrap();
                    if lhs_bit != rhs_bit {
                        true
                    } else {
                        false
                    }
                }
            },
            Value::Bottom(_) => false,
        })
    }

    /// Returns true if both are values, and self is ltu to other
    pub fn value_ltu(&self, other: &Value) -> Result<bool> {
        if let Some(lhs) = self.value() {
            if let Some(rhs) = other.value() {
                return Ok(lhs.cmpltu(rhs)?.is_one());
            }
        }
        Ok(false)
    }

    pub fn join(&self, other: &Value) -> Value {
        match self {
            Value::Top(bits) => Value::Top(*bits),
            Value::Value(lhs) => match other {
                Value::Top(bits) => Value::Top(*bits),
                Value::Value(rhs) => {
                    if lhs == rhs {
                        self.clone()
                    } else {
                        Value::Top(lhs.bits())
                    }
                }
                Value::Bottom(_) => self.clone(),
            },
            Value::Bottom(_) => other.clone(),
        }
    }

    pub fn binop<F>(&self, rhs: &Value, f: F) -> Result<Value>
    where
        F: Fn(&ir::Constant, &ir::Constant) -> Result<ir::Constant>,
    {
        Ok(match self {
            Value::Top(bits) => Value::Top(*bits),
            Value::Value(lhs) => match rhs {
                Value::Top(bits) => Value::Top(*bits),
                Value::Value(rhs) => Value::Value(f(lhs, rhs)?),
                Value::Bottom(_) => Value::Value(lhs.clone()),
            },
            Value::Bottom(_) => rhs.clone(),
        })
    }

    pub fn extop<F>(&self, bits: usize, f: F) -> Result<Value>
    where
        F: Fn(&ir::Constant) -> Result<ir::Constant>,
    {
        Ok(match self {
            Value::Top(_) => Value::Top(bits),
            Value::Value(constant) => Value::Value(f(constant)?),
            Value::Bottom(_) => Value::Bottom(bits),
        })
    }
}

/*
impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        match self {
            Value::Top(_) => match other {
                Value::Top(_) => Some(Ordering::Equal),
                Value::Value(_) | Value::Bottom(_) => Some(Ordering::Greater),
            },
            Value::Value(lhs) => match other {
                Value::Top(_) => Some(Ordering::Less),
                Value::Value(rhs) => {
                    if lhs == rhs {
                        Some(Ordering::Equal)
                    } else {
                        None
                    }
                }
                Value::Bottom(_) => Some(Ordering::Greater),
            },
            Value::Bottom(_) => match other {
                Value::Top(_) | Value::Value(_) => Some(Ordering::Less),
                Value::Bottom(_) => Some(Ordering::Equal),
            },
        }
    }
}
*/

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Top(bits) => write!(f, "⊤:{}", bits),
            Value::Value(value) => value.fmt(f),
            Value::Bottom(bits) => write!(f, "⊥:{}", bits),
        }
    }
}
