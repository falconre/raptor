use crate::analysis::strided_intervals::Value;
use crate::error::*;
use crate::ir;
use std::cmp::Ordering;
use std::fmt;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Interval {
    lo: Value,
    hi: Value,
}

impl Interval {
    pub fn new(lo: Value, hi: Value) -> Interval {
        Interval { lo, hi }
    }

    pub fn new_top(bits: usize) -> Interval {
        Interval {
            lo: Value::Top(bits),
            hi: Value::Top(bits),
        }
    }

    pub fn lo(&self) -> &Value {
        &self.lo
    }
    pub fn hi(&self) -> &Value {
        &self.hi
    }
    pub fn bits(&self) -> usize {
        self.lo().bits()
    }

    pub fn join(&self, other: &Interval) -> Result<Interval> {
        let mut lo = self.lo().min_(other.lo())?;
        let mut hi = self.hi().max_(other.hi())?;

        if lo.sign_overflow(self.lo())? {
            lo = Value::Top(lo.bits());
        }
        if hi.sign_overflow(self.hi())? {
            hi = Value::Top(hi.bits());
        }

        Ok(Interval::new(lo, hi))
    }

    pub fn widen(&self, other: &Interval) -> Result<Interval> {
        let mut result = self.clone();
        if other.lo.partial_cmp_lo(self.lo())?.unwrap() == Ordering::Greater {
            result.lo = Value::Top(self.lo().bits());
        }
        if other.hi.partial_cmp_hi(self.hi())?.unwrap() == Ordering::Greater {
            result.hi = Value::Top(self.hi().bits());
        }
        Ok(result)
    }

    pub fn narrow(&self, other: &Interval) -> Result<Interval> {
        let lo = match self.lo() {
            Value::Top(_) => other.lo().clone(),
            Value::Value(slov) => match other.lo() {
                Value::Top(bits) => Value::Top(*bits),
                Value::Value(olov) => {
                    if slov.cmpltu(olov)?.is_one() {
                        Value::Value(olov.clone())
                    } else {
                        Value::Value(slov.clone())
                    }
                }
                Value::Bottom(_) => self.lo().clone(),
            },
            Value::Bottom(_) => other.lo().clone(),
        };

        let hi = match self.hi() {
            Value::Top(_) => other.hi().clone(),
            Value::Value(slov) => match other.hi() {
                Value::Top(bits) => Value::Top(*bits),
                Value::Value(olov) => {
                    if slov.cmpltu(olov)?.is_one() {
                        Value::Value(slov.clone())
                    } else {
                        Value::Value(olov.clone())
                    }
                }
                Value::Bottom(_) => self.hi().clone(),
            },
            Value::Bottom(_) => other.hi().clone(),
        };

        Ok(Interval { lo, hi })
    }

    pub fn binop<F>(&self, rhs: &Interval, f: F) -> Result<Interval>
    where
        F: Clone + Fn(&ir::Constant, &ir::Constant) -> Result<ir::Constant>,
    {
        let lo = self.lo().binop(rhs.lo(), f.clone())?;
        let hi = self.hi().binop(rhs.hi(), f)?;
        Ok(Interval::new(lo, hi))
    }

    pub fn add(&self, rhs: &Interval) -> Result<Interval> {
        let lo = self.lo().binop(rhs.lo(), |l, r| Ok(l.add(r)?))?;
        let hi = self.hi().binop(rhs.hi(), |l, r| Ok(l.add(r)?))?;
        if lo.sign_overflow(self.lo())? || hi.sign_overflow(self.hi())? {
            Ok(Interval::new_top(self.bits()))
        } else if hi.value_ltu(&lo)? {
            Ok(Interval::new_top(self.bits()))
        } else {
            Ok(Interval::new(lo, hi))
        }
    }

    pub fn sub(&self, rhs: &Interval) -> Result<Interval> {
        // Find the lowest value of the two
        let lo = self.lo().binop(rhs.hi(), |l, r| Ok(l.sub(r)?))?;
        let hi = self.hi().binop(rhs.lo(), |l, r| Ok(l.sub(r)?))?;
        if lo.sign_overflow(self.lo())? || hi.sign_overflow(self.hi())? {
            Ok(Interval::new_top(self.bits()))
        } else if hi.value_ltu(&lo)? {
            Ok(Interval::new_top(self.bits()))
        } else {
            Ok(Interval::new(lo, hi))
        }
    }

    pub fn mul(&self, rhs: &Interval) -> Result<Interval> {
        let lo = self.lo().binop(rhs.lo(), |l, r| Ok(l.mul(r)?))?;
        let hi = self.hi().binop(rhs.hi(), |l, r| Ok(l.mul(r)?))?;

        if lo.sign_overflow(self.lo())? || hi.sign_overflow(self.hi())? {
            Ok(Interval::new_top(self.bits()))
        } else if hi.value_ltu(&lo)? {
            Ok(Interval::new_top(self.bits()))
        } else {
            Ok(Interval::new(lo, hi))
        }
    }

    pub fn divu(&self, _rhs: &Interval) -> Result<Interval> {
        Ok(Interval::new_top(self.bits()))
    }

    pub fn modu(&self, _rhs: &Interval) -> Result<Interval> {
        Ok(Interval::new_top(self.bits()))
    }

    pub fn divs(&self, _rhs: &Interval) -> Result<Interval> {
        Ok(Interval::new_top(self.bits()))
    }

    pub fn mods(&self, _rhs: &Interval) -> Result<Interval> {
        Ok(Interval::new_top(self.bits()))
    }

    // TODO: Better bounds checking?
    pub fn shl(&self, rhs: &Interval) -> Result<Interval> {
        let lo = self.lo().binop(rhs.lo(), |l, r| Ok(l.shl(r)?))?;
        let hi = self.hi().binop(rhs.hi(), |l, r| Ok(l.shl(r)?))?;

        if hi.value_ltu(&lo)? {
            Ok(Interval::new_top(self.bits()))
        } else {
            Ok(Interval::new(lo, hi))
        }
    }

    pub fn shr(&self, _rhs: &Interval) -> Result<Interval> {
        Ok(Interval::new_top(self.bits()))
    }

    pub fn and(&self, rhs: &Interval) -> Result<Interval> {
        if self.lo().is_top() && self.hi().is_top() {
            Ok(rhs.clone())
        } else if rhs.lo().is_top() && rhs.hi().is_top() {
            Ok(self.clone())
        } else {
            Ok(Interval::new_top(self.bits()))
        }
    }

    pub fn or(&self, _rhs: &Interval) -> Result<Interval> {
        Ok(Interval::new_top(self.bits()))
    }

    pub fn xor(&self, _rhs: &Interval) -> Result<Interval> {
        Ok(Interval::new_top(self.bits()))
    }

    pub fn cmpeq(&self, rhs: &Interval) -> Result<Interval> {
        self.binop(rhs, |l, r| Ok(l.cmpeq(r)?))
    }

    pub fn cmpneq(&self, rhs: &Interval) -> Result<Interval> {
        self.binop(rhs, |l, r| Ok(l.cmpneq(r)?))
    }

    pub fn cmplts(&self, rhs: &Interval) -> Result<Interval> {
        self.binop(rhs, |l, r| Ok(l.cmplts(r)?))
    }

    pub fn cmpltu(&self, rhs: &Interval) -> Result<Interval> {
        self.binop(rhs, |l, r| Ok(l.cmpltu(r)?))
    }

    pub fn trun(&self, bits: usize) -> Result<Interval> {
        let mut result = self.clone();
        result.lo = self
            .lo()
            .extop(bits, |c: &ir::Constant| Ok(c.trun(bits)?))?;
        result.hi = self.hi().extop(bits, |c| Ok(c.trun(bits)?))?;
        Ok(result)
    }

    pub fn zext(&self, bits: usize) -> Result<Interval> {
        let mut result = self.clone();
        result.lo = self.lo().extop(bits, |c| Ok(c.zext(bits)?))?;
        result.hi = self.hi().extop(bits, |c| Ok(c.zext(bits)?))?;
        Ok(result)
    }

    pub fn sext(&self, bits: usize) -> Result<Interval> {
        let mut lo = self.lo().extop(bits, |c| Ok(c.sext(bits)?))?;
        let mut hi = self.hi().extop(bits, |c| Ok(c.sext(bits)?))?;

        if lo.is_signed()? {
            lo = Value::Top(lo.bits());
        }
        if hi.is_signed()? {
            hi = Value::Top(hi.bits());
        }

        let mut result = self.clone();
        result.lo = lo;
        result.hi = hi;
        Ok(result)
    }
}

impl fmt::Display for Interval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({},{})", self.lo(), self.hi())
    }
}

impl PartialOrd for Interval {
    fn partial_cmp(&self, other: &Interval) -> Option<Ordering> {
        match self.lo().partial_cmp_lo(other.lo()).ok()?? {
            Ordering::Less => match self.hi().partial_cmp_hi(other.hi()).ok()?? {
                Ordering::Less | Ordering::Equal => Some(Ordering::Less),
                Ordering::Greater => None,
            },
            Ordering::Equal => match self.hi().partial_cmp_hi(other.hi()).ok()?? {
                Ordering::Less => Some(Ordering::Less),
                Ordering::Equal => None,
                Ordering::Greater => Some(Ordering::Greater),
            },
            Ordering::Greater => match self.hi().partial_cmp_hi(other.hi()).ok()?? {
                Ordering::Less => None,
                Ordering::Equal | Ordering::Greater => Some(Ordering::Greater),
            },
        }
    }
}
