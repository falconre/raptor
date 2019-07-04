use analysis::strided_intervals::Value;
use error::*;
use ir;
use std::cmp::Ordering;
use std::fmt;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Interval {
    lo: Value,
    hi: Value,
}

impl Interval {
    pub fn new(lo: Value, hi: Value) -> Interval {
        Interval { lo: lo, hi: hi }
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
        // If lo is less than or equal to rhs lo, then keep lo
        let lo_cmp = self.lo().binop(other.lo(), |l, r| {
            let exp = ir::Expression::or(l.cmpeq(r)?.into(), l.cmpltu(r)?.into())?;
            Ok(ir::eval(&exp)?)
        })?;

        let lo = if lo_cmp.value().map(|value| value.is_one()).unwrap_or(false) {
            self.lo().clone()
        } else {
            Value::Top(self.bits())
        };

        // if hi greater than other hi, then keep hi
        let hi_cmp = self.hi().binop(other.hi(), |l, r| {
            let exp = ir::Expression::xor(l.cmpltu(r)?.into(), ir::expr_const(1, 1))?;
            Ok(ir::eval(&exp)?)
        })?;

        let hi = if hi_cmp.value().map(|value| value.is_one()).unwrap_or(false) {
            self.hi().clone()
        } else {
            Value::Top(self.bits())
        };

        Ok(Interval::new(lo, hi))
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

        Ok(Interval { lo: lo, hi: hi })
    }

    pub fn binop<F>(&self, rhs: &Interval, f: F) -> Result<Interval>
    where
        F: Clone + Fn(&ir::Constant, &ir::Constant) -> Result<ir::Constant>,
    {
        let lo = self.lo().binop(rhs.lo(), f.clone())?;
        let hi = self.hi().binop(rhs.hi(), f.clone())?;
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
        Ok(Interval {
            lo: self
                .lo()
                .extop(bits, |c: &ir::Constant| Ok(c.trun(bits)?))?,
            hi: self.hi().extop(bits, |c| Ok(c.trun(bits)?))?,
        })
    }

    pub fn zext(&self, bits: usize) -> Result<Interval> {
        Ok(Interval {
            lo: self.lo().extop(bits, |c| Ok(c.zext(bits)?))?,
            hi: self.hi().extop(bits, |c| Ok(c.zext(bits)?))?,
        })
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

        Ok(Interval { lo: lo, hi: hi })
    }
}

impl fmt::Display for Interval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({},{})", self.lo(), self.hi())
    }
}

impl PartialOrd for Interval {
    fn partial_cmp(&self, other: &Interval) -> Option<Ordering> {
        if self.lo() < other.lo() {
            if self.hi() >= other.hi() {
                return Some(Ordering::Greater);
            }
        }
        if self.hi() > other.hi() {
            if self.lo() <= other.lo() {
                return Some(Ordering::Less);
            }
        }
        if self.lo() == other.lo() && self.hi() == other.hi() {
            return Some(Ordering::Equal);
        }
        None
    }
}
