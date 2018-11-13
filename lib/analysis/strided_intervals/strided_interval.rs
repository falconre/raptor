use analysis::strided_intervals::{Interval, Value};
use error::*;
use ir;
use std::cmp::Ordering;
use std::fmt;


fn si_gcd(a: usize, b: usize) -> usize {
    let mut lo = if a < b { a } else { b };
    let mut hi = if a > b { a } else { b };

    if lo == 0 { return hi; }

    loop {
        let remainder = hi % lo;
        if remainder == 0 { return lo; }
        hi = lo;
        lo = remainder;
    }
}

fn si_greater(a: usize, b: usize) -> usize {
    if a > b { a } else { b }
}


#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StridedInterval {
    interval: Interval,
    stride: usize
}


impl StridedInterval {
    pub fn new(interval: Interval, stride: usize) -> StridedInterval {
        StridedInterval {
            interval: interval,
            stride: stride
        }
    }

    pub fn new_top(bits: usize) -> StridedInterval {
        StridedInterval {
            interval: Interval::new_top(bits),
            stride: 0
        }
    }

    pub fn from_constant(constant: &ir::Constant) -> StridedInterval {
        StridedInterval {
            interval: Interval::new(Value::Value(constant.clone()),
                                    Value::Value(constant.clone())),
            stride: 0
        }
    }

    pub fn value(&self) -> Option<&ir::Constant> {
        self.interval().lo().value().and_then(|lo|
            self.interval().hi().value().and_then(|hi|
                if lo == hi { Some(lo) } else { None }))
    }

    pub fn interval(&self) -> &Interval { &self.interval }
    pub fn stride(&self) -> usize { self.stride }
    pub fn bits(&self) -> usize { self.interval.bits() }

    pub fn join(&self, other: &StridedInterval) -> Result<StridedInterval> {
        let interval = self.interval().join(other.interval())?;
        let stride = si_gcd(self.stride(), other.stride());
        Ok(StridedInterval::new(interval, stride))
    }

    pub fn widen(&self, other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new(
            self.interval().widen(other.interval())?,
            si_gcd(self.stride(), other.stride())))
    }

    pub fn narrow(&self, other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new(
            self.interval().narrow(other.interval())?,
            si_gcd(self.stride(), other.stride())))
    }

    pub fn add(&self, other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new(
            self.interval().add(other.interval())?,
            si_greater(self.stride(), other.stride())
        ))
    }

    pub fn sub(&self, other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new(
            self.interval().sub(other.interval())?,
            si_greater(self.stride(), other.stride())
        ))
    }

    pub fn mul(&self, other: &StridedInterval) -> Result<StridedInterval> {
        // If one of the values we're multiplying is a constant (it has a
        // value), put that value on the left
        let (lhs, rhs) = if self.value().is_some() {
            (self, other)
        }
        else {
            (other, self)
        };

        // If the smaller stride is a stride of 1, and we have a value, set the
        // new stride to the larger stride times that constant value.
        let mut stride = si_gcd(lhs.stride(), rhs.stride());

        if let Some(constant) = lhs.value() {
            if stride < 1 { stride = 1; }
            stride *= constant.value_u64().map(|v| v as usize).unwrap_or(1);
        }

        Ok(StridedInterval::new(
            self.interval().mul(other.interval())?,
            stride
        ))
    }

    pub fn divu(&self, other: &StridedInterval) -> Result<StridedInterval> {
        // Put the smaller stride on the right
        let lhs = if self.stride() >= other.stride() { self } else { other };
        let rhs = if lhs == self { other } else { self };

        // If the smaller stride is a stride of 1, and we have a value, set the
        // new stride to the larger stride times that constant value.
        let mut stride = si_gcd(lhs.stride(), rhs.stride());
        if let Some(constant) = rhs.value() {
            if stride == 0 { stride = 1; }
            stride /= constant.value_u64().map(|v| v as usize).unwrap_or(1);
            stride = stride.max(1);
        }

        Ok(StridedInterval::new(
            self.interval().divu(other.interval())?,
            stride
        ))
    }

    pub fn modu(&self, _other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new_top(self.bits()))
    }

    pub fn divs(&self, _other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new_top(self.bits()))
    }

    pub fn mods(&self, _other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new_top(self.bits()))
    }

    pub fn shl(&self, other: &StridedInterval) -> Result<StridedInterval> {
        let mut stride = self.stride();

        if let Some(constant) = other.value() {
            stride <<= constant.value_u64().map(|s| s as usize).unwrap_or(0);
        }

        Ok(StridedInterval::new(
            self.interval().shl(other.interval())?,
            stride
        ))
    }

    pub fn shr(&self, other: &StridedInterval) -> Result<StridedInterval> {
        let mut stride = self.stride();

        if let Some(constant) = other.value() {
            stride >>= constant.value_u64().map(|s| s as usize).unwrap_or(0);
        }

        Ok(StridedInterval::new(
            self.interval().shr(other.interval())?,
            stride
        ))
    }

    pub fn and(&self, other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new(
            self.interval().and(other.interval())?,
            si_gcd(self.stride(), other.stride())
        ))
    }

    pub fn or(&self, _other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new_top(self.bits()))
    }

    pub fn xor(&self, _other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new_top(self.bits()))
    }

    pub fn cmpeq(&self, _other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new_top(1))
    }

    pub fn cmpneq(&self, _other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new_top(1))
    }

    pub fn cmplts(&self, _other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new_top(1))
    }

    pub fn cmpltu(&self, _other: &StridedInterval) -> Result<StridedInterval> {
        Ok(StridedInterval::new_top(1))
    }

    pub fn trun(&self, bits: usize) -> Result<StridedInterval> {
        Ok(StridedInterval::new(
            self.interval().trun(bits)?,
            self.stride()
        ))
    }

    pub fn zext(&self, bits: usize) -> Result<StridedInterval> {
        Ok(StridedInterval::new(
            self.interval().zext(bits)?,
            self.stride()
        ))
    }

    pub fn sext(&self, bits: usize) -> Result<StridedInterval> {
        Ok(StridedInterval::new(
            self.interval().sext(bits)?,
            self.stride()
        ))
    }
}


impl PartialOrd for StridedInterval {
    fn partial_cmp(&self, other: &StridedInterval) -> Option<Ordering> {
        if self.stride() < other.stride() {
            if self.interval() <= other.interval() {
                Some(Ordering::Less)
            }
            else {
                None
            }
        }
        else if self.stride() > other.stride() {
            if self.interval() >= other.interval() {
                Some(Ordering::Greater)
            }
            else {
                None
            }
        }
        else {
            self.interval().partial_cmp(other.interval())
        }
    }
}

impl fmt::Display for StridedInterval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.stride(), self.interval())
    }
}

impl ir::Value for StridedInterval {
    fn bits(&self) -> usize { self.interval().lo().bits() }
}


fn from_e(e: &ir::Expression<ir::Constant>) -> ir::Expression<StridedInterval> {
    match e {
        ir::Expression::LValue(lvalue) => match lvalue.as_ref() {
            ir::LValue::Variable(variable) => variable.clone().into(),
            ir::LValue::Dereference(dereference) =>
                ir::Dereference::new(from_e(dereference.expression())).into()
        },
        ir::Expression::RValue(rvalue) => match rvalue.as_ref() {
            ir::RValue::Value(constant) =>
                ir::RValue::Value(StridedInterval::new(
                    Interval::new(Value::Value(constant.clone()),
                                  Value::Value(constant.clone())),
                    1)).into(),
            ir::RValue::Reference(reference) => {
                let bits = reference.bits();
                ir::Reference::new(from_e(reference.expression()), bits).into()
            }
        },
        ir::Expression::Add(lhs, rhs) =>
            ir::Expression::add(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Sub(lhs, rhs) =>
            ir::Expression::sub(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Mul(lhs, rhs) =>
            ir::Expression::mul(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Divu(lhs, rhs) =>
            ir::Expression::divu(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Modu(lhs, rhs) =>
            ir::Expression::modu(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Divs(lhs, rhs) =>
            ir::Expression::divs(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Mods(lhs, rhs) =>
            ir::Expression::mods(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::And(lhs, rhs) =>
            ir::Expression::and(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Or(lhs, rhs) =>
            ir::Expression::or(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Xor(lhs, rhs) =>
            ir::Expression::xor(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Shl(lhs, rhs) =>
            ir::Expression::shl(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Shr(lhs, rhs) =>
            ir::Expression::shr(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Cmpeq(lhs, rhs) =>
            ir::Expression::cmpeq(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Cmpneq(lhs, rhs) =>
            ir::Expression::cmpneq(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Cmplts(lhs, rhs) =>
            ir::Expression::cmplts(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Cmpltu(lhs, rhs) =>
            ir::Expression::cmpltu(from_e(lhs), from_e(rhs)).unwrap(),
        ir::Expression::Trun(bits, rhs) =>
            ir::Expression::trun(*bits, from_e(rhs)).unwrap(),
        ir::Expression::Sext(bits, rhs) =>
            ir::Expression::sext(*bits, from_e(rhs)).unwrap(),
        ir::Expression::Zext(bits, rhs) =>
            ir::Expression::zext(*bits, from_e(rhs)).unwrap(),
        ir::Expression::Ite(_, then, _) =>
            ir::RValue::Value(StridedInterval::new_top(then.bits())).into()
    }
}


impl<'e> From<&'e ir::Expression<ir::Constant>> for ir::Expression<StridedInterval> {
    fn from(e: &ir::Expression<ir::Constant>) -> ir::Expression<StridedInterval> {
        from_e(e)
    }
}