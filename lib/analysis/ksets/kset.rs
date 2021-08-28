use crate::analysis::ksets::KSetValue;
use crate::analysis::strided_intervals::StridedInterval;
use crate::error::*;
use crate::ir;
use std::cmp::{Ordering, PartialOrd};
use std::collections::BTreeSet;
use std::fmt;

const DEFAULT_K: usize = 4;
const MAX_K: usize = 128;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct KSet {
    k: usize,
    value: KSetValue,
}

impl KSet {
    pub fn new(k: usize, value: KSetValue) -> KSet {
        KSet { k, value }
    }

    pub fn new_top(bits: usize) -> KSet {
        KSet {
            k: DEFAULT_K,
            value: KSetValue::Top(bits),
        }
    }

    pub fn from_constant(constant: ir::Constant) -> KSet {
        let mut hashset = BTreeSet::new();
        hashset.insert(constant);

        KSet {
            k: DEFAULT_K,
            value: KSetValue::Value(hashset),
        }
    }

    pub fn from_strided_interval(strided_interval: &StridedInterval) -> KSet {
        if strided_interval.interval().lo().is_top()
            || strided_interval.interval().hi().is_top()
            || strided_interval.bits() > 64
        {
            return KSet::new_top(strided_interval.bits());
        }

        let lo = strided_interval
            .interval()
            .lo()
            .value()
            .unwrap()
            .value_u64()
            .unwrap();
        let hi = strided_interval
            .interval()
            .hi()
            .value()
            .unwrap()
            .value_u64()
            .unwrap();

        let stride = strided_interval.stride();
        let stride = if stride == 0 { 1 } else { stride };
        let k = (hi - lo) as usize / stride;
        if k > MAX_K {
            return KSet::new_top(strided_interval.bits());
        }

        let mut hashset = BTreeSet::new();
        let mut v = lo;
        while v <= hi {
            hashset.insert(ir::Constant::new(v, strided_interval.bits()));
            v += strided_interval.stride() as u64;
        }

        KSet::new(k as usize, KSetValue::Value(hashset))
    }

    pub fn k(&self) -> usize {
        self.k
    }
    pub fn value(&self) -> &KSetValue {
        &self.value
    }
    pub fn bits(&self) -> usize {
        self.value().bits()
    }

    pub fn join(&self, other: &KSet) -> KSet {
        let k = self.k().max(other.k());

        KSet::new(k, self.value().join(other.value())).set_top_if_above_k()
    }

    fn set_top_if_above_k(self) -> KSet {
        if self
            .value()
            .value()
            .map(|value| value.len() > self.k())
            .unwrap_or(false)
        {
            KSet::new(self.k(), KSetValue::Top(self.bits()))
        } else {
            self
        }
    }

    fn binop<F>(&self, rhs: &KSet, f: F) -> Result<KSet>
    where
        F: Fn(&KSetValue, &KSetValue) -> Result<KSetValue>,
    {
        let value = f(self.value(), rhs.value())?;
        Ok(KSet::new(self.k(), value).set_top_if_above_k())
    }

    fn extop<F>(&self, bits: usize, f: F) -> Result<KSet>
    where
        F: Fn(&KSetValue, usize) -> Result<KSetValue>,
    {
        let value = f(self.value(), bits)?;
        Ok(KSet::new(self.k(), value).set_top_if_above_k())
    }

    pub fn add(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::add)
    }

    pub fn sub(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::sub)
    }

    pub fn mul(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::mul)
    }

    pub fn divu(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::divu)
    }

    pub fn modu(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::modu)
    }

    pub fn divs(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::divs)
    }

    pub fn mods(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::mods)
    }

    pub fn and(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::and)
    }

    pub fn or(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::or)
    }

    pub fn xor(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::xor)
    }

    pub fn shl(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::shl)
    }

    pub fn shr(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::shr)
    }

    pub fn cmpeq(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::cmpeq)
    }

    pub fn cmpneq(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::cmpneq)
    }

    pub fn cmpltu(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::cmpltu)
    }

    pub fn cmplts(&self, rhs: &KSet) -> Result<KSet> {
        self.binop(rhs, KSetValue::cmplts)
    }

    pub fn zext(&self, bits: usize) -> Result<KSet> {
        self.extop(bits, KSetValue::zext)
    }

    pub fn sext(&self, bits: usize) -> Result<KSet> {
        self.extop(bits, KSetValue::sext)
    }

    pub fn trun(&self, bits: usize) -> Result<KSet> {
        let v = self.extop(bits, KSetValue::trun)?;
        Ok(v)
    }

    pub fn ite(cond: &KSet, then: &KSet, else_: &KSet) -> Result<KSet> {
        let k = match cond.value() {
            KSetValue::Top(_) => then.k().max(else_.k()),
            KSetValue::Value(value) => {
                if value.len() == 1 {
                    if value.iter().next().unwrap().is_one() {
                        then.k()
                    } else {
                        else_.k()
                    }
                } else {
                    then.k().max(else_.k())
                }
            }
            KSetValue::Bottom(_) => then.k().max(else_.k()),
        };

        let value = KSetValue::ite(cond.value(), then.value(), else_.value())?;
        Ok(KSet::new(k, value).set_top_if_above_k())
    }
}

impl fmt::Display for KSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.k(), self.value())
    }
}

impl ir::Value for KSet {
    fn bits(&self) -> usize {
        KSet::bits(self)
    }
}

impl PartialOrd for KSet {
    fn partial_cmp(&self, other: &KSet) -> Option<Ordering> {
        let order = self.value().partial_cmp(other.value())?;

        if self.k() < other.k() {
            if order == Ordering::Greater {
                None
            } else {
                Some(Ordering::Less)
            }
        } else if self.k() > other.k() {
            if order == Ordering::Less {
                None
            } else {
                Some(Ordering::Greater)
            }
        } else {
            Some(order)
        }
    }
}

impl<'e> Into<ir::Expression<KSet>> for &'e ir::Expression<ir::Constant> {
    fn into(self) -> ir::Expression<KSet> {
        match self {
            ir::Expression::LValue(lvalue) => match lvalue.as_ref() {
                ir::LValue::Variable(variable) => variable.clone().into(),
                ir::LValue::Dereference(dereference) => {
                    ir::Dereference::new(dereference.expression().into()).into()
                }
            },
            ir::Expression::RValue(rvalue) => match rvalue.as_ref() {
                ir::RValue::Value(constant) => {
                    ir::RValue::Value(KSet::from_constant(constant.clone())).into()
                }
                ir::RValue::Reference(reference) => {
                    ir::Reference::new(reference.expression().into(), reference.bits()).into()
                }
            },
            ir::Expression::Add(lhs, rhs) => {
                ir::Expression::add(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Sub(lhs, rhs) => {
                ir::Expression::sub(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Mul(lhs, rhs) => {
                ir::Expression::mul(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Divu(lhs, rhs) => {
                ir::Expression::divu(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Modu(lhs, rhs) => {
                ir::Expression::modu(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Divs(lhs, rhs) => {
                ir::Expression::divs(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Mods(lhs, rhs) => {
                ir::Expression::mods(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::And(lhs, rhs) => {
                ir::Expression::and(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Or(lhs, rhs) => {
                ir::Expression::or(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Xor(lhs, rhs) => {
                ir::Expression::xor(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Shl(lhs, rhs) => {
                ir::Expression::shl(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Shr(lhs, rhs) => {
                ir::Expression::shr(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Cmpeq(lhs, rhs) => {
                ir::Expression::cmpeq(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Cmpneq(lhs, rhs) => {
                ir::Expression::cmpneq(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Cmplts(lhs, rhs) => {
                ir::Expression::cmplts(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Cmpltu(lhs, rhs) => {
                ir::Expression::cmpltu(lhs.as_ref().into(), rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Zext(bits, rhs) => {
                ir::Expression::zext(*bits, rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Sext(bits, rhs) => {
                ir::Expression::sext(*bits, rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Trun(bits, rhs) => {
                ir::Expression::trun(*bits, rhs.as_ref().into()).unwrap()
            }
            ir::Expression::Ite(cond, then, else_) => ir::Expression::ite(
                cond.as_ref().into(),
                then.as_ref().into(),
                else_.as_ref().into(),
            )
            .unwrap(),
        }
    }
}
