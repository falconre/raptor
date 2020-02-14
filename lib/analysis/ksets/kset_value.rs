use crate::error::*;
use crate::ir;
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum KSetValue {
    Top(usize),
    Value(BTreeSet<ir::Constant>),
    Bottom(usize),
}

impl KSetValue {
    pub fn value(&self) -> Option<&BTreeSet<ir::Constant>> {
        match self {
            KSetValue::Top(_) | KSetValue::Bottom(_) => None,
            KSetValue::Value(value) => Some(value),
        }
    }

    pub fn bits(&self) -> usize {
        match self {
            KSetValue::Top(bits) => *bits,
            KSetValue::Value(values) => values.iter().next().unwrap().bits(),
            KSetValue::Bottom(bits) => *bits,
        }
    }

    pub fn join(&self, other: &KSetValue) -> KSetValue {
        match self {
            KSetValue::Top(bits) => KSetValue::Top(*bits),
            KSetValue::Value(lhs) => match other {
                KSetValue::Top(bits) => KSetValue::Top(*bits),
                KSetValue::Value(rhs) => {
                    let mut hashset = lhs.clone();
                    for v in rhs {
                        hashset.insert(v.clone());
                    }
                    KSetValue::Value(hashset)
                }
                KSetValue::Bottom(bits) => KSetValue::Bottom(*bits),
            },
            KSetValue::Bottom(bits) => KSetValue::Bottom(*bits),
        }
    }

    pub fn binop<F>(&self, rhs: &KSetValue, f: F) -> Result<KSetValue>
    where
        F: Fn(&ir::Constant, &ir::Constant) -> Result<ir::Constant>,
    {
        Ok(match self {
            KSetValue::Top(bits) => KSetValue::Top(*bits),
            KSetValue::Value(l) => match rhs {
                KSetValue::Top(bits) => KSetValue::Top(*bits),
                KSetValue::Value(r) => {
                    let mut values = BTreeSet::new();
                    for ll in l {
                        for rr in r {
                            values.insert(f(ll, rr)?);
                        }
                    }
                    KSetValue::Value(values)
                }
                KSetValue::Bottom(bits) => KSetValue::Bottom(*bits),
            },
            KSetValue::Bottom(bits) => KSetValue::Bottom(*bits),
        })
    }

    pub fn cmpop<F>(&self, rhs: &KSetValue, f: F) -> Result<KSetValue>
    where
        F: Fn(&ir::Constant, &ir::Constant) -> Result<ir::Constant>,
    {
        Ok(match self {
            KSetValue::Top(_) => KSetValue::Top(1),
            KSetValue::Value(l) => match rhs {
                KSetValue::Top(_) => KSetValue::Top(1),
                KSetValue::Value(r) => {
                    let mut values = BTreeSet::new();
                    for ll in l {
                        for rr in r {
                            values.insert(f(ll, rr)?);
                        }
                    }
                    KSetValue::Value(values)
                }
                KSetValue::Bottom(_) => KSetValue::Bottom(1),
            },
            KSetValue::Bottom(_) => KSetValue::Bottom(1),
        })
    }

    pub fn extop<F>(&self, bits: usize, f: F) -> Result<KSetValue>
    where
        F: Fn(&ir::Constant, usize) -> Result<ir::Constant>,
    {
        Ok(match self {
            KSetValue::Top(_) => KSetValue::Top(bits),
            KSetValue::Value(l) => {
                let mut values = BTreeSet::new();
                for ll in l {
                    values.insert(f(ll, bits)?);
                }
                KSetValue::Value(values)
            }
            KSetValue::Bottom(_) => KSetValue::Bottom(bits),
        })
    }

    pub fn add(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.binop(rhs, |l, r| Ok(l.add(r)?))
    }

    pub fn sub(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.binop(rhs, |l, r| Ok(l.sub(r)?))
    }

    pub fn mul(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.binop(rhs, |l, r| Ok(l.mul(r)?))
    }

    pub fn divu(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.binop(rhs, |l, r| Ok(l.divu(r)?))
    }

    pub fn modu(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.binop(rhs, |l, r| Ok(l.modu(r)?))
    }

    pub fn divs(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.binop(rhs, |l, r| Ok(l.divs(r)?))
    }

    pub fn mods(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.binop(rhs, |l, r| Ok(l.mods(r)?))
    }

    pub fn and(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.binop(rhs, |l, r| Ok(l.and(r)?))
    }

    pub fn or(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.binop(rhs, |l, r| Ok(l.or(r)?))
    }

    pub fn xor(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.binop(rhs, |l, r| Ok(l.xor(r)?))
    }

    pub fn shl(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.binop(rhs, |l, r| Ok(l.shl(r)?))
    }

    pub fn shr(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.binop(rhs, |l, r| Ok(l.shr(r)?))
    }

    pub fn cmpeq(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.cmpop(rhs, |l, r| Ok(l.cmpeq(r)?))
    }

    pub fn cmpneq(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.cmpop(rhs, |l, r| Ok(l.cmpneq(r)?))
    }

    pub fn cmplts(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.cmpop(rhs, |l, r| Ok(l.cmplts(r)?))
    }

    pub fn cmpltu(&self, rhs: &KSetValue) -> Result<KSetValue> {
        if self.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }

        self.cmpop(rhs, |l, r| Ok(l.cmpltu(r)?))
    }

    pub fn zext(&self, bits: usize) -> Result<KSetValue> {
        if self.bits() >= bits {
            return Err(ErrorKind::Sort.into());
        }

        self.extop(bits, |l, b| Ok(l.zext(b)?))
    }

    pub fn sext(&self, bits: usize) -> Result<KSetValue> {
        if self.bits() >= bits {
            return Err(ErrorKind::Sort.into());
        }

        self.extop(bits, |l, b| Ok(l.sext(b)?))
    }

    pub fn trun(&self, bits: usize) -> Result<KSetValue> {
        if self.bits() <= bits {
            return Err(ErrorKind::Sort.into());
        }

        self.extop(bits, |l, b| Ok(l.trun(b)?))
    }

    pub fn ite(cond: &KSetValue, then: &KSetValue, else_: &KSetValue) -> Result<KSetValue> {
        if cond.bits() != 1 {
            return Err(ErrorKind::Sort.into());
        } else if then.bits() != else_.bits() {
            return Err(ErrorKind::Sort.into());
        }

        Ok(match cond {
            KSetValue::Top(_) => then.join(else_),
            KSetValue::Value(value) => {
                if value.len() == 1 {
                    if value.iter().next().unwrap().is_one() {
                        then.clone()
                    } else {
                        else_.clone()
                    }
                } else {
                    then.join(else_)
                }
            }
            KSetValue::Bottom(bits) => KSetValue::Bottom(*bits),
        })
    }
}

impl PartialOrd for KSetValue {
    fn partial_cmp(&self, other: &KSetValue) -> Option<Ordering> {
        match self {
            KSetValue::Top(_) => match other {
                KSetValue::Top(_) => Some(Ordering::Equal),
                KSetValue::Value(_) | KSetValue::Bottom(_) => Some(Ordering::Greater),
            },
            KSetValue::Value(lhs) => match other {
                KSetValue::Top(_) => Some(Ordering::Less),
                KSetValue::Value(rhs) => {
                    let mut order = Ordering::Equal;
                    for l in lhs {
                        if rhs.get(l).is_none() {
                            order = Ordering::Greater;
                        }
                    }
                    for r in rhs {
                        if lhs.get(r).is_none() {
                            if order == Ordering::Greater {
                                return None;
                            }
                            order = Ordering::Less;
                        }
                    }
                    Some(order)
                }
                KSetValue::Bottom(_) => Some(Ordering::Greater),
            },
            KSetValue::Bottom(_) => match other {
                KSetValue::Top(_) | KSetValue::Value(_) => Some(Ordering::Less),
                KSetValue::Bottom(_) => Some(Ordering::Equal),
            },
        }
    }
}

impl fmt::Display for KSetValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{}]",
            match self {
                KSetValue::Top(bits) => format!("⊤:{}", bits),
                KSetValue::Value(value) => value
                    .iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<String>>()
                    .join(","),
                KSetValue::Bottom(bits) => format!("⊥:{}", bits),
            }
        )
    }
}
