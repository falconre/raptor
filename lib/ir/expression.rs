use falcon::il;
use ir::*;
use std::fmt;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Expression<V: Value> {
    LValue(Box<LValue<V>>),
    RValue(Box<RValue<V>>),
    Add(Box<Expression<V>>, Box<Expression<V>>),
    Sub(Box<Expression<V>>, Box<Expression<V>>),
    Mul(Box<Expression<V>>, Box<Expression<V>>),
    Divu(Box<Expression<V>>, Box<Expression<V>>),
    Modu(Box<Expression<V>>, Box<Expression<V>>),
    Divs(Box<Expression<V>>, Box<Expression<V>>),
    Mods(Box<Expression<V>>, Box<Expression<V>>),
    And(Box<Expression<V>>, Box<Expression<V>>),
    Or(Box<Expression<V>>, Box<Expression<V>>),
    Xor(Box<Expression<V>>, Box<Expression<V>>),
    Shl(Box<Expression<V>>, Box<Expression<V>>),
    Shr(Box<Expression<V>>, Box<Expression<V>>),
    Cmpeq(Box<Expression<V>>, Box<Expression<V>>),
    Cmpneq(Box<Expression<V>>, Box<Expression<V>>),
    Cmplts(Box<Expression<V>>, Box<Expression<V>>),
    Cmpltu(Box<Expression<V>>, Box<Expression<V>>),
    Trun(usize, Box<Expression<V>>),
    Sext(usize, Box<Expression<V>>),
    Zext(usize, Box<Expression<V>>),
    Ite(Box<Expression<V>>, Box<Expression<V>>, Box<Expression<V>>),
}

impl Expression<Constant> {
    /// Convert an il expression into an ir expression
    pub fn from_il(expression: &il::Expression) -> Expression<Constant> {
        match expression {
            il::Expression::Scalar(scalar) => {
                Expression::LValue(Box::new(LValue::Variable(scalar.clone().into())))
            }
            il::Expression::Constant(constant) => {
                Expression::RValue(Box::new(constant.clone().into()))
            }
            il::Expression::Add(lhs, rhs) => Expression::Add(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Sub(lhs, rhs) => Expression::Sub(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Mul(lhs, rhs) => Expression::Mul(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Divu(lhs, rhs) => Expression::Divu(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Modu(lhs, rhs) => Expression::Modu(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Divs(lhs, rhs) => Expression::Divs(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Mods(lhs, rhs) => Expression::Mods(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::And(lhs, rhs) => Expression::And(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Or(lhs, rhs) => Expression::Or(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Xor(lhs, rhs) => Expression::Xor(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Shl(lhs, rhs) => Expression::Shl(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Shr(lhs, rhs) => Expression::Shr(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Cmpeq(lhs, rhs) => Expression::Cmpeq(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Cmpneq(lhs, rhs) => Expression::Cmpneq(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Cmpltu(lhs, rhs) => Expression::Cmpltu(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Cmplts(lhs, rhs) => Expression::Cmplts(
                Box::new(Expression::from_il(lhs)),
                Box::new(Expression::from_il(rhs)),
            ),
            il::Expression::Trun(bits, rhs) => {
                Expression::Trun(*bits, Box::new(Expression::from_il(rhs)))
            }
            il::Expression::Sext(bits, rhs) => {
                Expression::Sext(*bits, Box::new(Expression::from_il(rhs)))
            }
            il::Expression::Zext(bits, rhs) => {
                Expression::Zext(*bits, Box::new(Expression::from_il(rhs)))
            }
            il::Expression::Ite(cond, then, else_) => Expression::Ite(
                Box::new(Expression::from_il(cond)),
                Box::new(Expression::from_il(then)),
                Box::new(Expression::from_il(else_)),
            ),
        }
    }

    pub fn all_constants(&self) -> bool {
        self.all_values()
    }

    pub fn constant(&self) -> Option<&Constant> {
        self.rvalue().and_then(|rvalue| rvalue.value())
    }
}

impl<V: Value> Expression<V> {
    pub fn bits(&self) -> usize {
        match self {
            Expression::LValue(lvalue) => lvalue.bits(),
            Expression::RValue(rvalue) => rvalue.bits(),
            Expression::Add(lhs, _)
            | Expression::Sub(lhs, _)
            | Expression::Mul(lhs, _)
            | Expression::Divu(lhs, _)
            | Expression::Modu(lhs, _)
            | Expression::Divs(lhs, _)
            | Expression::Mods(lhs, _)
            | Expression::And(lhs, _)
            | Expression::Or(lhs, _)
            | Expression::Xor(lhs, _)
            | Expression::Shl(lhs, _)
            | Expression::Shr(lhs, _) => lhs.bits(),
            Expression::Cmpeq(_, _)
            | Expression::Cmpneq(_, _)
            | Expression::Cmplts(_, _)
            | Expression::Cmpltu(_, _) => 1,
            Expression::Trun(bits, _) | Expression::Sext(bits, _) | Expression::Zext(bits, _) => {
                *bits
            }
            Expression::Ite(_, then, _) => then.bits(),
        }
    }

    pub fn replace_variable(
        &self,
        variable: &Variable,
        expression: &Expression<V>,
    ) -> Result<Expression<V>> {
        self.replace_expression(&variable.clone().into(), expression)
    }

    pub fn all_values(&self) -> bool {
        match self {
            Expression::LValue(_) => false,
            Expression::RValue(rvalue) => match rvalue.as_ref() {
                RValue::Value(_) => true,
                _ => false,
            },
            Expression::Add(lhs, rhs)
            | Expression::Sub(lhs, rhs)
            | Expression::Mul(lhs, rhs)
            | Expression::Divu(lhs, rhs)
            | Expression::Modu(lhs, rhs)
            | Expression::Divs(lhs, rhs)
            | Expression::Mods(lhs, rhs)
            | Expression::And(lhs, rhs)
            | Expression::Or(lhs, rhs)
            | Expression::Xor(lhs, rhs)
            | Expression::Shl(lhs, rhs)
            | Expression::Shr(lhs, rhs)
            | Expression::Cmpeq(lhs, rhs)
            | Expression::Cmpneq(lhs, rhs)
            | Expression::Cmpltu(lhs, rhs)
            | Expression::Cmplts(lhs, rhs) => lhs.all_values() && rhs.all_values(),
            Expression::Trun(_, rhs) | Expression::Zext(_, rhs) | Expression::Sext(_, rhs) => {
                rhs.all_values()
            }
            Expression::Ite(cond, then, else_) => {
                cond.all_values() && then.all_values() && else_.all_values()
            }
        }
    }

    pub fn replace_expression(
        &self,
        needle: &Expression<V>,
        expr: &Expression<V>,
    ) -> Result<Expression<V>> {
        if self == needle {
            return Ok(expr.clone());
        }
        match self {
            Expression::LValue(lvalue) => match lvalue.as_ref() {
                LValue::Variable(_) => Ok(self.clone()),
                LValue::Dereference(dereference) => Ok(dereference_expr(
                    dereference.expression().replace_expression(needle, expr)?,
                )),
            },
            Expression::RValue(rvalue) => match rvalue.as_ref() {
                RValue::Value(_) => Ok(self.clone()),
                RValue::Reference(reference) => Ok(reference_expr(
                    reference.expression().replace_expression(needle, expr)?,
                    reference.bits(),
                )),
            },
            Expression::Add(lhs, rhs) => Expression::add(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Sub(lhs, rhs) => Expression::sub(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Mul(lhs, rhs) => Expression::mul(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Divu(lhs, rhs) => Expression::divu(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Modu(lhs, rhs) => Expression::modu(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Divs(lhs, rhs) => Expression::divs(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Mods(lhs, rhs) => Expression::mods(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::And(lhs, rhs) => Expression::and(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Or(lhs, rhs) => Expression::or(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Xor(lhs, rhs) => Expression::xor(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Shl(lhs, rhs) => Expression::shl(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Shr(lhs, rhs) => Expression::shr(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Cmpeq(lhs, rhs) => Expression::cmpeq(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Cmpneq(lhs, rhs) => Expression::cmpneq(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Cmpltu(lhs, rhs) => Expression::cmpltu(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Cmplts(lhs, rhs) => Expression::cmplts(
                lhs.replace_expression(needle, expr)?,
                rhs.replace_expression(needle, expr)?,
            ),
            Expression::Trun(bits, rhs) => {
                Expression::trun(*bits, rhs.replace_expression(needle, expr)?)
            }
            Expression::Sext(bits, rhs) => {
                Expression::sext(*bits, rhs.replace_expression(needle, expr)?)
            }
            Expression::Zext(bits, rhs) => {
                Expression::zext(*bits, rhs.replace_expression(needle, expr)?)
            }
            Expression::Ite(cond, then, else_) => Expression::ite(
                cond.replace_expression(needle, expr)?,
                then.replace_expression(needle, expr)?,
                else_.replace_expression(needle, expr)?,
            ),
        }
    }

    pub fn variables(&self) -> Vec<&Variable> {
        let mut variables = match self {
            Expression::LValue(lvalue) => match lvalue.as_ref() {
                LValue::Variable(variable) => vec![variable],
                LValue::Dereference(dereference) => dereference.expression().variables(),
            },
            Expression::RValue(rvalue) => match rvalue.as_ref() {
                RValue::Value(_) => Vec::new(),
                RValue::Reference(reference) => reference.expression().variables(),
            },
            Expression::Add(lhs, rhs)
            | Expression::Sub(lhs, rhs)
            | Expression::Mul(lhs, rhs)
            | Expression::Divu(lhs, rhs)
            | Expression::Modu(lhs, rhs)
            | Expression::Divs(lhs, rhs)
            | Expression::Mods(lhs, rhs)
            | Expression::And(lhs, rhs)
            | Expression::Or(lhs, rhs)
            | Expression::Xor(lhs, rhs)
            | Expression::Shl(lhs, rhs)
            | Expression::Shr(lhs, rhs)
            | Expression::Cmpeq(lhs, rhs)
            | Expression::Cmpneq(lhs, rhs)
            | Expression::Cmplts(lhs, rhs)
            | Expression::Cmpltu(lhs, rhs) => {
                let mut variables = lhs.variables();
                variables.append(&mut rhs.variables());
                variables
            }
            Expression::Trun(_, rhs) | Expression::Sext(_, rhs) | Expression::Zext(_, rhs) => {
                rhs.variables()
            }
            Expression::Ite(cond, then, else_) => {
                let mut variables = cond.variables();
                variables.append(&mut then.variables());
                variables.append(&mut else_.variables());
                variables
            }
        };

        variables.sort();
        variables.dedup();
        variables
    }

    pub fn contains_reference(&self) -> bool {
        match self {
            Expression::LValue(lvalue) => match lvalue.as_ref() {
                LValue::Variable(_) => false,
                LValue::Dereference(dereference) => dereference.expression().contains_reference(),
            },
            Expression::RValue(rvalue) => match rvalue.as_ref() {
                RValue::Value(_) => false,
                RValue::Reference(_) => true,
            },
            Expression::Add(lhs, rhs)
            | Expression::Sub(lhs, rhs)
            | Expression::Mul(lhs, rhs)
            | Expression::Divu(lhs, rhs)
            | Expression::Modu(lhs, rhs)
            | Expression::Divs(lhs, rhs)
            | Expression::Mods(lhs, rhs)
            | Expression::And(lhs, rhs)
            | Expression::Or(lhs, rhs)
            | Expression::Xor(lhs, rhs)
            | Expression::Shl(lhs, rhs)
            | Expression::Shr(lhs, rhs)
            | Expression::Cmpeq(lhs, rhs)
            | Expression::Cmpneq(lhs, rhs)
            | Expression::Cmplts(lhs, rhs)
            | Expression::Cmpltu(lhs, rhs) => lhs.contains_reference() || rhs.contains_reference(),
            Expression::Trun(_, rhs) | Expression::Sext(_, rhs) | Expression::Zext(_, rhs) => {
                rhs.contains_reference()
            }
            Expression::Ite(cond, then, else_) => {
                cond.contains_reference() || then.contains_reference() || else_.contains_reference()
            }
        }
    }

    pub fn lvalue(&self) -> Option<&LValue<V>> {
        match self {
            Expression::LValue(lvalue) => Some(lvalue),
            _ => None,
        }
    }

    pub fn variable(&self) -> Option<&Variable> {
        self.lvalue().and_then(|lvalue| lvalue.variable())
    }

    pub fn stack_variable(&self) -> Option<&StackVariable> {
        self.lvalue().and_then(|lvalue| lvalue.stack_variable())
    }

    pub fn scalar(&self) -> Option<&Scalar> {
        self.lvalue().and_then(|lvalue| lvalue.scalar())
    }

    pub fn rvalue(&self) -> Option<&RValue<V>> {
        match self {
            Expression::RValue(rvalue) => Some(rvalue),
            _ => None,
        }
    }

    pub fn reference(&self) -> Option<&Reference<V>> {
        self.rvalue().and_then(|rvalue| rvalue.reference())
    }

    pub fn stack_pointer(&self) -> Option<&StackVariable> {
        self.reference()
            .and_then(|reference| reference.expression().stack_variable())
    }

    pub fn add(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Add(Box::new(lhs), Box::new(rhs)))
    }

    pub fn sub(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Sub(Box::new(lhs), Box::new(rhs)))
    }

    pub fn mul(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Mul(Box::new(lhs), Box::new(rhs)))
    }

    pub fn divu(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Divu(Box::new(lhs), Box::new(rhs)))
    }

    pub fn modu(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Modu(Box::new(lhs), Box::new(rhs)))
    }

    pub fn divs(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Divs(Box::new(lhs), Box::new(rhs)))
    }

    pub fn mods(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Mods(Box::new(lhs), Box::new(rhs)))
    }

    pub fn and(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::And(Box::new(lhs), Box::new(rhs)))
    }

    pub fn or(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Or(Box::new(lhs), Box::new(rhs)))
    }

    pub fn xor(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Xor(Box::new(lhs), Box::new(rhs)))
    }

    pub fn shl(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Shl(Box::new(lhs), Box::new(rhs)))
    }

    pub fn shr(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Shr(Box::new(lhs), Box::new(rhs)))
    }

    pub fn cmpeq(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Cmpeq(Box::new(lhs), Box::new(rhs)))
    }

    pub fn cmpneq(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Cmpneq(Box::new(lhs), Box::new(rhs)))
    }

    pub fn cmplts(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Cmplts(Box::new(lhs), Box::new(rhs)))
    }

    pub fn cmpltu(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
        if lhs.bits() != rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Cmpltu(Box::new(lhs), Box::new(rhs)))
    }

    pub fn trun(bits: usize, rhs: Expression<V>) -> Result<Expression<V>> {
        if bits >= rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Trun(bits, Box::new(rhs)))
    }

    pub fn zext(bits: usize, rhs: Expression<V>) -> Result<Expression<V>> {
        if bits <= rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Zext(bits, Box::new(rhs)))
    }

    pub fn sext(bits: usize, rhs: Expression<V>) -> Result<Expression<V>> {
        if bits <= rhs.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Sext(bits, Box::new(rhs)))
    }

    pub fn ite(
        cond: Expression<V>,
        then: Expression<V>,
        else_: Expression<V>,
    ) -> Result<Expression<V>> {
        if cond.bits() != 1 || then.bits() != else_.bits() {
            return Err(ErrorKind::Sort.into());
        }
        Ok(Expression::Ite(
            Box::new(cond),
            Box::new(then),
            Box::new(else_),
        ))
    }
}

impl<V: Value> fmt::Display for Expression<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::LValue(lvalue) => lvalue.fmt(f),
            Expression::RValue(rvalue) => rvalue.fmt(f),
            Expression::Add(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            Expression::Sub(lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
            Expression::Mul(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            Expression::Divu(lhs, rhs) => write!(f, "({} /u {})", lhs, rhs),
            Expression::Modu(lhs, rhs) => write!(f, "({} %u {})", lhs, rhs),
            Expression::Divs(lhs, rhs) => write!(f, "({} /s {})", lhs, rhs),
            Expression::Mods(lhs, rhs) => write!(f, "({} %s {})", lhs, rhs),
            Expression::And(lhs, rhs) => write!(f, "({} & {})", lhs, rhs),
            Expression::Or(lhs, rhs) => write!(f, "({} | {})", lhs, rhs),
            Expression::Xor(lhs, rhs) => write!(f, "({} ^ {})", lhs, rhs),
            Expression::Shl(lhs, rhs) => write!(f, "({} << {})", lhs, rhs),
            Expression::Shr(lhs, rhs) => write!(f, "({} >> {})", lhs, rhs),
            Expression::Cmpeq(lhs, rhs) => write!(f, "({} == {})", lhs, rhs),
            Expression::Cmpneq(lhs, rhs) => write!(f, "({} != {})", lhs, rhs),
            Expression::Cmplts(lhs, rhs) => write!(f, "({} <s {})", lhs, rhs),
            Expression::Cmpltu(lhs, rhs) => write!(f, "({} <u {})", lhs, rhs),
            Expression::Trun(bits, rhs) => write!(f, "trun.{}({})", bits, rhs),
            Expression::Sext(bits, rhs) => write!(f, "sext.{}({})", bits, rhs),
            Expression::Zext(bits, rhs) => write!(f, "zext.{}({})", bits, rhs),
            Expression::Ite(cond, then, else_) => write!(f, "ite({}, {}, {})", cond, then, else_),
        }
    }
}

impl<V: Value> From<Scalar> for Expression<V> {
    fn from(scalar: Scalar) -> Expression<V> {
        let v: Variable = scalar.into();
        v.into()
    }
}

impl From<Constant> for Expression<Constant> {
    fn from(constant: Constant) -> Expression<Constant> {
        Expression::RValue(Box::new(constant.into()))
    }
}
