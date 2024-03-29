//! Raptor IR, a higher-level IR for Falcon IL

mod block;
mod call;
mod control_flow_graph;
mod dereference;
mod edge;
mod expression;
mod function;
mod instruction;
mod location;
mod lvalue;
mod operation;
mod program;
mod reference;
mod rvalue;
mod scalar;
mod stack_variable;
mod variable;

pub use self::block::Block;
pub use self::call::{Call, CallTarget};
pub use self::control_flow_graph::ControlFlowGraph;
pub use self::dereference::Dereference;
pub use self::edge::Edge;
pub use self::expression::Expression;
pub use self::function::Function;
pub use self::instruction::Instruction;
pub use self::location::{
    FunctionLocation, ProgramLocation, RefFunctionLocation, RefProgramLocation,
};
pub use self::lvalue::LValue;
pub use self::operation::Operation;
pub use self::program::Program;
pub use self::reference::Reference;
pub use self::rvalue::RValue;
pub use self::scalar::Scalar;
pub use self::stack_variable::StackVariable;
pub use self::variable::Variable;

pub type Constant = ::falcon::il::Constant;

use crate::error::*;

pub trait Value:
    Clone + ::std::fmt::Debug + ::std::fmt::Display + Eq + ::std::hash::Hash + PartialEq + Send + Sync
{
    fn bits(&self) -> usize;
}

impl Value for Constant {
    fn bits(&self) -> usize {
        self.bits()
    }
}

pub fn const_(value: u64, bits: usize) -> Constant {
    Constant::new(value, bits)
}

pub fn expr_const(value: u64, bits: usize) -> Expression<Constant> {
    Constant::new(value, bits).into()
}

pub fn value_expr<V: Value>(value: V) -> Expression<V> {
    RValue::Value(value).into()
}

pub fn reference_expr<V: Value>(expr: Expression<V>, bits: usize) -> Expression<V> {
    RValue::Reference(Reference::new(expr, bits)).into()
}

pub fn dereference_expr<V: Value>(expr: Expression<V>) -> Expression<V> {
    LValue::Dereference(Dereference::new(expr)).into()
}

pub fn scalar<S: Into<String>>(name: S, bits: usize) -> Scalar {
    Scalar::new(name, bits)
}

pub fn add<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::add(lhs, rhs)
}

pub fn sub<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::sub(lhs, rhs)
}

pub fn mul<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::mul(lhs, rhs)
}

pub fn divu<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::divu(lhs, rhs)
}

pub fn modu<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::modu(lhs, rhs)
}

pub fn divs<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::divs(lhs, rhs)
}

pub fn mods<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::mods(lhs, rhs)
}

pub fn and<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::and(lhs, rhs)
}

pub fn or<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::or(lhs, rhs)
}

pub fn xor<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::xor(lhs, rhs)
}

pub fn shl<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::shl(lhs, rhs)
}

pub fn shr<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::shr(lhs, rhs)
}

pub fn cmpeq<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::cmpeq(lhs, rhs)
}

pub fn cmpneq<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::cmpneq(lhs, rhs)
}

pub fn cmplts<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::cmplts(lhs, rhs)
}

pub fn cmpltu<V: Value>(lhs: Expression<V>, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::cmpltu(lhs, rhs)
}

pub fn trun<V: Value>(bits: usize, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::trun(bits, rhs)
}

pub fn zext<V: Value>(bits: usize, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::zext(bits, rhs)
}

pub fn sext<V: Value>(bits: usize, rhs: Expression<V>) -> Result<Expression<V>> {
    Expression::sext(bits, rhs)
}

pub fn ite<V: Value>(
    cond: Expression<V>,
    then: Expression<V>,
    else_: Expression<V>,
) -> Result<Expression<V>> {
    Expression::ite(cond, then, else_)
}

pub fn eval(expr: &Expression<Constant>) -> Result<Constant> {
    Ok(match expr {
        Expression::LValue(lvalue) => {
            return Err(ErrorKind::EvalNonConstant(format!("{}", lvalue)).into())
        }
        Expression::RValue(rvalue) => match rvalue.as_ref() {
            RValue::Value(constant) => constant.clone(),
            RValue::Reference(reference) => {
                return Err(ErrorKind::EvalNonConstant(format!("{}", reference)).into())
            }
        },
        Expression::Add(lhs, rhs) => eval(lhs)?.add(&eval(rhs)?)?,
        Expression::Sub(lhs, rhs) => eval(lhs)?.sub(&eval(rhs)?)?,
        Expression::Mul(lhs, rhs) => eval(lhs)?.mul(&eval(rhs)?)?,
        Expression::Divu(lhs, rhs) => eval(lhs)?.divu(&eval(rhs)?)?,
        Expression::Modu(lhs, rhs) => eval(lhs)?.modu(&eval(rhs)?)?,
        Expression::Divs(lhs, rhs) => eval(lhs)?.divs(&eval(rhs)?)?,
        Expression::Mods(lhs, rhs) => eval(lhs)?.mods(&eval(rhs)?)?,
        Expression::And(lhs, rhs) => eval(lhs)?.and(&eval(rhs)?)?,
        Expression::Or(lhs, rhs) => eval(lhs)?.or(&eval(rhs)?)?,
        Expression::Xor(lhs, rhs) => eval(lhs)?.xor(&eval(rhs)?)?,
        Expression::Shl(lhs, rhs) => eval(lhs)?.shl(&eval(rhs)?)?,
        Expression::Shr(lhs, rhs) => eval(lhs)?.shr(&eval(rhs)?)?,
        Expression::Cmpeq(lhs, rhs) => eval(lhs)?.cmpeq(&eval(rhs)?)?,
        Expression::Cmpneq(lhs, rhs) => eval(lhs)?.cmpneq(&eval(rhs)?)?,
        Expression::Cmplts(lhs, rhs) => eval(lhs)?.cmplts(&eval(rhs)?)?,
        Expression::Cmpltu(lhs, rhs) => eval(lhs)?.cmpltu(&eval(rhs)?)?,
        Expression::Zext(bits, rhs) => eval(rhs)?.zext(*bits)?,
        Expression::Trun(bits, rhs) => eval(rhs)?.trun(*bits)?,
        Expression::Sext(bits, rhs) => eval(rhs)?.sext(*bits)?,
        Expression::Ite(cond, then, else_) => {
            if eval(cond)?.is_one() {
                eval(then)?
            } else {
                eval(else_)?
            }
        }
    })
}

fn reduce_binop<F, H>(
    lhs: Expression<Constant>,
    rhs: Expression<Constant>,
    expression_op: F,
    isize_op: H,
) -> Result<Expression<Constant>>
where
    F: Fn(Expression<Constant>, Expression<Constant>) -> Result<Expression<Constant>>,
    H: Fn(isize, isize) -> isize,
{
    if let Some(l) = lhs.stack_pointer() {
        if let Some(r) = rhs.constant() {
            let r_i64: i64 = r
                .value_i64()
                .expect("Failed to get constant as i64 while reducing expression");
            return Ok(Reference::new(
                StackVariable::new(isize_op(l.offset(), r_i64 as isize), l.bits()).into(),
                lhs.bits(),
            )
            .into());
        }
    }

    expression_op(lhs, rhs)
}

pub fn reduce(expr: &Expression<Constant>) -> Result<Expression<Constant>> {
    // Move constant to the right
    let expr = match expr {
        Expression::Add(lhs, rhs)
        | Expression::Mul(lhs, rhs)
        | Expression::And(lhs, rhs)
        | Expression::Or(lhs, rhs)
        | Expression::Xor(lhs, rhs)
        | Expression::Cmpeq(lhs, rhs) => {
            if lhs.is_constant() && !rhs.is_constant() {
                let ll = rhs.as_ref().clone();
                let rr = lhs.as_ref().clone();
                match expr {
                    Expression::Add(_, _) => Expression::add(ll, rr)?,
                    Expression::Mul(_, _) => Expression::mul(ll, rr)?,
                    Expression::And(_, _) => Expression::and(ll, rr)?,
                    Expression::Or(_, _) => Expression::or(ll, rr)?,
                    Expression::Xor(_, _) => Expression::xor(ll, rr)?,
                    Expression::Cmpeq(_, _) => Expression::cmpeq(ll, rr)?,
                    _ => unreachable!(),
                }
            } else {
                expr.clone()
            }
        }
        _ => expr.clone(),
    };

    let expr_out = match &expr {
        Expression::LValue(lvalue) => match lvalue.as_ref() {
            LValue::Variable(_) => expr.clone(),
            LValue::Dereference(dereference) => dereference_expr(reduce(dereference.expression())?),
        },
        Expression::RValue(rvalue) => match rvalue.as_ref() {
            RValue::Value(_) => expr.clone(),
            RValue::Reference(reference) => {
                reference_expr(reduce(reference.expression())?, reference.bits())
            }
        },
        Expression::Add(lhs, rhs) => {
            reduce_binop(reduce(lhs)?, reduce(rhs)?, Expression::add, |lhs, rhs| {
                lhs + rhs
            })?
        }
        Expression::Sub(lhs, rhs) => {
            reduce_binop(reduce(lhs)?, reduce(rhs)?, Expression::sub, |lhs, rhs| {
                lhs - rhs
            })?
        }
        Expression::And(lhs, rhs) => {
            reduce_binop(reduce(lhs)?, reduce(rhs)?, Expression::and, |lhs, rhs| {
                lhs & rhs
            })?
        }
        _ => expr.clone(),
    };

    if expr_out == expr {
        if let Some(expr) = simplify(&expr)? {
            reduce(&expr)
        } else if expr.all_constants() {
            Ok(eval(&expr)?.into())
        } else {
            Ok(expr_out)
        }
    } else {
        reduce(&expr_out)
    }
}

pub fn simplify(expr: &Expression<Constant>) -> Result<Option<Expression<Constant>>> {
    match expr {
        Expression::Add(lhs, rhs) => {
            if rhs
                .constant()
                .map(|constant| constant.is_zero())
                .unwrap_or(false)
            {
                return Ok(Some(lhs.as_ref().clone()));
            } else if lhs
                .constant()
                .map(|constant| constant.is_zero())
                .unwrap_or(false)
            {
                return Ok(Some(rhs.as_ref().clone()));
            }
        }
        Expression::Sub(lhs, rhs) => {
            if let Some(rhs_constant) = rhs.as_ref().constant() {
                if let Expression::Add(ll, rr) = lhs.as_ref() {
                    if let Some(constant) = ll.as_ref().constant() {
                        return Ok(Some(Expression::add(
                            constant.sub(rhs_constant)?.into(),
                            rr.as_ref().clone(),
                        )?));
                    }
                    if let Some(constant) = rr.as_ref().constant() {
                        return Ok(Some(Expression::add(
                            ll.as_ref().clone(),
                            constant.sub(rhs_constant)?.into(),
                        )?));
                    }
                    if let Some(stack_variable) = ll.as_ref().stack_pointer() {
                        let bits = ll.reference().unwrap().bits();
                        if let Some(rci64) = rhs_constant.value_i64() {
                            return Ok(Some(Expression::add(
                                Reference::new(
                                    StackVariable::new(
                                        stack_variable.offset() - rci64 as isize,
                                        stack_variable.bits(),
                                    )
                                    .into(),
                                    bits,
                                )
                                .into(),
                                rr.as_ref().clone(),
                            )?));
                        }
                    }
                }
                if rhs_constant.is_zero() {
                    return Ok(Some(lhs.as_ref().clone()));
                }
            }
        }
        Expression::Mul(lhs, rhs) => {
            if lhs
                .constant()
                .map(|constant| constant.is_one())
                .unwrap_or(false)
            {
                return Ok(Some(rhs.as_ref().clone()));
            }
            if lhs
                .constant()
                .map(|constant| constant.is_zero())
                .unwrap_or(false)
            {
                return Ok(Some(Constant::new(0, lhs.bits()).into()));
            }
            if rhs
                .constant()
                .map(|constant| constant.is_one())
                .unwrap_or(false)
            {
                return Ok(Some(lhs.as_ref().clone()));
            }
            if rhs
                .constant()
                .map(|constant| constant.is_zero())
                .unwrap_or(false)
            {
                return Ok(Some(Constant::new(0, rhs.bits()).into()));
            }
        }
        Expression::Cmpeq(lhs, rhs) => {
            // We will always have the constant value on the rhs
            let (lhs, rhs) = if lhs.is_constant() && !rhs.is_constant() {
                (rhs, lhs)
            } else {
                (lhs, rhs)
            };

            if let Some(rhs_constant) = rhs.constant() {
                if rhs_constant.is_one() && rhs_constant.bits() == 1 {
                    return Ok(Some(lhs.as_ref().clone()));
                }
                if let Expression::Sub(ll, lr) = lhs.as_ref() {
                    if let Some(constant) = lr.constant() {
                        return Ok(Some(Expression::cmpeq(
                            ll.as_ref().clone(),
                            rhs_constant.add(constant)?.into(),
                        )?));
                    }
                }
            }

            if rhs
                .constant()
                .map(|constant| constant.is_zero() && constant.bits() == 1)
                .unwrap_or(false)
            {
                if let Expression::Cmpeq(ll, lr) = lhs.as_ref() {
                    if lr
                        .constant()
                        .map(|constant| constant.is_zero() && constant.bits() == 1)
                        .unwrap_or(false)
                    {
                        return Ok(Some(Expression::cmpeq(
                            ll.as_ref().clone(),
                            Constant::new(1, 1).into(),
                        )?));
                    } else if lr
                        .constant()
                        .map(|constant| constant.is_one())
                        .unwrap_or(false)
                    {
                        return Ok(Some(Expression::cmpeq(
                            ll.as_ref().clone(),
                            Constant::new(0, 1).into(),
                        )?));
                    }
                }
            }
        }
        _ => {}
    }
    Ok(None)
}
