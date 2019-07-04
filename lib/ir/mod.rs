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

use error::*;

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
    RValue::Reference(Reference::new(expr, bits).into()).into()
}

pub fn dereference_expr<V: Value>(expr: Expression<V>) -> Expression<V> {
    LValue::Dereference(Dereference::new(expr).into()).into()
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

fn reduce_binop<F, G, H>(
    lhs: Expression<Constant>,
    rhs: Expression<Constant>,
    expression_op: F,
    constant_op: G,
    isize_op: H,
) -> Result<Expression<Constant>>
where
    F: Fn(Expression<Constant>, Expression<Constant>) -> Result<Expression<Constant>>,
    G: Fn(&Constant, &Constant) -> Result<Constant>,
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

    if let Some(l) = lhs.constant() {
        if let Some(r) = rhs.stack_pointer() {
            let l_i64 = l
                .value_i64()
                .expect("Failed to get constant as i64 while reducing expression");
            return Ok(Reference::new(
                StackVariable::new(isize_op(r.offset(), l_i64 as isize), r.bits()).into(),
                rhs.bits(),
            )
            .into());
        }
        if let Some(r) = rhs.constant() {
            return Ok(constant_op(l, r)?.into());
        }
    }

    expression_op(lhs, rhs)
}

pub fn reduce(expr: &Expression<Constant>) -> Result<Expression<Constant>> {
    let expr_out = match expr {
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
        Expression::Add(lhs, rhs) => reduce_binop(
            reduce(lhs)?,
            reduce(rhs)?,
            Expression::add,
            |lhs, rhs| Ok(lhs.add(rhs)?),
            |lhs, rhs| lhs + rhs,
        )?,
        Expression::Sub(lhs, rhs) => reduce_binop(
            reduce(lhs)?,
            reduce(rhs)?,
            Expression::sub,
            |lhs, rhs| Ok(lhs.sub(rhs)?),
            |lhs, rhs| lhs - rhs,
        )?,
        Expression::And(lhs, rhs) => reduce_binop(
            reduce(lhs)?,
            reduce(rhs)?,
            Expression::and,
            |lhs, rhs| Ok(lhs.and(rhs)?),
            |lhs, rhs| lhs & rhs,
        )?,
        _ => expr.clone(),
    };

    if expr_out == *expr {
        if let Some(expr) = simplify(expr) {
            reduce(&expr)
        } else if expr.all_constants() {
            Ok(eval(expr)?.into())
        } else {
            Ok(expr_out)
        }
    } else {
        reduce(&expr_out)
    }
}

pub fn simplify(expr: &Expression<Constant>) -> Option<Expression<Constant>> {
    match expr {
        Expression::Add(lhs, rhs) => {
            if rhs
                .constant()
                .map(|constant| constant.is_zero())
                .unwrap_or(false)
            {
                return Some(lhs.as_ref().clone());
            } else if lhs
                .constant()
                .map(|constant| constant.is_zero())
                .unwrap_or(false)
            {
                return Some(rhs.as_ref().clone());
            }
        }
        _ => {}
    }
    None
}
