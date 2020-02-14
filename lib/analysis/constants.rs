//! A very simple, and fast, constants folding and propagation.
//!
//! Each location has the known constant values for all variables before
//! execution of that location.
//!
//! Calling Constants::eval() uses the known constant values to replace scalars,
//! and then attempts to evaluate the expression to an `ir::Constant`.

use crate::analysis::fixed_point;
use crate::error::*;
use crate::ir;
use crate::ir::eval;
use falcon;
use falcon::memory::MemoryPermissions;
use std::cmp::{Ordering, PartialOrd};
use std::collections::HashMap;

/// Compute constants for the given function
pub fn constants<'r>(
    function: &'r ir::Function<ir::Constant>,
    backing: Option<&falcon::memory::backing::Memory>,
) -> Result<HashMap<ir::ProgramLocation, Constants>> {
    let constants_analysis = ConstantsAnalysis::new(backing);

    let constants = fixed_point::fixed_point_forward(&constants_analysis, function)?;

    // we're now going to remap constants, so each position holds the values of
    // constants immediately preceeding its execution.

    fixed_point::incoming_results(&constants_analysis, function, constants, || {
        Constants::new()
    })
}

/// A latticed `ir::Constant` with a `Top` and `Bottom`
#[allow(dead_code)] // Bottom is never used
#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    Top,
    Constant(ir::Constant),
    Bottom,
}

impl Constant {
    pub fn value(&self) -> Option<&ir::Constant> {
        match *self {
            Constant::Constant(ref constant) => Some(constant),
            Constant::Top | Constant::Bottom => None,
        }
    }
}

impl PartialOrd for Constant {
    fn partial_cmp(&self, other: &Constant) -> Option<Ordering> {
        match *self {
            Constant::Top => match *other {
                Constant::Top => Some(Ordering::Equal),
                Constant::Constant(_) | Constant::Bottom => Some(Ordering::Greater),
            },
            Constant::Constant(ref lc) => match *other {
                Constant::Top => Some(Ordering::Less),
                Constant::Constant(ref rc) => {
                    if lc == rc {
                        Some(Ordering::Equal)
                    } else {
                        None
                    }
                }
                Constant::Bottom => Some(Ordering::Greater),
            },
            Constant::Bottom => match *other {
                Constant::Top | Constant::Constant(_) => Some(Ordering::Less),
                Constant::Bottom => Some(Ordering::Equal),
            },
        }
    }
}

/// A mapping between Variables and known constants values.
#[derive(Clone, Debug, PartialEq)]
pub struct Constants {
    constants: HashMap<ir::Variable, Constant>,
}

impl PartialOrd for Constants {
    fn partial_cmp(&self, other: &Constants) -> Option<Ordering> {
        if self.constants.len() < other.constants.len() {
            for (ls, lc) in self.constants.iter() {
                if !other.constants.get(ls).map(|rc| lc <= rc).unwrap_or(false) {
                    return None;
                }
            }
            Some(Ordering::Less)
        } else if self.constants.len() > other.constants.len() {
            for (ls, lc) in other.constants.iter() {
                if !self.constants.get(ls).map(|rc| lc <= rc).unwrap_or(false) {
                    return None;
                }
            }
            Some(Ordering::Greater)
        } else {
            let mut order = Ordering::Equal;
            for (ls, lc) in &self.constants {
                match other.constants.get(ls) {
                    Some(rc) => {
                        if lc < rc {
                            if order <= Ordering::Equal {
                                order = Ordering::Less;
                            } else {
                                return None;
                            }
                        } else if lc > rc {
                            if order >= Ordering::Equal {
                                order = Ordering::Greater;
                            } else {
                                return None;
                            }
                        }
                    }
                    None => {
                        return None;
                    }
                }
            }
            Some(order)
        }
    }
}

/// We need a special replace function here to make sure we don't replace
/// expressions inside references.
///
/// Take a stack variable, var_0x8:32 for example, and give it a value of 16.
/// If we replaced var_0x8:32 inside a reference, we would turn &(var_0x8:32)
/// into &(0x10:32), which is not what we want.
pub fn replace_expression<V: ir::Value>(
    haystack: &ir::Expression<V>,
    needle: &ir::Expression<V>,
    expr: &ir::Expression<V>,
) -> Result<ir::Expression<V>> {
    if haystack == needle {
        return Ok(expr.clone());
    }
    match haystack {
        ir::Expression::LValue(lvalue) => match lvalue.as_ref() {
            ir::LValue::Variable(_) => Ok(haystack.clone()),
            ir::LValue::Dereference(dereference) => Ok(ir::dereference_expr(
                dereference.expression().replace_expression(needle, expr)?,
            )),
        },
        ir::Expression::RValue(rvalue) => match rvalue.as_ref() {
            ir::RValue::Value(_) => Ok(haystack.clone()),
            ir::RValue::Reference(_) => Ok(haystack.clone()),
        },
        ir::Expression::Add(lhs, rhs) => ir::Expression::add(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Sub(lhs, rhs) => ir::Expression::sub(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Mul(lhs, rhs) => ir::Expression::mul(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Divu(lhs, rhs) => ir::Expression::divu(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Modu(lhs, rhs) => ir::Expression::modu(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Divs(lhs, rhs) => ir::Expression::divs(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Mods(lhs, rhs) => ir::Expression::mods(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::And(lhs, rhs) => ir::Expression::and(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Or(lhs, rhs) => ir::Expression::or(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Xor(lhs, rhs) => ir::Expression::xor(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Shl(lhs, rhs) => ir::Expression::shl(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Shr(lhs, rhs) => ir::Expression::shr(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Cmpeq(lhs, rhs) => ir::Expression::cmpeq(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Cmpneq(lhs, rhs) => ir::Expression::cmpneq(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Cmpltu(lhs, rhs) => ir::Expression::cmpltu(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Cmplts(lhs, rhs) => ir::Expression::cmplts(
            lhs.replace_expression(needle, expr)?,
            rhs.replace_expression(needle, expr)?,
        ),
        ir::Expression::Trun(bits, rhs) => {
            ir::Expression::trun(*bits, rhs.replace_expression(needle, expr)?)
        }
        ir::Expression::Sext(bits, rhs) => {
            ir::Expression::sext(*bits, rhs.replace_expression(needle, expr)?)
        }
        ir::Expression::Zext(bits, rhs) => {
            ir::Expression::zext(*bits, rhs.replace_expression(needle, expr)?)
        }
        ir::Expression::Ite(cond, then, else_) => ir::Expression::ite(
            cond.replace_expression(needle, expr)?,
            then.replace_expression(needle, expr)?,
            else_.replace_expression(needle, expr)?,
        ),
    }
}

impl Constants {
    pub fn new() -> Constants {
        Constants {
            constants: HashMap::new(),
        }
    }

    pub fn variable(&self, variable: &ir::Variable) -> Option<&ir::Constant> {
        self.constants
            .get(variable)
            .and_then(|constant| constant.value())
    }

    fn set_variable(&mut self, variable: ir::Variable, constant: Constant) {
        self.constants.insert(variable, constant);
    }

    fn top(&mut self) {
        self.constants
            .iter_mut()
            .for_each(|(_, constant)| *constant = Constant::Top);
    }

    pub fn replace_with_constants(
        &self,
        expression: &ir::Expression<ir::Constant>,
    ) -> Result<ir::Expression<ir::Constant>> {
        let variables = expression.variables();
        let mut expression = expression.clone();

        for variable in variables {
            if let Some(constant) = self.variable(variable) {
                expression = replace_expression(
                    &expression,
                    &variable.clone().into(),
                    &constant.clone().into(),
                )?;
            }
        }

        Ok(expression)
    }

    pub fn reduce(
        &self,
        expression: &ir::Expression<ir::Constant>,
    ) -> Result<ir::Expression<ir::Constant>> {
        ir::reduce(&self.replace_with_constants(expression)?)
    }

    pub fn eval(&self, expression: &ir::Expression<ir::Constant>) -> Result<Option<ir::Constant>> {
        let expression = self.replace_with_constants(expression)?;

        if expression.all_constants() {
            Ok(Some(eval(&expression)?))
        } else {
            Ok(None)
        }
    }

    fn join(self, other: &Constants) -> Constants {
        let mut result = self.clone();
        for (variable, constant) in other.constants.iter() {
            match self.constants.get(variable) {
                Some(c) => {
                    if c != constant {
                        result.set_variable(variable.clone(), Constant::Top);
                    }
                }
                None => result.set_variable(variable.clone(), constant.clone()),
            }
        }
        result
    }
}

// We require a struct to implement methods for our analysis over.
struct ConstantsAnalysis<'c> {
    backing: Option<&'c falcon::memory::backing::Memory>,
}

impl<'c> ConstantsAnalysis<'c> {
    pub fn new(backing: Option<&'c falcon::memory::backing::Memory>) -> ConstantsAnalysis<'c> {
        ConstantsAnalysis { backing: backing }
    }
}

impl<'r, 'c> fixed_point::FixedPointAnalysis<'r, Constants, ir::Constant>
    for ConstantsAnalysis<'c>
{
    fn trans(
        &self,
        location: &ir::RefProgramLocation<'r, ir::Constant>,
        state: Option<Constants>,
    ) -> Result<Constants> {
        let mut state = match state {
            Some(state) => state,
            None => Constants::new(),
        };

        let state = match location.instruction() {
            Some(instruction) => match instruction.operation() {
                ir::Operation::Assign { dst, src } => {
                    let constant = state
                        .eval(src)?
                        .map(|constant| Constant::Constant(constant))
                        .unwrap_or(Constant::Top);

                    state.set_variable(dst.clone(), constant);
                    state
                }
                ir::Operation::Load { dst, index } => {
                    if let Some(address) = state.eval(&index)?.and_then(|c| c.value_u64()) {
                        let is_read_only_memory = self
                            .backing
                            .and_then(|backing| {
                                backing.permissions(address).map(|permissions| {
                                    (permissions.contains(MemoryPermissions::WRITE) == false)
                                        && (permissions.contains(MemoryPermissions::READ))
                                })
                            })
                            .unwrap_or(false);

                        let is_t9_load = dst
                            .scalar()
                            .map(|scalar| scalar.name() == "$t9")
                            .unwrap_or(false);

                        if is_read_only_memory || is_t9_load {
                            println!("$t9 load at {}", location);
                            let value = self
                                .backing
                                .and_then(|backing| {
                                    backing
                                        .get(address, dst.bits())
                                        .map(|constant| Constant::Constant(constant))
                                })
                                .unwrap_or(Constant::Top);
                            state.set_variable(dst.clone(), value);
                            state
                        } else {
                            state.set_variable(dst.clone(), Constant::Top);
                            state
                        }
                    } else {
                        state.set_variable(dst.clone(), Constant::Top);
                        state
                    }
                }
                ir::Operation::Call(call) => {
                    if let Some(variables_written) = call.variables_written() {
                        variables_written.into_iter().for_each(|variable| {
                            state.set_variable(variable.clone(), Constant::Top)
                        });
                    } else {
                        state.top();
                    }
                    state
                }
                ir::Operation::Branch { .. } => {
                    state.top();
                    state
                }
                ir::Operation::Intrinsic(intrinsic) => {
                    if let Some(scalars_written) = intrinsic.scalars_written() {
                        scalars_written.into_iter().for_each(|scalar| {
                            state.set_variable(scalar.clone().into(), Constant::Top)
                        });
                    } else {
                        state.top();
                    }
                    state
                }
                ir::Operation::Store { .. } | ir::Operation::Return(_) | ir::Operation::Nop => {
                    state
                }
            },
            None => state,
        };

        Ok(state)
    }

    fn join<'j>(&self, state0: Constants, state1: &'j Constants) -> Result<Constants> {
        Ok(state0.join(state1))
    }
}
