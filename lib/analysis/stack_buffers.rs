/// An analysis to identify variables which point to stack buffers
use crate::analysis::fixed_point;
use crate::analysis::lattice::{Lattice, LatticedValue, LatticedVariables};
use crate::error::*;
use crate::ir;
use std::cmp::{Ordering, PartialOrd};
use std::collections::HashMap;
use std::fmt;

pub fn stack_buffers<'f>(
    function: &'f ir::Function<ir::Constant>,
) -> Result<HashMap<ir::ProgramLocation, StackBuffers>> {
    let stack_buffers_analysis = StackBufferAnalysis {};
    let stack_buffers = fixed_point::fixed_point_forward(&stack_buffers_analysis, function)?;

    Ok(stack_buffers)
    // fixed_point::incoming_results(&stack_buffers_analysis, function, stack_buffers, || {
    //     StackBuffers::new()
    // })
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StackBuffer {
    stack_variable: ir::StackVariable,
}

impl StackBuffer {
    pub fn new(stack_variable: ir::StackVariable) -> StackBuffer {
        StackBuffer {
            stack_variable: stack_variable,
        }
    }

    pub fn stack_variable(&self) -> &ir::StackVariable {
        &self.stack_variable
    }

    pub fn bits(&self) -> usize {
        self.stack_variable.bits()
    }

    pub fn join(&self, other: &StackBuffer) -> Result<StackBuffer> {
        Ok(if self == other {
            self.clone()
        } else {
            let lhs_offset = self.stack_variable.offset();
            let rhs_offset = other.stack_variable.offset();
            StackBuffer::new(ir::StackVariable::new(
                lhs_offset.min(rhs_offset),
                self.stack_variable.bits(),
            ))
        })
    }
}

impl PartialOrd for StackBuffer {
    fn partial_cmp(&self, rhs: &StackBuffer) -> Option<Ordering> {
        if self == rhs {
            Some(Ordering::Equal)
        } else {
            None
        }
    }
}

impl LatticedValue for StackBuffer {
    fn join(&self, other: &StackBuffer) -> Result<StackBuffer> {
        self.join(other)
    }
}

impl Into<ir::StackVariable> for StackBuffer {
    fn into(self) -> ir::StackVariable {
        self.stack_variable
    }
}

impl From<ir::StackVariable> for StackBuffer {
    fn from(stack_variable: ir::StackVariable) -> StackBuffer {
        StackBuffer {
            stack_variable: stack_variable,
        }
    }
}

impl fmt::Display for StackBuffer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.stack_variable)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StackBuffers {
    latticed_variables: LatticedVariables<StackBuffer>,
}

impl StackBuffers {
    pub fn new() -> StackBuffers {
        StackBuffers {
            latticed_variables: LatticedVariables::new(),
        }
    }

    pub fn join(self, other: &StackBuffers) -> Result<StackBuffers> {
        Ok(StackBuffers {
            latticed_variables: self.latticed_variables.join(&other.latticed_variables)?,
        })
    }

    pub fn set(&mut self, variable: ir::Variable, lattice: Lattice<StackBuffer>) {
        self.latticed_variables.set_variable(variable, lattice);
    }

    pub fn top(&mut self) {
        self.latticed_variables.top();
    }

    pub fn eval(&self, expr: &ir::Expression<ir::Constant>) -> Result<Lattice<StackBuffer>> {
        Ok(match expr {
            ir::Expression::RValue(rvalue) => match rvalue.as_ref() {
                ir::RValue::Reference(reference) => {
                    if let Some(stack_variable) = reference.expression().stack_variable() {
                        Lattice::Value(StackBuffer::new(stack_variable.clone()))
                    } else {
                        Lattice::Top(reference.bits())
                    }
                }
                ir::RValue::Value(constant) => Lattice::Bottom(constant.bits()),
            },
            ir::Expression::LValue(lvalue) => match lvalue.as_ref() {
                ir::LValue::Variable(variable) => self
                    .latticed_variables
                    .variable(variable)
                    .cloned()
                    .unwrap_or(Lattice::Top(variable.bits())),
                ir::LValue::Dereference(dereference) => Lattice::Top(dereference.bits()),
            },
            ir::Expression::Add(lhs, rhs)
            | ir::Expression::Sub(lhs, rhs)
            | ir::Expression::Mul(lhs, rhs) => self.eval(lhs)?.join(&self.eval(rhs)?)?,
            ir::Expression::Divu(_, _)
            | ir::Expression::Modu(_, _)
            | ir::Expression::Divs(_, _)
            | ir::Expression::Mods(_, _)
            | ir::Expression::And(_, _)
            | ir::Expression::Or(_, _)
            | ir::Expression::Xor(_, _)
            | ir::Expression::Shl(_, _)
            | ir::Expression::Shr(_, _)
            | ir::Expression::Cmpeq(_, _)
            | ir::Expression::Cmpneq(_, _)
            | ir::Expression::Cmpltu(_, _)
            | ir::Expression::Cmplts(_, _)
            | ir::Expression::Ite(_, _, _) => Lattice::Top(expr.bits()),
            ir::Expression::Zext(bits, _)
            | ir::Expression::Sext(bits, _)
            | ir::Expression::Trun(bits, _) => Lattice::Top(*bits),
        })
    }
}

impl PartialOrd for StackBuffers {
    fn partial_cmp(&self, rhs: &StackBuffers) -> Option<Ordering> {
        self.latticed_variables.partial_cmp(&rhs.latticed_variables)
    }
}

struct StackBufferAnalysis {}

impl<'f> fixed_point::FixedPointAnalysis<'f, StackBuffers, ir::Constant> for StackBufferAnalysis {
    fn trans(
        &self,
        location: &ir::RefProgramLocation<'f, ir::Constant>,
        state: Option<StackBuffers>,
    ) -> Result<StackBuffers> {
        let mut stack_buffers = match state {
            Some(stack_buffers) => stack_buffers,
            None => StackBuffers::new(),
        };

        if let Some(instruction) = location.instruction() {
            match instruction.operation() {
                ir::Operation::Assign { dst, src } => {
                    stack_buffers.set(dst.clone(), stack_buffers.eval(src)?);
                }
                ir::Operation::Load { dst, .. } => {
                    stack_buffers.set(dst.clone(), Lattice::Top(dst.bits()));
                }
                ir::Operation::Call(call) => {
                    if let Some(variables_written) = call.variables_written() {
                        for variable_written in variables_written {
                            stack_buffers.set(
                                variable_written.clone(),
                                Lattice::Top(variable_written.bits()),
                            )
                        }
                    } else {
                        stack_buffers.top();
                    }
                }
                ir::Operation::Intrinsic(intrinsic) => {
                    if let Some(scalars_written) = intrinsic.scalars_written() {
                        for scalar_written in scalars_written {
                            stack_buffers.set(
                                scalar_written.clone().into(),
                                Lattice::Top(scalar_written.bits()),
                            )
                        }
                    } else {
                        stack_buffers.top();
                    }
                }
                ir::Operation::Store { .. } | ir::Operation::Nop(_) => {}
                ir::Operation::Branch { .. } | ir::Operation::Return(_) => {
                    stack_buffers.top();
                }
            }
        }

        Ok(stack_buffers)
    }

    fn join(&self, state0: StackBuffers, state1: &StackBuffers) -> Result<StackBuffers> {
        state0.join(state1)
    }
}
