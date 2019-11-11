use analysis::fixed_point;
use error::*;
use falcon::architecture::Architecture;
use ir;
use std::cmp::{Ordering, PartialOrd};
use std::collections::HashMap;

pub fn stack_pointer_offsets<'f>(
    function: &'f ir::Function<ir::Constant>,
    architecture: &dyn Architecture,
) -> Result<HashMap<ir::ProgramLocation, StackPointerOffsets>> {
    // perform the initial analysis
    let spoa = StackPointerOffsetAnalysis::new(architecture.stack_pointer().into());

    let stack_pointer_offsets = fixed_point::fixed_point_forward(&spoa, function)?;

    // incoming_results won't work here, this is a bit more complicated.

    let mut result = HashMap::new();
    for (location, _) in &stack_pointer_offsets {
        result.insert(
            location.clone(),
            location
                .apply(function)?
                .backward()
                .into_iter()
                .fold(State::new(), |spo, location| {
                    spo.join(&stack_pointer_offsets[&location.into()])
                }),
        );
    }

    let mut initial_state = State::new();
    initial_state.set_variable(
        architecture.stack_pointer().into(),
        AnalysisValue::Offset(ir::const_(0, architecture.stack_pointer().bits())),
    );

    result.insert(
        ir::RefProgramLocation::from_function(function)
            .expect("Could not get initial function state")
            .into(),
        initial_state,
    );

    // get the values for the stack pointer
    Ok(result
        .into_iter()
        .map(|(location, state)| (location, StackPointerOffsets::from_state(&state)))
        .collect())
}

pub fn perfect(
    stack_pointer_offsets: &HashMap<ir::RefProgramLocation<ir::Constant>, StackPointerOffsets>,
    stack_pointer: &ir::Variable,
) -> bool {
    stack_pointer_offsets
        .iter()
        .all(|(_, stack_pointer_offsets)| stack_pointer_offsets.get(stack_pointer).is_some())
}

pub struct StackPointerOffsets {
    variables: HashMap<ir::Variable, isize>,
}

impl StackPointerOffsets {
    pub fn new() -> StackPointerOffsets {
        StackPointerOffsets {
            variables: HashMap::new(),
        }
    }

    fn from_state(state: &State) -> StackPointerOffsets {
        StackPointerOffsets {
            variables: state
                .variables()
                .into_iter()
                .filter(|(_, value)| value.offset().is_some())
                .map(|(variable, value)| {
                    (
                        variable.clone(),
                        value
                            .offset()
                            .expect("Expected offset in StackPointerOffsets::from_state")
                            .value_i64()
                            .expect(&format!(
                                "Could not make value_i64 in \
                                 StackPointerOffsets::from_state: {}",
                                value.offset().unwrap()
                            )) as isize,
                    )
                })
                .collect::<HashMap<ir::Variable, isize>>(),
        }
    }

    pub fn replace<V: ir::Value>(
        &self,
        expression: &ir::Expression<V>,
        bits: usize,
    ) -> Result<ir::Expression<V>> {
        match expression {
            ir::Expression::LValue(lvalue) => match lvalue.as_ref() {
                ir::LValue::Variable(variable) => Ok(self
                    .get(variable)
                    .map(|offset| {
                        ir::Reference::new(
                            ir::StackVariable::new(offset, variable.bits()).into(),
                            bits,
                        )
                        .into()
                    })
                    .unwrap_or(expression.clone())),
                ir::LValue::Dereference(dereference) => {
                    Ok(ir::Dereference::new(self.replace(dereference.expression(), bits)?).into())
                }
            },
            ir::Expression::RValue(rvalue) => match rvalue.as_ref() {
                ir::RValue::Value(_) => Ok(expression.clone()),
                ir::RValue::Reference(reference) => Ok(ir::Reference::new(
                    self.replace(reference.expression(), bits)?,
                    reference.bits(),
                )
                .into()),
            },
            ir::Expression::Add(lhs, rhs) => {
                ir::Expression::add(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Sub(lhs, rhs) => {
                ir::Expression::sub(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Mul(lhs, rhs) => {
                ir::Expression::mul(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Divu(lhs, rhs) => {
                ir::Expression::divu(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Modu(lhs, rhs) => {
                ir::Expression::modu(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Divs(lhs, rhs) => {
                ir::Expression::divs(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Mods(lhs, rhs) => {
                ir::Expression::mods(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::And(lhs, rhs) => {
                ir::Expression::and(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Or(lhs, rhs) => {
                ir::Expression::or(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Xor(lhs, rhs) => {
                ir::Expression::xor(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Shl(lhs, rhs) => {
                ir::Expression::shl(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Shr(lhs, rhs) => {
                ir::Expression::shr(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Cmpeq(lhs, rhs) => {
                ir::Expression::cmpeq(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Cmpneq(lhs, rhs) => {
                ir::Expression::cmpneq(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Cmplts(lhs, rhs) => {
                ir::Expression::cmplts(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Cmpltu(lhs, rhs) => {
                ir::Expression::cmpltu(self.replace(lhs, bits)?, self.replace(rhs, bits)?)
            }
            ir::Expression::Trun(bits_, rhs) => {
                ir::Expression::trun(*bits_, self.replace(rhs, bits)?)
            }
            ir::Expression::Zext(bits_, rhs) => {
                ir::Expression::zext(*bits_, self.replace(rhs, bits)?)
            }
            ir::Expression::Sext(bits_, rhs) => {
                ir::Expression::sext(*bits_, self.replace(rhs, bits)?)
            }
            ir::Expression::Ite(cond, then, else_) => ir::Expression::ite(
                self.replace(cond, bits)?,
                self.replace(then, bits)?,
                self.replace(else_, bits)?,
            ),
        }
    }

    pub fn get(&self, variable: &ir::Variable) -> Option<isize> {
        self.variables.get(variable).cloned()
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq)]
enum AnalysisValue {
    Top(usize),
    Offset(ir::Constant),
    Constant(ir::Constant),
    Bottom(usize),
}

impl ::std::fmt::Display for AnalysisValue {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            AnalysisValue::Top(_) => write!(f, "⊤"),
            AnalysisValue::Offset(offset) => write!(f, "Offset({})", offset),
            AnalysisValue::Constant(constant) => write!(f, "Constant({})", constant),
            AnalysisValue::Bottom(_) => write!(f, "⊥"),
        }
    }
}

impl ir::Value for AnalysisValue {
    fn bits(&self) -> usize {
        self.bits()
    }
}

impl PartialOrd for AnalysisValue {
    fn partial_cmp(&self, rhs: &AnalysisValue) -> Option<Ordering> {
        match self {
            AnalysisValue::Top(_) => match rhs {
                AnalysisValue::Top(_) => Some(Ordering::Equal),
                _ => Some(Ordering::Greater),
            },
            AnalysisValue::Offset(_) => match rhs {
                AnalysisValue::Top(_) => Some(Ordering::Less),
                AnalysisValue::Offset(_) => Some(Ordering::Equal),
                AnalysisValue::Constant(_) | AnalysisValue::Bottom(_) => Some(Ordering::Greater),
            },
            AnalysisValue::Constant(_) => match rhs {
                AnalysisValue::Top(_) | AnalysisValue::Offset(_) => Some(Ordering::Less),
                AnalysisValue::Constant(_) => Some(Ordering::Equal),
                AnalysisValue::Bottom(_) => Some(Ordering::Greater),
            },
            AnalysisValue::Bottom(_) => match rhs {
                AnalysisValue::Bottom(_) => Some(Ordering::Equal),
                _ => Some(Ordering::Less),
            },
        }
    }
}

impl AnalysisValue {
    fn offset(&self) -> Option<&ir::Constant> {
        match self {
            AnalysisValue::Offset(offset) => Some(offset),
            _ => None,
        }
    }

    fn join(&self, rhs: &AnalysisValue) -> AnalysisValue {
        match self {
            AnalysisValue::Top(bits) => AnalysisValue::Top(*bits),
            AnalysisValue::Offset(_) | AnalysisValue::Constant(_) => {
                if self == rhs {
                    self.clone()
                } else {
                    AnalysisValue::Top(self.bits())
                }
            }
            AnalysisValue::Bottom(_) => rhs.clone(),
        }
    }

    fn join_mut(&mut self, rhs: &AnalysisValue) {
        let value = self.join(rhs);
        *self = value;
    }

    fn bits(&self) -> usize {
        match self {
            AnalysisValue::Top(bits) | AnalysisValue::Bottom(bits) => *bits,
            AnalysisValue::Offset(constant) | AnalysisValue::Constant(constant) => constant.bits(),
        }
    }
}

fn eval_binop<F>(lhs: &AnalysisValue, rhs: &AnalysisValue, op: F) -> ::error::Result<AnalysisValue>
where
    F: Fn(&ir::Constant, &ir::Constant) -> ::falcon::error::Result<ir::Constant>,
{
    Ok(match lhs {
        AnalysisValue::Top(bits) => AnalysisValue::Top(*bits),
        AnalysisValue::Constant(lhs) => match rhs {
            AnalysisValue::Top(bits) => AnalysisValue::Top(*bits),
            AnalysisValue::Constant(rhs) => AnalysisValue::Constant(op(lhs, rhs)?),
            AnalysisValue::Offset(rhs) => AnalysisValue::Offset(op(lhs, rhs)?),
            AnalysisValue::Bottom(bits) => AnalysisValue::Bottom(*bits),
        },
        AnalysisValue::Offset(lhs) => match rhs {
            AnalysisValue::Top(bits) => AnalysisValue::Top(*bits),
            AnalysisValue::Constant(rhs) => AnalysisValue::Offset(op(lhs, rhs)?),
            AnalysisValue::Offset(_) => AnalysisValue::Top(lhs.bits()),
            AnalysisValue::Bottom(bits) => AnalysisValue::Bottom(*bits),
        },
        AnalysisValue::Bottom(bits) => AnalysisValue::Bottom(*bits),
    })
}

fn eval(expression: &ir::Expression<AnalysisValue>) -> ::error::Result<AnalysisValue> {
    Ok(match expression {
        ir::Expression::LValue(lvalue) => AnalysisValue::Top(lvalue.bits()),
        ir::Expression::RValue(rvalue) => match rvalue.as_ref() {
            ir::RValue::Value(value) => value.clone(),
            ir::RValue::Reference(reference) => reference
                .expression()
                .stack_variable()
                .map(|stack_variable| {
                    AnalysisValue::Offset(ir::const_(
                        stack_variable.offset() as u64,
                        reference.bits(),
                    ))
                })
                .unwrap_or(AnalysisValue::Top(reference.bits())),
        },
        ir::Expression::Add(lhs, rhs) => eval_binop(&eval(lhs)?, &eval(rhs)?, ir::Constant::add)?,
        ir::Expression::Sub(lhs, rhs) => eval_binop(&eval(lhs)?, &eval(rhs)?, ir::Constant::sub)?,
        ir::Expression::Mul(lhs, rhs) => eval_binop(&eval(lhs)?, &eval(rhs)?, ir::Constant::add)?,
        ir::Expression::Divu(lhs, rhs) => eval_binop(&eval(lhs)?, &eval(rhs)?, ir::Constant::divu)?,
        ir::Expression::Modu(lhs, rhs) => eval_binop(&eval(lhs)?, &eval(rhs)?, ir::Constant::modu)?,
        ir::Expression::Divs(lhs, rhs) => eval_binop(&eval(lhs)?, &eval(rhs)?, ir::Constant::divs)?,
        ir::Expression::Mods(lhs, rhs) => eval_binop(&eval(lhs)?, &eval(rhs)?, ir::Constant::mods)?,
        ir::Expression::And(lhs, rhs) => eval_binop(&eval(lhs)?, &eval(rhs)?, ir::Constant::and)?,
        ir::Expression::Or(lhs, rhs) => eval_binop(&eval(lhs)?, &eval(rhs)?, ir::Constant::or)?,
        ir::Expression::Xor(lhs, rhs) => eval_binop(&eval(lhs)?, &eval(rhs)?, ir::Constant::xor)?,
        ir::Expression::Shl(lhs, rhs) => eval_binop(&eval(lhs)?, &eval(rhs)?, ir::Constant::shl)?,
        ir::Expression::Shr(lhs, rhs) => eval_binop(&eval(lhs)?, &eval(rhs)?, ir::Constant::shr)?,
        ir::Expression::Cmpeq(_, _)
        | ir::Expression::Cmpneq(_, _)
        | ir::Expression::Cmplts(_, _)
        | ir::Expression::Cmpltu(_, _) => AnalysisValue::Top(1),
        ir::Expression::Trun(bits, _)
        | ir::Expression::Sext(bits, _)
        | ir::Expression::Zext(bits, _) => AnalysisValue::Top(*bits),
        ir::Expression::Ite(_, then, _) => AnalysisValue::Top(then.bits()),
    })
}

#[derive(Clone, Debug, PartialEq)]
struct State {
    variables: HashMap<ir::Variable, AnalysisValue>,
    stack: HashMap<ir::Constant, AnalysisValue>,
}

impl State {
    fn new() -> State {
        State {
            variables: HashMap::new(),
            stack: HashMap::new(),
        }
    }

    fn replace(
        &self,
        expression: &ir::Expression<ir::Constant>,
    ) -> Result<ir::Expression<AnalysisValue>> {
        match expression {
            ir::Expression::LValue(lvalue) => match lvalue.as_ref() {
                ir::LValue::Variable(variable) => Ok(self
                    .variable(variable)
                    .map(|v| ir::value_expr(v.clone()))
                    .unwrap_or(ir::value_expr(AnalysisValue::Top(variable.bits())))),
                ir::LValue::Dereference(dereference) => {
                    Ok(ir::Dereference::new(self.replace(dereference.expression())?).into())
                }
            },
            ir::Expression::RValue(rvalue) => match rvalue.as_ref() {
                ir::RValue::Value(v) => Ok(ir::value_expr(AnalysisValue::Constant(v.clone()))),
                ir::RValue::Reference(reference) => Ok(reference
                    .expression()
                    .stack_variable()
                    .map(|stack_variable| {
                        ir::value_expr(AnalysisValue::Offset(ir::const_(
                            stack_variable.offset() as u64,
                            reference.bits(),
                        )))
                    })
                    .unwrap_or(
                        ir::Reference::new(self.replace(reference.expression())?, reference.bits())
                            .into(),
                    )),
            },
            ir::Expression::Add(lhs, rhs) => {
                ir::Expression::add(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Sub(lhs, rhs) => {
                ir::Expression::sub(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Mul(lhs, rhs) => {
                ir::Expression::mul(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Divu(lhs, rhs) => {
                ir::Expression::divu(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Modu(lhs, rhs) => {
                ir::Expression::modu(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Divs(lhs, rhs) => {
                ir::Expression::divs(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Mods(lhs, rhs) => {
                ir::Expression::mods(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::And(lhs, rhs) => {
                ir::Expression::and(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Or(lhs, rhs) => {
                ir::Expression::or(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Xor(lhs, rhs) => {
                ir::Expression::xor(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Shl(lhs, rhs) => {
                ir::Expression::shl(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Shr(lhs, rhs) => {
                ir::Expression::shr(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Cmpeq(lhs, rhs) => {
                ir::Expression::cmpeq(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Cmpneq(lhs, rhs) => {
                ir::Expression::cmpneq(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Cmplts(lhs, rhs) => {
                ir::Expression::cmplts(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Cmpltu(lhs, rhs) => {
                ir::Expression::cmpltu(self.replace(lhs)?, self.replace(rhs)?)
            }
            ir::Expression::Trun(bits, rhs) => ir::Expression::trun(*bits, self.replace(rhs)?),
            ir::Expression::Zext(bits, rhs) => ir::Expression::zext(*bits, self.replace(rhs)?),
            ir::Expression::Sext(bits, rhs) => ir::Expression::sext(*bits, self.replace(rhs)?),
            ir::Expression::Ite(cond, then, else_) => ir::Expression::ite(
                self.replace(cond)?,
                self.replace(then)?,
                self.replace(else_)?,
            ),
        }
    }

    fn store(&mut self, index: ir::Constant, value: AnalysisValue) {
        self.stack.insert(index, value);
    }

    fn load(&self, index: &ir::Constant) -> Option<&AnalysisValue> {
        self.stack.get(index)
    }

    fn variable(&self, variable: &ir::Variable) -> Option<&AnalysisValue> {
        self.variables.get(variable)
    }

    fn variables(&self) -> &HashMap<ir::Variable, AnalysisValue> {
        &self.variables
    }

    fn set_variable(&mut self, variable: ir::Variable, analysis_value: AnalysisValue) {
        self.variables.insert(variable, analysis_value);
    }

    fn join(mut self, rhs: &State) -> State {
        for (variable, analysis_value) in &rhs.variables {
            self.variables
                .entry(variable.clone())
                .or_insert(AnalysisValue::Bottom(analysis_value.bits()))
                .join_mut(analysis_value);
        }
        self
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, rhs: &State) -> Option<Ordering> {
        let mut ordering = if self
            .variables
            .iter()
            .any(|(variable, _)| !rhs.variables.contains_key(variable))
        {
            Ordering::Greater
        } else {
            Ordering::Equal
        };
        for (variable, analysis_value) in &rhs.variables {
            match self.variables.get(variable) {
                Some(lhs) => match lhs.partial_cmp(&analysis_value) {
                    Some(o) => {
                        if o == ordering {
                            continue;
                        } else if o != ordering && ordering == Ordering::Equal {
                            ordering = o;
                            continue;
                        } else {
                            return None;
                        }
                    }
                    None => {
                        return None;
                    }
                },
                None => {
                    if ordering == Ordering::Greater {
                        return None;
                    } else {
                        ordering = Ordering::Less;
                    }
                }
            }
        }
        Some(ordering)
    }
}

struct StackPointerOffsetAnalysis {
    stack_pointer: ir::Variable,
}

impl StackPointerOffsetAnalysis {
    fn new(stack_pointer: ir::Variable) -> StackPointerOffsetAnalysis {
        StackPointerOffsetAnalysis {
            stack_pointer: stack_pointer,
        }
    }

    // Handle an operation for stack pointer offset analysis
    // fn handle_operation(
    //     &self,
    //     operation: &ir::Operation<ir::Constant>,
    //     mut state: State
    // ) -> Result<State> {
    //     Ok(match *operation {
    //         ir::Operation::Assign { ref dst, ref src } => {
    //             let src = state.replace(src);
    //             let src = eval(&src)?;

    //             state.set_variable(dst.clone(), src);
    //             state
    //         },
    //         ir::Operation::Load { ref dst, .. } => {
    //             state.set_variable(dst.clone(), AnalysisValue::Top);
    //             state
    //         },
    //         _ => state
    //     })
    // }
}

impl<'f> fixed_point::FixedPointAnalysis<'f, State, ir::Constant> for StackPointerOffsetAnalysis {
    fn trans(
        &self,
        location: &ir::RefProgramLocation<'f, ir::Constant>,
        state: Option<State>,
    ) -> Result<State> {
        // If we are the function entry, we set the value of the stack pointer
        // to 0.
        let mut state = match state {
            Some(state) => state,
            None => {
                // Get function entry
                let function_entry = ir::RefProgramLocation::from_function(location.function())
                    .ok_or("Unable to get function entry")?;

                if *location == function_entry {
                    let mut state = State::new();
                    state.set_variable(
                        self.stack_pointer.clone(),
                        AnalysisValue::Offset(ir::Constant::new(0, self.stack_pointer.bits())),
                    );
                    state
                } else {
                    State::new()
                }
            }
        };

        Ok(match *location.function_location() {
            ir::RefFunctionLocation::Instruction(_, ref instruction) => {
                match instruction.operation() {
                    ir::Operation::Assign { dst, src } => {
                        let src1 = state.replace(src)?;
                        let src2 = eval(&src1)?;
                        state.set_variable(dst.clone(), src2);
                        state
                    }
                    ir::Operation::Store { index, src } => {
                        if let Some(index) = eval(&state.replace(index)?)?.offset() {
                            if let Some(src) = eval(&state.replace(src)?)?.offset() {
                                state.store(index.clone(), AnalysisValue::Offset(src.clone()));
                            }
                        }
                        state
                    }
                    ir::Operation::Load { dst, index } => {
                        let value = if let Some(index) = eval(&state.replace(index)?)?.offset() {
                            state
                                .load(index)
                                .cloned()
                                .unwrap_or(AnalysisValue::Top(dst.bits()))
                        } else {
                            AnalysisValue::Top(dst.bits())
                        };
                        state.set_variable(dst.clone(), value);
                        state
                    }
                    ir::Operation::Branch { .. }
                    | ir::Operation::Call(_)
                    | ir::Operation::Intrinsic(_)
                    | ir::Operation::Return(_)
                    | ir::Operation::Nop => state,
                }
            }
            _ => state,
        })
    }

    fn join(&self, state0: State, state1: &State) -> Result<State> {
        Ok(state0.join(state1))
    }
}
