//! An incomplete Strided Intervals implementation.
use crate::analysis::{fixed_point, use_def, LocationSet};
use crate::error::*;
use crate::ir;
use crate::solver;
use std::cell::RefCell;
use std::cmp::{Ordering, PartialOrd};
use std::collections::{HashMap, HashSet};

mod interval;
mod strided_interval;
mod value;
pub use self::interval::Interval;
pub use self::strided_interval::StridedInterval;
pub use self::value::Value;

pub fn strided_intervals<'f>(
    function: &'f ir::Function<ir::Constant>,
) -> Result<HashMap<ir::ProgramLocation, State>> {
    let strided_interval_analysis = StridedIntervalAnalysis::new(function)?;

    let strided_intervals = fixed_point::fixed_point_forward(&strided_interval_analysis, function)?;

    fixed_point::incoming_results(
        &strided_interval_analysis,
        function,
        strided_intervals,
        || State::new(),
    )
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct State {
    variables: HashMap<ir::Variable, StridedInterval>,
}

impl State {
    pub fn new() -> State {
        State {
            variables: HashMap::new(),
        }
    }

    pub fn variable(&self, variable: &ir::Variable) -> Option<&StridedInterval> {
        self.variables.get(variable)
    }

    pub fn variables(&self) -> &HashMap<ir::Variable, StridedInterval> {
        &self.variables
    }

    pub fn set(&mut self, variable: ir::Variable, strided_interval: StridedInterval) {
        self.variables.insert(variable, strided_interval);
    }

    pub fn eval(&self, e: &ir::Expression<StridedInterval>) -> Result<StridedInterval> {
        Ok(match e {
            ir::Expression::LValue(lvalue) => match lvalue.as_ref() {
                ir::LValue::Variable(variable) => self
                    .variable(variable)
                    .map(|si| si.clone())
                    .unwrap_or(StridedInterval::new_top(e.bits())),
                ir::LValue::Dereference(_) => StridedInterval::new_top(e.bits()),
            },
            ir::Expression::RValue(rvalue) => match rvalue.as_ref() {
                ir::RValue::Value(strided_interval) => strided_interval.clone(),
                ir::RValue::Reference(_) => StridedInterval::new_top(e.bits()),
            },
            ir::Expression::Add(lhs, rhs) => self.eval(&lhs)?.add(&self.eval(rhs)?)?,
            ir::Expression::Sub(lhs, rhs) => self.eval(&lhs)?.sub(&self.eval(rhs)?)?,
            ir::Expression::Mul(lhs, rhs) => self.eval(&lhs)?.mul(&self.eval(rhs)?)?,
            ir::Expression::Divu(lhs, rhs) => self.eval(&lhs)?.divu(&self.eval(rhs)?)?,
            ir::Expression::Modu(lhs, rhs) => self.eval(&lhs)?.modu(&self.eval(rhs)?)?,
            ir::Expression::Divs(lhs, rhs) => self.eval(&lhs)?.divs(&self.eval(rhs)?)?,
            ir::Expression::Mods(lhs, rhs) => self.eval(&lhs)?.mods(&self.eval(rhs)?)?,
            ir::Expression::And(lhs, rhs) => self.eval(&lhs)?.and(&self.eval(rhs)?)?,
            ir::Expression::Or(lhs, rhs) => self.eval(&lhs)?.or(&self.eval(rhs)?)?,
            ir::Expression::Xor(lhs, rhs) => self.eval(&lhs)?.xor(&self.eval(rhs)?)?,
            ir::Expression::Shl(lhs, rhs) => self.eval(&lhs)?.shl(&self.eval(rhs)?)?,
            ir::Expression::Shr(lhs, rhs) => self.eval(&lhs)?.shr(&self.eval(rhs)?)?,
            ir::Expression::Cmpeq(lhs, rhs) => self.eval(&lhs)?.cmpeq(&self.eval(rhs)?)?,
            ir::Expression::Cmpneq(lhs, rhs) => self.eval(&lhs)?.cmpneq(&self.eval(rhs)?)?,
            ir::Expression::Cmplts(lhs, rhs) => self.eval(&lhs)?.cmplts(&self.eval(rhs)?)?,
            ir::Expression::Cmpltu(lhs, rhs) => self.eval(&lhs)?.cmpltu(&self.eval(rhs)?)?,
            ir::Expression::Trun(bits, rhs) => self.eval(&rhs)?.trun(*bits)?,
            ir::Expression::Zext(bits, rhs) => self.eval(&rhs)?.zext(*bits)?,
            ir::Expression::Sext(bits, rhs) => self.eval(&rhs)?.sext(*bits)?,
            ir::Expression::Ite(_, then, _) => StridedInterval::new_top(then.bits()),
        })
    }

    pub fn join(mut self, other: &State) -> Result<State> {
        for (variable, si) in other.variables() {
            let si = match self.variables.get(variable) {
                Some(si2) => si.join(si2)?,
                None => si.clone(),
            };
            self.variables.insert(variable.clone(), si);
        }
        Ok(self)
    }

    pub fn top(&mut self) {
        self.variables
            .iter_mut()
            .for_each(|(_, si)| *si = StridedInterval::new_top(si.bits()));
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &State) -> Option<Ordering> {
        let mut order = Ordering::Equal;

        for (variable, lhs) in self.variables() {
            if other.variables().get(variable).is_none() {
                if order == Ordering::Less {
                    return None;
                } else {
                    order = Ordering::Greater;
                }
                continue;
            }
            let rhs = &other.variables()[variable];
            if lhs > rhs {
                if order == Ordering::Less {
                    return None;
                } else {
                    order = Ordering::Greater;
                }
            } else if lhs < rhs {
                if order == Ordering::Greater {
                    return None;
                } else {
                    order = Ordering::Less;
                }
            }
        }

        for (variable, _rhs) in other.variables() {
            if self.variables.get(variable).is_none() {
                if order == Ordering::Greater {
                    return None;
                } else {
                    order = Ordering::Less;
                }
            }
        }

        Some(order)
    }
}

struct StridedIntervalAnalysis {
    visited: RefCell<HashSet<ir::ProgramLocation>>,
    use_def: HashMap<ir::ProgramLocation, LocationSet>,
    used_variables: HashMap<ir::ProgramLocation, HashSet<ir::Variable>>,
}

impl StridedIntervalAnalysis {
    fn new(function: &ir::Function<ir::Constant>) -> Result<StridedIntervalAnalysis> {
        let mut used_variables: HashMap<ir::ProgramLocation, HashSet<ir::Variable>> =
            HashMap::new();

        let use_def = use_def(function)?;

        for (pl, location_set) in use_def.iter() {
            for location in location_set.locations() {
                let rpl = location.apply(function)?;
                let hash_set = used_variables.entry(pl.clone()).or_insert(HashSet::new());

                let variables_read = rpl
                    .instruction()
                    .and_then(|instruction| instruction.variables_read());
                if let Some(variables_read) = variables_read {
                    for variable in variables_read {
                        hash_set.insert(variable.clone());
                    }
                }
            }
        }

        Ok(StridedIntervalAnalysis {
            visited: RefCell::new(HashSet::new()),
            use_def: use_def,
            used_variables: used_variables,
        })
    }

    fn is_visited(&self, location: &ir::ProgramLocation) -> bool {
        self.visited.borrow().contains(location)
    }

    fn visit(&self, location: ir::ProgramLocation) {
        self.visited.borrow_mut().insert(location);
    }

    fn use_def(&self, program_location: &ir::ProgramLocation) -> &LocationSet {
        &self.use_def[program_location]
    }

    fn used_variables(
        &self,
        program_location: &ir::ProgramLocation,
    ) -> Option<&HashSet<ir::Variable>> {
        self.used_variables.get(program_location)
    }
}

impl<'r> fixed_point::FixedPointAnalysis<'r, State, ir::Constant> for StridedIntervalAnalysis {
    fn trans(
        &self,
        location: &ir::RefProgramLocation<'r, ir::Constant>,
        state: Option<State>,
    ) -> Result<State> {
        let mut state = match state {
            Some(state) => state,
            None => State::new(),
        };

        let program_location: ir::ProgramLocation = location.clone().into();

        let state = match location.function_location() {
            ir::RefFunctionLocation::Instruction(_, instruction) => match instruction.operation() {
                ir::Operation::Assign { dst, src } => {
                    let src = state.eval(&src.into())?;
                    if instruction
                        .address()
                        .map(|a| a == 0x402680)
                        .unwrap_or(false)
                    {
                        println!("{} {} = {}", instruction, dst, src);
                    }
                    let src2 = if self.is_visited(&program_location) {
                        if let Some(original_src) = state.variable(dst) {
                            original_src.widen(&src)?
                        } else {
                            src
                        }
                    } else {
                        src
                    };
                    state.set(dst.clone(), src2);
                    state
                }
                ir::Operation::Load { dst, .. } => {
                    state.set(dst.clone(), StridedInterval::new_top(dst.bits()));
                    state
                }
                ir::Operation::Branch { .. } => {
                    state.top();
                    state
                }
                ir::Operation::Call(call) => {
                    if let Some(variables) = call.variables_written() {
                        variables.into_iter().for_each(|variable| {
                            state.set(variable.clone(), StridedInterval::new_top(variable.bits()));
                        });
                    } else {
                        state.top();
                    }
                    state
                }
                ir::Operation::Intrinsic(intrinsic) => {
                    if let Some(scalars_written) = intrinsic.scalars_written() {
                        scalars_written
                            .into_iter()
                            .map(|s| s.clone().into())
                            .for_each(|variable: ir::Variable| {
                                state.set(
                                    variable.clone(),
                                    StridedInterval::new_top(variable.bits()),
                                )
                            });
                    } else {
                        state.top();
                    }
                    state
                }
                ir::Operation::Return(_) | ir::Operation::Store { .. } | ir::Operation::Nop => {
                    state
                }
            },
            ir::RefFunctionLocation::Edge(edge) => {
                if let Some(condition) = edge.condition() {
                    let mut constraints = vec![condition.clone()];

                    // Used the solver to constrain variables used in this
                    // expression. This works well for conditional flags.
                    for pl in self.use_def(&program_location).locations() {
                        let rpl = pl.apply(location.function())?;

                        if let Some(instruction) = rpl.instruction() {
                            let operation = instruction.operation();
                            if !operation.is_assign() {
                                continue;
                            }
                            let dst = operation.dst().unwrap();
                            let src = operation.src().unwrap();
                            let constraint =
                                ir::Expression::cmpeq(dst.clone().into(), src.clone())?;
                            constraints.push(constraint);
                        }
                    }

                    if let Some(used_variables) = self.used_variables(&program_location) {
                        for variable in used_variables {
                            let variable_expression: ir::Expression<ir::Constant> =
                                variable.clone().into();
                            let lo = solver::minimize(&constraints, &variable_expression)?;
                            let hi = solver::maximize(&constraints, &variable_expression)?;

                            let lo: Value = match lo {
                                Some(constant) => Value::Value(constant),
                                None => Value::Top(variable.bits()),
                            };

                            let hi: Value = match hi {
                                Some(constant) => Value::Value(constant),
                                None => Value::Top(variable.bits()),
                            };

                            let narrow_interval = Interval::new(lo, hi);
                            let narrow_strided_interval = StridedInterval::new(narrow_interval, 0);

                            let strided_interval = match state.variable(variable) {
                                Some(si) => si.narrow(&narrow_strided_interval)?,
                                None => narrow_strided_interval,
                            };

                            state.set(variable.clone(), strided_interval);
                        }
                    } else {
                        for variable in condition.variables() {
                            state.set(variable.clone(), StridedInterval::new_top(variable.bits()));
                        }
                    }

                    let constraints = vec![condition.clone()];

                    // Now constrain the variables in the expression
                    for variable in condition.variables() {
                        let lo = solver::minimize(&constraints, &variable.clone().into())?;
                        let hi = solver::maximize(&constraints, &variable.clone().into())?;

                        let lo: Value = lo
                            .map(|v| Value::Value(v))
                            .unwrap_or(Value::Top(variable.bits()));

                        let hi: Value = hi
                            .map(|v| Value::Value(v))
                            .unwrap_or(Value::Top(variable.bits()));

                        let narrow_interval = Interval::new(lo, hi);
                        let narrow_strided_interval = StridedInterval::new(narrow_interval, 0);

                        let strided_interval = match state.variable(variable) {
                            Some(si) => si.narrow(&narrow_strided_interval)?,
                            None => narrow_strided_interval,
                        };

                        state.set(variable.clone(), strided_interval);
                    }
                }
                state
            }
            ir::RefFunctionLocation::EmptyBlock(_) => state,
        };

        self.visit(program_location);

        Ok(state)
    }

    fn join(&self, state0: State, state1: &State) -> Result<State> {
        state0.join(state1)
    }
}
