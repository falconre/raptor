//! An incomplete Strided Intervals implementation.
use crate::analysis::{fixed_point, use_def, LocationSet};
use crate::error::*;
use crate::ir;
use crate::solver;
use std::cmp::{Ordering, PartialOrd};
use std::collections::{HashMap, HashSet, VecDeque};

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
    variables_widen: HashMap<ir::Variable, StridedInterval>,
}

impl State {
    pub fn new() -> State {
        State {
            variables: HashMap::new(),
            variables_widen: HashMap::new(),
        }
    }

    pub fn variable(&self, variable: &ir::Variable) -> Option<&StridedInterval> {
        self.variables.get(variable)
    }

    pub fn variables(&self) -> &HashMap<ir::Variable, StridedInterval> {
        &self.variables
    }

    pub fn set(&mut self, variable: ir::Variable, strided_interval: StridedInterval) {
        if let Some(previous_si) = self.variables.get(&variable) {
            if *previous_si != strided_interval {
                self.variables_widen
                    .insert(variable.clone(), previous_si.clone());
            }
        }
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
        for (variable, si) in &other.variables_widen {
            let si = match self.variables_widen.get(variable) {
                Some(other_si) => si.join(other_si)?,
                None => si.clone(),
            };
            self.variables_widen.insert(variable.clone(), si);
        }

        for (variable, other_si) in other.variables() {
            let si = match self.variables.get(variable) {
                Some(self_si) => {
                    let mut joined = self_si.join(other_si)?;

                    // Do we need to widen?
                    if joined > *self_si {
                        if self
                            .variables_widen
                            .get(variable)
                            .map(|widen_si| joined > *widen_si)
                            .unwrap_or(false)
                        {
                            joined = self_si.widen(&joined)?;
                        }
                        self.variables_widen
                            .insert(variable.clone(), joined.clone());
                    }

                    joined
                }
                None => other_si.clone(),
            };
            self.variables.insert(variable.clone(), si);
        }
        Ok(self)
    }

    pub fn top(&mut self) {
        self.variables
            .iter_mut()
            .for_each(|(_, si)| *si = StridedInterval::new_top(si.bits()));
        self.variables_widen
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
    use_def: HashMap<ir::ProgramLocation, LocationSet>,
}

impl StridedIntervalAnalysis {
    fn new(function: &ir::Function<ir::Constant>) -> Result<StridedIntervalAnalysis> {
        let use_def = use_def(function)?;

        Ok(StridedIntervalAnalysis { use_def: use_def })
    }

    fn use_def(&self, program_location: &ir::ProgramLocation) -> &LocationSet {
        &self.use_def[program_location]
    }

    /// Takes a RefProgramLocation which points at an edge. Returns all the
    /// variables in the predecessor block which are possibly influenced by the
    /// condition, recursively
    fn block_guarded_variables(
        &self,
        ref_program_location: &ir::RefProgramLocation<ir::Constant>,
    ) -> Result<HashSet<ir::Variable>> {
        // The block to search within
        let block_index = ref_program_location.edge().unwrap().head();

        let mut variables: HashSet<ir::Variable> = HashSet::new();

        // locations we've already looked at
        let mut locations_considered: HashSet<ir::ProgramLocation> = HashSet::new();

        // queue of locations we still need to look at
        let mut queue: VecDeque<ir::ProgramLocation> = VecDeque::new();

        for program_location in self
            .use_def(&ref_program_location.clone().into())
            .locations()
        {
            queue.push_back(program_location.clone());
        }

        while !queue.is_empty() {
            let pl = queue.pop_front().unwrap();
            locations_considered.insert(pl.clone().into());

            let rpl = pl.apply(ref_program_location.function())?;

            if rpl
                .block()
                .map(|block| block.index() != block_index)
                .unwrap_or(true)
            {
                continue;
            }

            let instruction = match rpl.instruction() {
                Some(instruction) => instruction,
                None => continue,
            };

            match instruction.operation() {
                ir::Operation::Assign { dst, src } => {
                    variables.insert(dst.clone());
                    for variable in src.variables() {
                        variables.insert(variable.clone());
                    }
                    for location in self.use_def(&pl).locations() {
                        if !locations_considered.contains(location) {
                            queue.push_back(location.clone());
                        }
                    }
                }
                ir::Operation::Load { dst, .. } => {
                    variables.insert(dst.clone());
                }
                ir::Operation::Store { .. }
                | ir::Operation::Branch { .. }
                | ir::Operation::Call(_)
                | ir::Operation::Intrinsic(_)
                | ir::Operation::Return(_)
                | ir::Operation::Nop(_) => {}
            }
        }

        Ok(variables)
    }

    /// Given a program location, and the current state, create a set of
    /// constraints which capture information about variables which reach this
    /// point in the program.
    fn build_constraints(
        &self,
        ref_program_location: &ir::RefProgramLocation<ir::Constant>,
    ) -> Result<Vec<ir::Expression<ir::Constant>>> {
        let mut constraints = Vec::new();

        // The block to search within
        let block_index = ref_program_location.edge().unwrap().head();

        // locations we've already looked at
        let mut locations_considered: HashSet<ir::ProgramLocation> = HashSet::new();

        // We can only consider the most recent assignment to a variable
        let mut already_assigned: HashSet<ir::Variable> = HashSet::new();

        // queue of locations we still need to look at
        let mut queue: VecDeque<ir::ProgramLocation> = VecDeque::new();

        for program_location in self
            .use_def(&ref_program_location.clone().into())
            .locations()
        {
            queue.push_back(program_location.clone());
        }

        while !queue.is_empty() {
            let pl = queue.pop_front().unwrap();
            locations_considered.insert(pl.clone().into());

            let rpl = pl.apply(ref_program_location.function())?;

            if rpl
                .block()
                .map(|block| block.index() != block_index)
                .unwrap_or(true)
            {
                continue;
            }

            let operation = match rpl.instruction() {
                Some(instruction) => instruction.operation(),
                None => continue,
            };

            if !operation.is_assign() {
                continue;
            }
            let dst = operation.dst().unwrap();
            let src = operation.src().unwrap();

            if already_assigned.contains(dst) {
                continue;
            }
            already_assigned.insert(dst.clone());

            let constraint = ir::Expression::cmpeq(dst.clone().into(), src.clone())?;
            if constraint.contains_reference() {
                continue;
            }
            constraints.push(ir::reduce(&constraint)?);

            for location in self.use_def(&pl).locations() {
                if !locations_considered.contains(location) {
                    queue.push_back(location.clone());
                }
            }
        }

        Ok(constraints)
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

        let state = match location.function_location() {
            ir::RefFunctionLocation::Instruction(_, instruction) => match instruction.operation() {
                ir::Operation::Assign { dst, src } => {
                    let src = state.eval(&src.into())?;
                    state.set(dst.clone(), src);
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
                ir::Operation::Return(_) | ir::Operation::Store { .. } | ir::Operation::Nop(_) => {
                    state
                }
            },
            ir::RefFunctionLocation::Edge(edge) => {
                if let Some(condition) = edge.condition() {
                    let mut fast_solver = solver::FastSolver::new();
                    fast_solver.add_constraint(ir::reduce(condition)?);
                    fast_solver.append_constraints(self.build_constraints(location)?);
                    fast_solver.run()?;

                    // for constraint in fast_solver.constraints() {
                    //     println!(
                    //         "constraint: {} - {}",
                    //         constraint,
                    //         ir::reduce(&fast_solver.eval(constraint.clone())?)?
                    //     );
                    // }

                    for variable in self.block_guarded_variables(&location)? {
                        if let Some(constant) = fast_solver.variable(&variable) {
                            let strided_interval = StridedInterval::new(
                                Interval::new(
                                    Value::Value(constant.clone()),
                                    Value::Value(constant.clone()),
                                ),
                                1,
                            );
                            state.set(variable.clone(), strided_interval);
                            continue;
                        }

                        let variable_expression: ir::Expression<ir::Constant> =
                            variable.clone().into();

                        let lo =
                            solver::minimize(&fast_solver.constraints(), &variable_expression)?;
                        let hi =
                            solver::maximize(&fast_solver.constraints(), &variable_expression)?;

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

                        let strided_interval = match state.variable(&variable) {
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

        Ok(state)
    }

    fn join(&self, state0: State, state1: &State) -> Result<State> {
        state0.join(state1)
    }
}
