//! Detect callee-saved variables.
//!
//! In assembly, callees often times save variables. We want to detect these,
//! so we can remove the code for the saving and restoring of these variables.

use crate::analysis::fixed_point::{fixed_point_forward, incoming_results, FixedPointAnalysis};
use crate::error::*;
use crate::ir;
use std::cmp::{Ordering, PartialOrd};
use std::collections::HashMap;

/// Returns a HashSet at each location. If a variable is present in that
/// HashSet, then it is transient at that point in the program.
pub fn transient_assignments<'f, V: ir::Value>(
    function: &'f ir::Function<V>,
) -> Result<HashMap<ir::ProgramLocation, TransientAssignments>> {
    let transient_assignment_analysis = TransientAssignmentAnalysis {};

    let result = fixed_point_forward(&transient_assignment_analysis, function)?;
    Ok(incoming_results(
        &transient_assignment_analysis,
        function,
        result,
        || TransientAssignments::new(),
    )?)
}

#[allow(dead_code)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TransientAssignment {
    Top,
    Variable(ir::Variable),
    Bottom,
}

impl TransientAssignment {
    pub fn variable(&self) -> Option<&ir::Variable> {
        match self {
            TransientAssignment::Variable(variable) => Some(variable),
            _ => None,
        }
    }

    fn join(&self, other: &TransientAssignment) -> TransientAssignment {
        match self {
            TransientAssignment::Top => TransientAssignment::Top,
            TransientAssignment::Variable(lhs) => match other {
                TransientAssignment::Top => TransientAssignment::Top,
                TransientAssignment::Variable(rhs) => {
                    if lhs == rhs {
                        self.clone()
                    } else {
                        TransientAssignment::Top
                    }
                }
                TransientAssignment::Bottom => self.clone(),
            },
            TransientAssignment::Bottom => other.clone(),
        }
    }
}

impl ::std::fmt::Display for TransientAssignment {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            TransientAssignment::Top => write!(f, "⊤"),
            TransientAssignment::Variable(variable) => variable.fmt(f),
            TransientAssignment::Bottom => write!(f, "⊥"),
        }
    }
}

impl PartialOrd for TransientAssignment {
    fn partial_cmp(&self, other: &TransientAssignment) -> Option<Ordering> {
        match self {
            TransientAssignment::Top => match other {
                TransientAssignment::Top => Some(Ordering::Equal),
                TransientAssignment::Variable(_) | TransientAssignment::Bottom => {
                    Some(Ordering::Less)
                }
            },
            TransientAssignment::Variable(lhs) => match other {
                TransientAssignment::Top => Some(Ordering::Less),
                TransientAssignment::Variable(rhs) => {
                    if lhs == rhs {
                        Some(Ordering::Equal)
                    } else {
                        None
                    }
                }
                TransientAssignment::Bottom => Some(Ordering::Greater),
            },
            TransientAssignment::Bottom => match other {
                TransientAssignment::Top | TransientAssignment::Variable(_) => Some(Ordering::Less),
                TransientAssignment::Bottom => Some(Ordering::Equal),
            },
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TransientAssignmentChain {
    chain: Vec<ir::ProgramLocation>,
    assignment: TransientAssignment,
}

impl TransientAssignmentChain {
    pub fn new(assignment: TransientAssignment) -> TransientAssignmentChain {
        TransientAssignmentChain {
            chain: Vec::new(),
            assignment: assignment,
        }
    }

    pub fn push_chain(&mut self, pl: ir::ProgramLocation) {
        // only if we have a value do we add to the chain
        if self.assignment().variable().is_some() {
            self.chain.push(pl);
        }
    }

    pub fn assignment(&self) -> &TransientAssignment {
        &self.assignment
    }
    pub fn chain(&self) -> &[ir::ProgramLocation] {
        &self.chain
    }

    fn join(&self, other: &TransientAssignmentChain) -> TransientAssignmentChain {
        let mut chain = self.chain.clone();
        for location in &other.chain {
            if !chain.contains(location) {
                chain.push(location.clone());
            }
        }

        TransientAssignmentChain {
            assignment: self.assignment.join(&other.assignment),
            chain: chain,
        }
    }

    fn new_top() -> TransientAssignmentChain {
        TransientAssignmentChain {
            chain: Vec::new(),
            assignment: TransientAssignment::Top,
        }
    }
}

impl PartialOrd for TransientAssignmentChain {
    fn partial_cmp(&self, other: &TransientAssignmentChain) -> Option<Ordering> {
        self.assignment().partial_cmp(other.assignment())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TransientAssignments {
    chains: HashMap<ir::Variable, TransientAssignmentChain>,
}

impl TransientAssignments {
    pub fn new() -> TransientAssignments {
        TransientAssignments {
            chains: HashMap::new(),
        }
    }

    /// Get the chains for a variable
    pub fn get(&self, variable: &ir::Variable) -> Option<&TransientAssignmentChain> {
        self.chains.get(variable)
    }

    pub fn chains(&self) -> &HashMap<ir::Variable, TransientAssignmentChain> {
        &self.chains
    }

    fn eval<V: ir::Value>(&self, expression: &ir::Expression<V>) -> TransientAssignmentChain {
        match expression {
            ir::Expression::LValue(lvalue) => match lvalue.as_ref() {
                ir::LValue::Variable(variable) => {
                    self.chains.get(variable).map(|v| v.clone()).unwrap_or(
                        TransientAssignmentChain::new(TransientAssignment::Variable(
                            variable.clone(),
                        )),
                    )
                }
                ir::LValue::Dereference(_) => {
                    TransientAssignmentChain::new(TransientAssignment::Top)
                }
            },
            _ => TransientAssignmentChain::new(TransientAssignment::Top),
        }
    }

    fn join(mut self, other: &TransientAssignments) -> TransientAssignments {
        for (variable, tac) in &other.chains {
            self.chains
                .entry(variable.clone())
                .and_modify(|v| *v = v.join(&tac))
                .or_insert(tac.clone());
        }
        self
    }

    fn set(
        &mut self,
        variable: ir::Variable,
        transient_assignment_chain: TransientAssignmentChain,
    ) {
        self.chains.insert(variable, transient_assignment_chain);
    }

    fn top(&mut self) {
        self.chains
            .iter_mut()
            .for_each(|(_, tac)| *tac = TransientAssignmentChain::new_top());
    }
}

// This is copied essentially verbatim from Constants::PartialOrd, so if this
// is wrong, go look there
impl PartialOrd for TransientAssignments {
    fn partial_cmp(&self, other: &TransientAssignments) -> Option<Ordering> {
        if self.chains.len() < other.chains.len() {
            for (lv, lt) in self.chains.iter() {
                if !other.chains.get(lv).map(|rt| lt <= rt).unwrap_or(false) {
                    return None;
                }
            }
            Some(Ordering::Less)
        } else if self.chains.len() > other.chains.len() {
            for (lv, lt) in other.chains.iter() {
                if !self.chains.get(lv).map(|rt| lt <= rt).unwrap_or(false) {
                    return None;
                }
            }
            Some(Ordering::Greater)
        } else {
            let mut order = Ordering::Equal;
            for (lv, lt) in &self.chains {
                match other.chains.get(lv) {
                    Some(rt) => {
                        if rt < lt {
                            if order <= Ordering::Equal {
                                order = Ordering::Less;
                            } else {
                                return None;
                            }
                        } else if lt > rt {
                            if order >= Ordering::Greater {
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

struct TransientAssignmentAnalysis {}

impl<'f, V: 'f + ir::Value> FixedPointAnalysis<'f, TransientAssignments, V>
    for TransientAssignmentAnalysis
{
    fn trans(
        &self,
        location: &ir::RefProgramLocation<'f, V>,
        state: Option<TransientAssignments>,
    ) -> Result<TransientAssignments> {
        let mut state = match state {
            Some(state) => state,
            None => TransientAssignments::new(),
        };

        let state = match location.instruction() {
            Some(instruction) => match instruction.operation() {
                ir::Operation::Assign { dst, src } => {
                    let mut src = state.eval(src);
                    src.push_chain(location.clone().into());
                    state.set(dst.to_owned(), src);
                    state
                }
                ir::Operation::Load { dst, .. } => {
                    state.set(dst.to_owned(), TransientAssignmentChain::new_top());
                    state
                }
                ir::Operation::Call(call) => {
                    if let Some(arguments) = call.arguments() {
                        arguments
                            .into_iter()
                            .filter(|argument| argument.variable().is_some())
                            .map(|argument| argument.variable().unwrap())
                            .for_each(|argument| {
                                state.set(argument.clone(), TransientAssignmentChain::new_top())
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
                            state.set(scalar.clone().into(), TransientAssignmentChain::new_top())
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

    fn join(
        &self,
        state0: TransientAssignments,
        state1: &TransientAssignments,
    ) -> Result<TransientAssignments> {
        Ok(state0.join(state1))
    }
}
