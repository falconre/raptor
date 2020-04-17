/// This provides a lightweight solver which can solve simple sets of
/// expressions. We use this to avoid invoking heavier solvers (like z3) on
/// simple things.
use crate::error::*;
use crate::ir;
use std::collections::{HashMap, VecDeque};

#[derive(Debug, Eq, PartialEq)]
pub enum Value {
    Top,
    Constant(ir::Constant),
}

pub struct FastSolver {
    constraints: Vec<ir::Expression<ir::Constant>>,
    variables: HashMap<ir::Variable, Value>,
}

impl FastSolver {
    pub fn new() -> FastSolver {
        FastSolver {
            constraints: Vec::new(),
            variables: HashMap::new(),
        }
    }

    pub fn constraints(&self) -> &[ir::Expression<ir::Constant>] {
        &self.constraints
    }

    pub fn add_constraint(&mut self, constraint: ir::Expression<ir::Constant>) {
        self.constraints.push(constraint);
    }

    pub fn append_constraints(&mut self, mut constraints: Vec<ir::Expression<ir::Constant>>) {
        self.constraints.append(&mut constraints);
    }

    fn variables(&self) -> &HashMap<ir::Variable, Value> {
        &self.variables
    }

    pub fn variable(&self, variable: &ir::Variable) -> Option<&ir::Constant> {
        self.variables.get(variable).and_then(|value| match value {
            Value::Top => None,
            Value::Constant(constant) => Some(constant),
        })
    }

    pub fn eval(
        &self,
        mut expression: ir::Expression<ir::Constant>,
    ) -> Result<ir::Expression<ir::Constant>> {
        for (variable, value) in self.variables() {
            if let Value::Constant(constant) = value {
                expression = expression.replace_variable(variable, &constant.clone().into())?;
            }
        }
        Ok(expression)
    }

    // If this expression results in a variable being set to a constant, return
    // that variable and the constant
    fn solve_constant(
        &self,
        expression: &ir::Expression<ir::Constant>,
    ) -> Result<Option<(ir::Variable, ir::Constant)>> {
        match expression {
            ir::Expression::Cmpeq(lhs, rhs) => {
                if let Some(variable) = lhs.variable() {
                    if rhs.is_constant() {
                        let constant = ir::eval(rhs)?;
                        return Ok(Some((variable.clone(), constant)));
                    }
                }
            }
            _ => {}
        }

        Ok(None)
    }

    pub fn run(&mut self) -> Result<()> {
        let mut queue = VecDeque::new();

        for constraint in self.constraints() {
            queue.push_back(constraint.clone());
        }

        loop {
            // We run until the length of our queue stabilizes
            let queue_len = queue.len();

            // We also run again if we Top any variables that weren't already
            // Top
            let mut variable_topped = false;

            // These are the constraints we didn't solve for
            let mut next_queue = VecDeque::new();

            while !queue.is_empty() {
                let constraint = queue.pop_front().unwrap();

                let constraint = self.eval(ir::reduce(&constraint)?)?;

                let (variable, constant) = if constraint.bits() == 1 && constraint.is_variable() {
                    (
                        constraint.variable().unwrap().clone(),
                        ir::Constant::new(1, 1),
                    )
                } else if let Some((variable, constant)) = self.solve_constant(&constraint)? {
                    (variable, constant)
                } else {
                    next_queue.push_back(constraint);
                    continue;
                };

                let value = if let Some(value) = self.variables.get(&variable) {
                    match value {
                        Value::Top => Value::Top,
                        Value::Constant(existing_constant) => {
                            if *existing_constant == constant {
                                Value::Constant(constant)
                            } else {
                                variable_topped = true;
                                Value::Top
                            }
                        }
                    }
                } else {
                    Value::Constant(constant)
                };
                self.variables.insert(variable, value);
            }

            if next_queue.len() == queue_len {
                if !variable_topped {
                    break;
                }
            }

            while !next_queue.is_empty() {
                queue.push_back(next_queue.pop_front().unwrap());
            }
        }

        Ok(())
    }
}
