//! Parameter Identification
//!
//! Rules for being a parameter
//! MUST
//!   * Be read before being written
//! MUST NOT
//!   * Be a StackVariable with an offset below 0
//!   * Originate from a reference to a stack variable with an offset below 0
//!

use analysis::variable_use_def;
use error::*;
use ir;
use std::collections::HashSet;





/// Attempt to identify the variables and stack arguments which are the incoming
/// parameters to this function.
pub fn parameter_identification<V: ir::Value>(
    function: &ir::Function<V>
) -> Result<Vec<ir::Variable>> {
    let vud = variable_use_def(function)?;

    let mut parameters = HashSet::new();

    for (location, vud) in vud {
        let rpl = location.apply(function)?;
        let variables_read =
            rpl.instruction()
                .map(|instruction| variables_read(instruction.operation()))
                .unwrap_or(
                    rpl.edge().and_then(|edge|
                        edge.condition()
                            .map(|c| get_variables(c))));

        let variables_read = match variables_read {
            Some(variables_read) => variables_read,
            None => continue
        };

        for variable_read in variables_read {
            if vud[variable_read].is_empty() {
                parameters.insert(variable_read.to_owned());
            }
        }
    }

    // Filter out any stack variables which are below the incoming stack
    // pointer value
    let parameters =
        parameters.into_iter()
            .filter(|variable|
                variable.stack_variable()
                    .map(|stack_variable| stack_variable.offset() >= 0)
                    .unwrap_or(true))
            .collect::<Vec<ir::Variable>>();

    Ok(parameters)
}


// We compute


fn variables_read<V: ir::Value>(o: &ir::Operation<V>)
    -> Option<Vec<&ir::Variable>> {

    match o {
        ir::Operation::Assign { src, .. } => Some(get_variables(src)),
        ir::Operation::Store { index, src } =>
            Some(get_variables(index).into_iter()
                    .chain(get_variables(src).into_iter())
                    .collect()),
        ir::Operation::Load { index, .. } => Some(get_variables(index)),
        ir::Operation::Branch { target } => Some(get_variables(target)),
        ir::Operation::Call(call) =>
            call.arguments()
                .map(|arguments|
                    arguments.into_iter().fold(Vec::new(), |mut v, argument| {
                        v.append(&mut get_variables(argument));
                        v
                    })),
        ir::Operation::Intrinsic(_) => None,
        ir::Operation::Return(result) =>
            result.as_ref().map(|e| get_variables(e)),
        ir::Operation::Nop => Some(Vec::new())
    }
}


// We are going to exclude some variables here that we are looking for.
// Variables which are behind a reference won't be included in the variables
// we count as incoming arguments.
fn get_variables<V: ir::Value>(e: &ir::Expression<V>) -> Vec<&ir::Variable> {
    let mut variables = match e {
        ir::Expression::LValue(lvalue) => match lvalue.as_ref() {
            ir::LValue::Variable(variable) => vec![variable],
            ir::LValue::Dereference(dereference) =>
                get_variables(dereference.expression())
        },
        ir::Expression::RValue(rvalue) => match rvalue.as_ref() {
            ir::RValue::Value(_) |
            ir::RValue::Reference(_) => Vec::new()
        },
        ir::Expression::Add(lhs, rhs) |
        ir::Expression::Sub(lhs, rhs) |
        ir::Expression::Mul(lhs, rhs) |
        ir::Expression::Divu(lhs, rhs) |
        ir::Expression::Modu(lhs, rhs) |
        ir::Expression::Divs(lhs, rhs) |
        ir::Expression::Mods(lhs, rhs) |
        ir::Expression::And(lhs, rhs) |
        ir::Expression::Or(lhs, rhs) |
        ir::Expression::Xor(lhs, rhs) |
        ir::Expression::Shl(lhs, rhs) |
        ir::Expression::Shr(lhs, rhs) |
        ir::Expression::Cmpeq(lhs, rhs) |
        ir::Expression::Cmpneq(lhs, rhs) |
        ir::Expression::Cmplts(lhs, rhs) |
        ir::Expression::Cmpltu(lhs, rhs) => {
            let mut variables = get_variables(lhs);
            variables.append(&mut get_variables(rhs));
            variables
        },
        ir::Expression::Trun(_, rhs) |
        ir::Expression::Sext(_, rhs) |
        ir::Expression::Zext(_, rhs) => get_variables(rhs),
        ir::Expression::Ite(cond, then, else_) => {
            let mut variables = get_variables(cond);
            variables.append(&mut get_variables(then));
            variables.append(&mut get_variables(else_));
            variables
        }
    };

    variables.sort();
    variables.dedup();
    variables
}
