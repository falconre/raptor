//! Dead-Code Elimination

use analysis::{def_use, reaching_definitions};
use error::*;
use ir;
use std::collections::HashSet;

#[allow(dead_code)]
/// Eliminate dead code in an IL function
pub fn dead_code_elimination<V: ir::Value>(function: &ir::Function<V>) -> Result<ir::Function<V>> {
    let rd = reaching_definitions::reaching_definitions(function)?;

    // This is a set of assignments we will always consider used.
    let mut live: HashSet<ir::FunctionLocation> = HashSet::new();

    // Every assignment that reaches the last instruction in a block with no
    // successor, except stack variables
    function
        .blocks()
        .into_iter()
        .filter(|block| {
            function
                .control_flow_graph()
                .edges_out(block.index())
                .unwrap()
                .is_empty()
        })
        .for_each(|block| {
            let rfl = if let Some(instruction) = block.instructions().last() {
                ir::RefFunctionLocation::Instruction(block, instruction)
            } else {
                ir::RefFunctionLocation::EmptyBlock(block)
            };
            let rpl = ir::RefProgramLocation::new(function, rfl);

            rd.get(&rpl.into())
                .unwrap()
                .locations()
                .iter()
                .map(|location| location.apply(function).unwrap())
                .filter(|location| {
                    let instruction = location.instruction().unwrap();
                    let cond = !(instruction.operation().is_assign()
                        && instruction
                            .operation()
                            .dst()
                            .unwrap()
                            .stack_variable()
                            .is_some());
                    cond
                })
                .for_each(|location| {
                    live.insert(location.function_location().clone().into());
                });
        });

    for block in function.blocks() {
        for instruction in block.instructions() {
            match instruction.operation() {
                ir::Operation::Call(call) => {
                    // If this call does not have variables read set, then
                    // assignments which may reach here in reality may not reach
                    // here per analysis.
                    if call.variables_read().is_none() {
                        let rpl = ir::RefProgramLocation::new(function,
                            ir::RefFunctionLocation::Instruction(block, instruction));
                        // We killed all the assignments that reach here, so we
                        // need to go back up one instruction
                        rpl.backward()
                            .into_iter()
                            .for_each(|rpl| rd[&rpl.into()].locations()
                                .into_iter()
                                .map(|location| location.apply(function).unwrap())
                                // .inspect(|location| println!("{} is alive because {} has no variables_read",
                                //     location, call))
                                .for_each(|location| {
                                    live.insert(
                                        location.function_location()
                                            .clone()
                                            .into());
                                })
                            );
                    }
                }
                ir::Operation::Branch { .. } |
                ir::Operation::Intrinsic { .. } => {
                    let rpl = ir::RefProgramLocation::new(function,
                        ir::RefFunctionLocation::Instruction(block, instruction));
                    rd[&rpl.into()].locations()
                        .into_iter()
                        .for_each(|location| {
                            live.insert(location.function_location().clone());
                        });
                }
                ir::Operation::Assign { .. } |
                ir::Operation::Load { .. } |
                ir::Operation::Store { .. } |
                ir::Operation::Return(_) | // We may need to do same thing for return?
                ir::Operation::Nop => {}
            }
        }
    }

    let du = def_use(function)?;

    // Get every assignment with no uses, that isn't in live
    let kill = function
        .program_locations()
        .into_iter()
        // we will only remove certain types of instructions
        .filter(|location| {
            location
                .instruction()
                .map(|instruction| match instruction.operation() {
                    ir::Operation::Assign { .. } | ir::Operation::Load { .. } => true,
                    ir::Operation::Store { .. }
                    | ir::Operation::Branch { .. }
                    | ir::Operation::Call(_)
                    | ir::Operation::Intrinsic(_)
                    | ir::Operation::Return(_)
                    | ir::Operation::Nop => false,
                })
                .unwrap_or(false)
        })
        .map(|location| location.into())
        .filter(|location: &ir::ProgramLocation| !live.contains(location.function_location()))
        .filter(|location: &ir::ProgramLocation| du[&location].is_empty())
        .map(|location| location.function_location().clone())
        .collect::<Vec<ir::FunctionLocation>>();

    // Eliminate those instructions from our new function
    let kill: Vec<ir::FunctionLocation> =
        kill.into_iter().map(|location| location.into()).collect();

    let mut dce_function = function.clone();

    for k in kill {
        let instruction_index = k.instruction_index().unwrap();
        let block_index = k.block_index().unwrap();
        let block = dce_function.block_mut(block_index).unwrap();
        *block
            .instruction_mut(instruction_index)
            .ok_or("Failed to find instruction")?
            .operation_mut() = ir::Operation::Nop;
    }

    Ok(dce_function)
}
