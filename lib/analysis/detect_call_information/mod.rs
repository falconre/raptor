//! Automatically detect inter-procedural call information.

use analysis::transient_assignments::{transient_assignments, TransientAssignments};
use error::*;
use ir;
use std::collections::HashMap;

mod block_state;
mod call_site;
mod call_type;
mod return_site;
mod tagged_constant;
pub use self::block_state::BlockState;
pub use self::call_site::CallSite;
pub use self::call_type::CallType;
pub use self::return_site::ReturnSite;
pub use self::tagged_constant::TaggedConstant;

fn detect_call<'f>(
    location: &ir::RefFunctionLocation<'f, ir::Constant>,
    function: &'f ir::Function<ir::Constant>,
    block: &'f ir::Block<ir::Constant>,
    instruction: &'f ir::Instruction<ir::Constant>,
    block_state: &BlockState<'f>,
    call_type: &CallType,
) -> Option<CallSite<'f>> {
    // First, we need to find the address of the instruction
    // following this one
    let next_instruction = {
        let next_instructions =
            ir::RefProgramLocation::new(function, location.clone()).next_instructions();
        if next_instructions.len() != 1 {
            return None;
        }
        next_instructions
            .get(0)
            .unwrap()
            .instruction()
            .unwrap()
            .clone()
    };

    match call_type.return_type() {
        ReturnType::Register(variable) => {
            let return_register = match block_state.get(variable) {
                Some(return_register) => return_register,
                None => return None,
            };
            let return_address = return_register.constant().value_u64().unwrap();
            if return_address == next_instruction.address().unwrap() {
                return Some(CallSite::new(
                    location.clone(),
                    return_register.locations().to_vec(),
                ));
            }
        }
        ReturnType::PushStack => {
            let stack_pointer = match block_state.get(call_type.stack_pointer()) {
                Some(stack_pointer) => stack_pointer,
                None => return None,
            };
            let stack_value = match block_state
                .load(&stack_pointer.constant(), stack_pointer.constant().bits())
            {
                Some(stack_pointer) => stack_pointer,
                None => return None,
            };
            // just get every other instruction with the same instruction address,
            // that isn't this call
            let mut other_instructions = Vec::new();
            for ins in block.instructions() {
                if ins.address().unwrap() == instruction.address().unwrap()
                    && ins.index() != instruction.index()
                {
                    other_instructions.push(ir::RefFunctionLocation::Instruction(block, ins));
                }
            }
            other_instructions.append(&mut stack_value.locations().to_vec());
            other_instructions.sort();
            other_instructions.dedup();
            if stack_value.constant().value_u64().unwrap() == next_instruction.address().unwrap() {
                return Some(CallSite::new(location.clone(), other_instructions));
            }
        }
    }

    None
}

fn detect_return<'f>(
    location: &ir::RefFunctionLocation<'f, ir::Constant>,
    function: &'f ir::Function<ir::Constant>,
    instruction: &'f ir::Instruction<ir::Constant>,
    call_type: &CallType,
    transient_assignments: &HashMap<ir::ProgramLocation, TransientAssignments>,
) -> Option<ReturnSite<'f>> {
    match call_type.return_type() {
        ReturnType::Register(variable) => {
            // Make sure we are branching to the target variable
            let target = instruction.operation().target().unwrap();
            let is_target_variable = target
                .variable()
                .map(|target_variable| target_variable == variable)
                .unwrap_or(false);
            if is_target_variable {
                return Some(ReturnSite::new(location.clone(), Vec::new()));
            }
        }
        ReturnType::PushStack => {
            // Get the target for this branch
            let target = instruction.operation().target().unwrap();
            let target_variable = target.variable()?;

            // Check to see if it is transient up to a stack variable with
            // an offset of 0
            let rpl = ir::RefProgramLocation::new(function, location.clone());
            let pl: ir::ProgramLocation = rpl.into();
            let transient_assignment_chain = transient_assignments[&pl].get(target_variable)?;
            let offset = transient_assignment_chain
                .assignment()
                .variable()?
                .stack_variable()?
                .offset();
            if offset == 0 {
                return Some(ReturnSite::new(location.clone(), Vec::new()));
            }
        }
    }

    None
}

pub fn detect_call_information<'f>(
    function: &'f ir::Function<ir::Constant>,
    call_type: &'f CallType,
) -> Result<(Vec<CallSite<'f>>, Vec<ReturnSite<'f>>)> {
    let mut call_sites: Vec<CallSite<'f>> = Vec::new();
    let mut return_sites: Vec<ReturnSite<'f>> = Vec::new();

    let transient_assignments = transient_assignments(function)?;

    // For every block
    for block in function.blocks() {
        let mut block_state: BlockState = BlockState::new();
        block_state.set(
            call_type.stack_pointer().clone(),
            Some(TaggedConstant::new(
                ir::const_(0xff000000, call_type.stack_pointer().bits()),
                vec![ir::RefFunctionLocation::from_block(block)],
            )),
        )?;

        for instruction in block.instructions() {
            let rfl: ir::RefFunctionLocation<'f, ir::Constant> =
                ir::RefFunctionLocation::Instruction(block, instruction);

            match instruction.operation() {
                ir::Operation::Assign { dst, src } => {
                    let src = block_state
                        .eval(src)?
                        .map(|src| TaggedConstant::new(src, vec![rfl.into()]));
                    block_state.set(dst.clone(), src)?;
                }
                ir::Operation::Load { dst, index } => {
                    let constant = block_state
                        .eval(&index)?
                        .and_then(|index| block_state.load(&index, dst.bits()))
                        .map(|tagged_value| tagged_value.constant().clone());

                    block_state.set(
                        dst.clone(),
                        constant.map(|constant| TaggedConstant::new(constant, vec![rfl.into()])),
                    )?;
                }
                ir::Operation::Store { index, src } => {
                    if let Some(index) = block_state.eval(index)? {
                        let src = block_state.eval(src)?;
                        block_state
                            .store(index, src.map(|src| TaggedConstant::new(src, vec![rfl])));
                    } else {
                        block_state.clear_memory();
                    }
                }
                ir::Operation::Branch { .. } => {
                    if let Some(call_site) =
                        detect_call(&rfl, function, block, instruction, &block_state, call_type)
                    {
                        call_sites.push(call_site);
                    }
                    if let Some(return_site) = detect_return(
                        &rfl,
                        function,
                        instruction,
                        call_type,
                        &transient_assignments,
                    ) {
                        return_sites.push(return_site);
                    }
                }
                ir::Operation::Call { .. }
                | ir::Operation::Intrinsic { .. }
                | ir::Operation::Return(_)
                | ir::Operation::Nop => {}
            }
        }
    }

    Ok((call_sites, return_sites))
}

pub enum ReturnType {
    Register(ir::Variable),
    PushStack,
}
