use crate::analysis::stack_pointer_offsets::StackPointerOffsets;
use crate::data::{ParameterType, FUNCTIONS};
use crate::error::*;
use crate::translator::TranslationInformation;
use crate::{analysis, ir};
use falcon::analysis::calling_convention::ArgumentType;
use std::collections::{HashMap, HashSet};

// Functions that are annotated as no_return should have that reflected
// in the graph.
pub fn no_returns(function: &mut ir::Function<ir::Constant>) -> Result<()> {
    // First, we need to know where every call to a noreturn function is

    let no_returns = {
        let mut no_returns = Vec::new();
        for block in function.blocks() {
            for instruction in block.instructions() {
                match instruction.operation() {
                    ir::Operation::Call(call) => {
                        if call
                            .target()
                            .symbol()
                            .and_then(|symbol| FUNCTIONS.get(symbol))
                            .map(|function_declaration| function_declaration.no_return())
                            .unwrap_or(false)
                        {
                            no_returns.push(ir::FunctionLocation::Instruction(
                                block.index(),
                                instruction.index(),
                            ));
                        }
                    }
                    _ => continue,
                }
            }
        }
        no_returns
    };

    // We hope that this no_return is at the end of a block, because any
    // instruction which comes after it is unreachable, and if there are
    // more instructions in the block... then apparently the compiler thought
    // the function would return, or something else is oging on.
    //
    // In taht case, all we need to do is drop out-going edges from the block
    for no_return in no_returns {
        // verify this is the last instruction in the block
        if function
            .block(no_return.block_index().unwrap())
            .unwrap()
            .instructions()
            .last()
            .unwrap()
            .index()
            == no_return.instruction_index().unwrap()
        {
            let edges_out = function
                .control_flow_graph()
                .graph()
                .edges_out(no_return.block_index().unwrap())
                .map(|edges| {
                    edges
                        .into_iter()
                        .map(|edge| edge.tail())
                        .collect::<Vec<usize>>()
                })
                .unwrap_or(Vec::new());
            edges_out.into_iter().for_each(|tail| {
                function
                    .control_flow_graph_mut()
                    .graph_mut()
                    .remove_edge(no_return.block_index().unwrap(), tail)
                    .expect("Failed to remove edge in calls::noreturns")
            });
        }
    }

    Ok(())
}

/// Detect branch instructions which are calls, and turn them into proper
/// call instructions. Do the same for returns.
pub fn call_sites<'f>(
    function: &'f ir::Function<ir::Constant>,
    translation_information: &TranslationInformation,
) -> Result<ir::Function<ir::Constant>> {
    // Get in there, find those call sites
    let call_type = analysis::detect_call_information::CallType::new_arch(
        translation_information.architecture(),
        translation_information.calling_convention(),
    );

    let (call_sites, return_sites) =
        analysis::detect_call_information::detect_call_information(&function, &call_type)?;

    let mut new_function: ir::Function<ir::Constant> = function.clone();

    // Get those calls right
    for call_site in call_sites {
        // remove instructions which are used to set the return address
        for location in call_site.set_return_address() {
            let block_index = location.block().unwrap().index();
            let block = new_function.block_mut(block_index)?;
            block.replace_with_nop(location.instruction().unwrap().index())?;
        }

        let block_index = call_site.branch().block().unwrap().index();
        let block = new_function.block_mut(block_index).unwrap();
        let instruction_index = call_site.branch().instruction().unwrap().index();
        let instruction = block.instruction_mut(instruction_index).unwrap();

        let target_expression = call_site
            .branch()
            .instruction()
            .unwrap()
            .operation()
            .target()
            .unwrap()
            .clone();

        let mut call = ir::Call::new(ir::CallTarget::Expression(target_expression));

        call.set_variables_written(Some(
            translation_information
                .calling_convention()
                .trashed_registers()
                .iter()
                .map(|scalar| scalar.clone().into())
                .collect(),
        ));

        *instruction.operation_mut() = ir::Operation::Call(call);
    }

    for return_site in return_sites {
        // remove instructions which are used to set the return address
        for location in return_site.get_return_address() {
            let block_index = location.block().unwrap().index();
            let block = new_function.block_mut(block_index)?;
            block.replace_with_nop(location.instruction().unwrap().index())?;
        }

        // Replace branch operation at site of return with return operation
        let block_index = return_site.branch().block().unwrap().index();
        let block = new_function.block_mut(block_index)?;
        let instruction = block
            .instruction_mut(return_site.branch().instruction().unwrap().index())
            .unwrap();
        *instruction.operation_mut() = ir::Operation::Return(None);
    }

    Ok(new_function)
}

/// Given a function, and its argument types, return the variable types for
/// the function arguments
pub fn get_argument_variables(
    arguments: &[ParameterType],
    translation_information: &TranslationInformation,
    stack_pointer_offsets: &StackPointerOffsets,
) -> Result<Option<Vec<ir::Variable>>> {
    let mut result: Vec<ir::Variable> = Vec::new();

    let word_size = translation_information.architecture().word_size();

    for i in 0..arguments.len() {
        match translation_information
            .calling_convention()
            .argument_type(i)
        {
            ArgumentType::Register(scalar) => result.push(scalar.clone().into()),
            ArgumentType::Stack(offset) => {
                let stack_pointer = translation_information.architecture().stack_pointer();
                let stack_pointer: ir::Scalar = stack_pointer.into();
                let mut expr = ir::Expression::add(
                    ir::expr_const(offset as u64, stack_pointer.bits()),
                    stack_pointer.into(),
                )?;

                // If there was a return address on the stack, it went away
                // when we converted branches to calls, so we need to account
                // for that.
                if translation_information
                    .calling_convention()
                    .return_address_type()
                    .stack()
                    .is_some()
                {
                    let word_size = translation_information.architecture().word_size();
                    expr = ir::Expression::sub(
                        expr,
                        ir::expr_const((word_size / 8) as u64, word_size as usize),
                    )?;
                }

                let expr = ir::reduce(&stack_pointer_offsets.replace(&expr, word_size)?)?;

                if let Some(stack_pointer) = expr.stack_pointer() {
                    result.push(
                        ir::StackVariable::new(
                            stack_pointer.offset(),
                            translation_information
                                .calling_convention()
                                .stack_argument_length()
                                * 8,
                        )
                        .into(),
                    );
                } else {
                    // panic!("can't determine argument variable");
                    return Ok(None);
                }
            }
        }
    }

    Ok(Some(result))
}

/// Applies the semantics from external functions held in the data section to
/// a function.
///
/// Returns true if the function was modified in any way.
pub fn apply_functions(
    function: &mut ir::Function<ir::Constant>,
    translation_information: &TranslationInformation,
    stack_pointer_offsets: &HashMap<ir::ProgramLocation, StackPointerOffsets>,
) -> Result<()> {
    let function_index: usize = function.index().unwrap();

    for block in function.blocks_mut() {
        let block_index = block.index();
        for instruction in block.instructions_mut() {
            let function_location =
                ir::FunctionLocation::Instruction(block_index, instruction.index());
            let program_location = ir::ProgramLocation::new(function_index, function_location);
            let stack_pointer_offset = &stack_pointer_offsets[&program_location];

            let call = match instruction.operation_mut() {
                ir::Operation::Call(ref mut call) => call,
                _ => {
                    continue;
                }
            };
            // all symbols which currently map to expressions, see if we can
            // resolve symbols
            if let Some(expression) = call.target().expression().cloned() {
                let address = ir::eval(&expression)
                    .ok()
                    .and_then(|constant| constant.value_u64());
                let address = match address {
                    Some(address) => address,
                    None => {
                        continue;
                    }
                };

                if translation_information.symbol(address).is_some() {
                    *call = ir::Call::new(ir::CallTarget::Symbol(
                        translation_information
                            .symbol(address)
                            .unwrap()
                            .name()
                            .to_string(),
                    ));
                    call.set_variables_written(Some(
                        translation_information
                            .calling_convention()
                            .trashed_registers()
                            .iter()
                            .map(|scalar| scalar.clone().into())
                            .collect(),
                    ));
                }
            }
            // We perform additional actions when we turn a call into a call to
            // a symbol for the first time. If we haven't done that this loop
            // iteration, go to the next call.
            else {
                continue;
            }

            // all symbols which map to predefined functions, see if we can turn
            // them into proper calls
            let symbol_name = match call.target().symbol().map(|s| s.to_string()) {
                Some(symbol) => symbol,
                None => continue,
            };

            if let Some(function_declaration) = FUNCTIONS.get(&symbol_name) {
                let arguments = get_argument_variables(
                    function_declaration.parameters(),
                    translation_information,
                    &stack_pointer_offset,
                )?;

                if arguments.is_none() {
                    continue;
                }

                call.set_empty_arguments();

                if let Some(arguments) = arguments {
                    for argument in arguments {
                        call.push_argument(argument.into());
                    }
                }
                call.set_variables_written(Some(
                    translation_information
                        .calling_convention()
                        .trashed_registers()
                        .iter()
                        .map(|scalar| scalar.clone().into())
                        .collect(),
                ));
            }
        }
    }

    Ok(())
}

pub fn get_parameter(
    position: usize,
    translation_information: &TranslationInformation,
) -> ir::Variable {
    match translation_information
        .calling_convention()
        .argument_type(position)
    {
        ArgumentType::Register(register) => register.into(),
        ArgumentType::Stack(offset) => ir::StackVariable::new(
            offset as isize,
            translation_information
                .calling_convention()
                .stack_argument_length(),
        )
        .into(),
    }
}

pub fn set_function_parameters<V: ir::Value>(
    function: &mut ir::Function<V>,
    translation_information: &TranslationInformation,
) -> Result<()> {
    let mut initial_parameters: HashSet<ir::Variable> =
        analysis::parameter_identification(function)?
            .into_iter()
            .collect();

    let mut final_parameters: Vec<ir::Variable> = Vec::new();

    for i in 0..initial_parameters.len() {
        let potential_parameter = get_parameter(i, translation_information);
        if initial_parameters.contains(&potential_parameter) {
            initial_parameters.remove(&potential_parameter);
            final_parameters.push(potential_parameter);
        } else {
            break;
        }
    }

    final_parameters.sort();

    let mut remaining_parameters = initial_parameters
        .into_iter()
        .collect::<Vec<ir::Variable>>();

    remaining_parameters.sort();
    final_parameters.append(&mut remaining_parameters);

    function.set_parameters(Some(final_parameters));

    Ok(())
}
