use crate::modules::Module;
use analysis;
use analysis::stack_pointer_offsets::StackPointerOffsets;
use error::*;
use falcon::il;
use ir;
use std::collections::HashMap;

use translator::{calls, TranslationInformation};

pub struct FunctionTranslator<'t> {
    translation_information: TranslationInformation<'t>,
    modules: Vec<Box<Module>>,
}

impl<'t> FunctionTranslator<'t> {
    pub fn new(translation_information: TranslationInformation<'t>) -> FunctionTranslator<'t> {
        FunctionTranslator {
            translation_information: translation_information,
            modules: Vec::new(),
        }
    }

    pub fn ti(&self) -> &TranslationInformation {
        &self.translation_information
    }

    pub fn modules(&self) -> &[Box<Module>] {
        &self.modules
    }

    pub fn stack_pointers<'f>(
        &self,
        function: &'f ir::Function<ir::Constant>,
        stack_pointer_offsets: &HashMap<ir::ProgramLocation, StackPointerOffsets>,
    ) -> Result<ir::Function<ir::Constant>> {
        let word_size = self.ti().architecture().word_size();

        let mut new_function: ir::Function<ir::Constant> = function.clone();

        // first pass, we set the stack pointer equal to its offset
        for ref mut block in new_function.blocks_mut() {
            let ref_block = function.block(block.index()).expect("Failed to get block");
            for ref mut instruction in block.instructions_mut() {
                let ref_instruction = ref_block
                    .instruction(instruction.index())
                    .expect("Failed to get instruction");
                let rfl = ir::RefFunctionLocation::Instruction(ref_block, ref_instruction);
                let rpl = ir::RefProgramLocation::new(&function, rfl);
                let spo = match stack_pointer_offsets.get(&rpl.into()) {
                    Some(spo) => spo,
                    None => continue,
                };
                // let spo = &stack_pointer_offsets[&rpl];

                match instruction.operation_mut() {
                    ir::Operation::Assign { src, .. } => {
                        *src = spo.replace(src, word_size)?;
                    }
                    ir::Operation::Store { index, .. } | ir::Operation::Load { index, .. } => {
                        *index = spo.replace(index, word_size)?;
                    }
                    ir::Operation::Branch { .. }
                    | ir::Operation::Call { .. }
                    | ir::Operation::Intrinsic { .. }
                    | ir::Operation::Return(_)
                    | ir::Operation::Nop => {}
                }
            }
        }

        Ok(new_function)
    }

    pub fn reduce_function(&self, function: &mut ir::Function<ir::Constant>) -> Result<()> {
        // second pass, we reduce all expressions
        for ref mut block in function.blocks_mut() {
            for ref mut instruction in block.instructions_mut() {
                for expression in instruction.expressions_mut() {
                    *expression = ir::reduce(expression)?;
                }
            }
        }

        Ok(())
    }

    pub fn stack_variables(&self, function: &mut ir::Function<ir::Constant>) {
        // third pass, we create stack variables from stores/loads to/from
        // pure stack pointers
        for ref mut block in function.blocks_mut() {
            for ref mut instruction in block.instructions_mut() {
                let operation = {
                    let operation = instruction.operation();
                    match operation {
                        ir::Operation::Store { index, src } => {
                            if let Some(stack_pointer) = index.stack_pointer().cloned() {
                                ir::Operation::Assign {
                                    dst: ir::StackVariable::new(stack_pointer.offset(), src.bits())
                                        .into(),
                                    src: src.clone(),
                                }
                            } else {
                                operation.clone()
                            }
                        }
                        ir::Operation::Load { dst, index } => {
                            if let Some(stack_pointer) = index.stack_pointer().cloned() {
                                ir::Operation::Assign {
                                    dst: dst.clone(),
                                    src: ir::StackVariable::new(stack_pointer.offset(), dst.bits())
                                        .into(),
                                }
                            } else {
                                operation.clone()
                            }
                        }
                        _ => operation.clone(),
                    }
                };
                *instruction.operation_mut() = operation;
            }
        }
    }

    pub fn constants(
        &self,
        function: &ir::Function<ir::Constant>,
    ) -> Result<ir::Function<ir::Constant>> {
        let mut new_function = function.clone();
        let constants = analysis::constants::constants(&function, Some(self.ti().backing()))?;

        for ref mut block in new_function.blocks_mut() {
            let ref_block = function.block(block.index()).expect("Failed to get block");
            for ref mut instruction in block.instructions_mut() {
                let ref_instruction = ref_block
                    .instruction(instruction.index())
                    .expect("Failed to get instruction");
                let rfl = ir::RefFunctionLocation::Instruction(ref_block, ref_instruction);
                let rpl = ir::RefProgramLocation::new(&function, rfl);
                let pl: ir::ProgramLocation = rpl.into();

                match instruction.operation_mut() {
                    ir::Operation::Assign { src, .. } => {
                        *src = constants[&pl].reduce(src)?;
                    }
                    ir::Operation::Store { index, .. } | ir::Operation::Load { index, .. } => {
                        *index = constants[&pl].reduce(index)?;
                    }
                    ir::Operation::Branch { target } => {
                        *target = constants[&pl].reduce(target)?;
                    }
                    ir::Operation::Call(call) => {
                        if let Some(expression) = call.target_mut().expression_mut() {
                            *expression = constants[&pl].reduce(expression)?;
                        }
                        if let Some(arguments_mut) = call.arguments_mut() {
                            for argument in arguments_mut.iter_mut() {
                                *argument = constants[&pl].reduce(argument)?;
                            }
                        }
                    }
                    ir::Operation::Return(result) => {
                        if let Some(expression) = result {
                            *expression = constants[&pl].reduce(expression)?;
                        }
                    }
                    ir::Operation::Intrinsic { .. } | ir::Operation::Nop => {}
                }
            }
        }

        Ok(new_function)
    }

    pub fn remove_outgoing_transient_assignments(
        &self,
        function: &ir::Function<ir::Constant>,
    ) -> Result<ir::Function<ir::Constant>> {
        use analysis::{def_use, transient_assignments};

        let mut new_function = function.clone();

        // Get the locations of all return instructions
        let returns = function.locations().into_iter().filter(|location| {
            location
                .instruction()
                .map(|instruction| instruction.operation().is_return())
                .unwrap_or(false)
        });

        let returns = returns
            .map(|function_location| ir::RefProgramLocation::new(function, function_location));

        // Get the transient assignments
        let transient_assignments = transient_assignments::transient_assignments(function)?;

        // And the def use
        let du = def_use(function)?;

        // For each transient assignment at a return location
        for return_location in returns {
            let chains = transient_assignments[&return_location.into()].chains();
            'chain: for (variable, tac) in chains {
                // Is this a real transient assignment?
                if tac
                    .assignment()
                    .variable()
                    .map(|v| v != variable)
                    .unwrap_or(true)
                {
                    continue;
                }

                // Has the last assignment been used?
                let last_assignment = tac.chain().last().unwrap();
                if !du[last_assignment].is_empty() {
                    continue 'chain;
                }

                // We can safely nop all instances of this transient variable
                {
                    let block_index = last_assignment.function_location().block_index().unwrap();
                    let block = new_function.block_mut(block_index)?;
                    let instruction_index = last_assignment
                        .function_location()
                        .instruction_index()
                        .unwrap();
                    let instruction = block.instruction_mut(instruction_index).ok_or(format!(
                        "Could not get mutable instruction for \
                            function name: {},
                            block index: {},
                            instruction index: {}",
                        function.name(),
                        block_index,
                        instruction_index
                    ))?;
                    *instruction.operation_mut() = ir::Operation::Nop;
                }

                new_function.add_transient_variable(variable.clone());
            }
        }

        Ok(new_function)
    }

    /// This returns a new function because sometimes we add debugging
    /// comments in the function
    pub fn strided_intervals(
        &self,
        function: &ir::Function<ir::Constant>,
    ) -> Result<(
        ir::Function<ir::Constant>,
        HashMap<ir::ProgramLocation, analysis::strided_intervals::State>,
    )> {
        let strided_intervals = analysis::strided_intervals::strided_intervals(function)?;

        let mut new_function = function.clone();

        for block in new_function.blocks_mut() {
            let block_index = block.index();
            for instruction in block.instructions_mut() {
                let fl = ir::FunctionLocation::Instruction(block_index, instruction.index());
                let pl = ir::ProgramLocation::new(function.index().unwrap(), fl);

                let state = &strided_intervals[&pl];

                let mut si_comments = Vec::new();
                // if let Some(variables) = instruction.variables() {
                //     for variable in variables {
                //         if let Some(si) = state.variable(variable) {
                //             si_comments.push(format!("{}={}", variable, si));
                //         }
                //     }
                // }

                if let Some(si) = state.variable(&ir::scalar("temp_0.0", 32).into()) {
                    si_comments.push(format!("temp_0.0={}", si));
                }

                instruction.set_comment(Some(si_comments.join(", ")));
            }
        }

        Ok((new_function, strided_intervals))
    }

    pub fn translate_function(
        &self,
        function: &il::Function,
    ) -> Result<ir::Function<ir::Constant>> {
        let mut function =
            ir::Function::<ir::Constant>::from_il(function).expect("Failed to translate function");

        for module in self.modules() {
            module.pre_analysis_function(&mut function)?;
        }

        self.optimize_function(function)
    }

    pub fn optimize_function_inner(
        &self,
        mut function: ir::Function<ir::Constant>,
    ) -> Result<ir::Function<ir::Constant>> {
        loop {
            let function_0 = calls::call_sites(&function, self.ti())?;

            let function_1 = self.constants(&function_0)?;

            let stack_pointer_offsets = analysis::stack_pointer_offsets::stack_pointer_offsets(
                &function_1,
                self.ti().architecture(),
            )?;

            let mut function_2 = self.stack_pointers(&function_1, &stack_pointer_offsets)?;

            self.reduce_function(&mut function_2)?;
            self.stack_variables(&mut function_2);

            calls::apply_functions(&mut function_2, self.ti(), &stack_pointer_offsets)?;

            calls::no_returns(&mut function_2)?;

            // calls::set_function_parameters(&mut function_2, self.ti())?;
            let function_2 = self.remove_outgoing_transient_assignments(&function_2)?;

            if function == function_2 {
                break;
            } else {
                function = function_2;
            }
        }

        Ok(function)
    }

    /// The purpose of optimize_function_outer is to identify and resolve jump
    /// tables, but this is terribly broken atm.
    pub fn optimize_function_outer(
        &self,
        function: ir::Function<ir::Constant>,
    ) -> Result<ir::Function<ir::Constant>> {
        self.optimize_function_inner(function)
    }
    /*
    pub fn optimize_function_outer(
        &self,
        mut function: ir::Function<ir::Constant>
    ) -> Result<ir::Function<ir::Constant>> {

        loop {
            // Perform inner optimizations
            let function2 = self.optimize_function_inner(function)?;

            // Calculate jump tables
            let (function2, strided_intervals) =
                self.strided_intervals(&function2)?;

            let jump_tables = analysis::ksets::jump_table_analysis(
                &function2,
                &strided_intervals,
                self.ti().backing()
            )?;

            // If we recovered any jump tables, we need to deal with those
            if jump_tables.len() == 0  {
                return Ok(function2);
            }
            else {
                println!("jump_tables.len() = {}", jump_tables.len());
            }

            // Create manual edges for the extended lifter
            let mut manual_edges = Vec::new();
            for jump_table in &jump_tables {
                let rpl = jump_table.location().apply(&function2)?;
                let branch_address =
                    rpl.address()
                        .ok_or("Failed to get address for location while \
                               applying jump tables")?;

                for entry in jump_table.entries() {

                    println!("branch_address: 0x{:x}", branch_address);
                    println!("target_address: 0x{:x}", entry.address());
                    println!("condition: {}", entry.condition());

                    manual_edges.push((branch_address,
                                       entry.address(),
                                       Some(entry.condition().clone())));
                }
            }

            // Lift a new function
            let mut il_function: il::Function =
                self.ti()
                    .architecture()
                    .translator()
                    .translate_function_extended(
                        self.ti().backing(),
                        function2.address(),
                        manual_edges)?;

            // Give it the same index as the original function
            il_function.set_index(function2.index());

            function =
                ir::Function::<ir::Constant>::from_il(&il_function)?;

            // Strided intervals must be recomputed, ugh
            let (mut function, strided_intervals) =
                self.strided_intervals(&function)?;

            // Block/instruction indices may change.... so we'll rerun jump
            // table analysis, and that way we know we have valid values for
            // this currently lifted function.
            let jump_tables = analysis::ksets::jump_table_analysis(
                &function,
                &strided_intervals,
                self.ti().backing()
            )?;

            println!("jump_tables len={}", jump_tables.len());

            // Find the branches that correspond to jump table entries
            for jump_table in &jump_tables {
                let block_index =
                    jump_table
                        .location()
                        .function_location()
                        .block_index()
                        .ok_or("Failed to get block index for jump table")?;
                let instruction_index =
                    jump_table
                        .location()
                        .function_location()
                        .instruction_index()
                        .ok_or("Failed to get instruction index for jump table")?;

                let mut block = function.block_mut(block_index)?;
                block.replace_with_nop(instruction_index)?;
            }

            function.set_index(function2.index());
        }
    }
    */

    pub fn optimize_function(
        &self,
        function: ir::Function<ir::Constant>,
    ) -> Result<ir::Function<ir::Constant>> {
        self.optimize_function_outer(function)
    }
}
