//! Parameter Identification
//!
//! Rules for being a parameter
//! MUST
//!   * Be read before being written
//! MUST NOT
//!   * Be a StackVariable with an offset below 0
//!   * Originate from a reference to a stack variable with an offset below 0
//!

use analysis::stack_pointer_offsets::stack_pointer_offsets;
use analysis::reaching_definitions;
use error::*;
use falcon::architecture::Architecture;
use falcon::analysis::calling_convention::CallingConvention;
use falcon::analysis::calling_convention::ArgumentType;
use ir;

/// Attempt to identify the arguments to function calls.
pub fn argument_identification(
    function: &ir::Function<ir::Constant>,
    calling_convention: &CallingConvention,
    architecture: &Architecture,
    stack_pointer: &ir::Variable,
) -> Result<()> {
    let rd = reaching_definitions(function)?;
    let spo = stack_pointer_offsets(function, architecture)?;

    for ref_program_location in function.program_locations().into_iter() {
        let program_location: ir::ProgramLocation =
            ref_program_location.clone().into();

        let instruction = match ref_program_location.instruction() {
            Some(instruction) => instruction,
            None => continue
        };

        let call = match instruction.operation().call() {
            Some(call) => call,
            None => continue
        };

        println!("{}", instruction);

        for i in 0..8 {
            let arg: ir::Variable = match calling_convention.argument_type(i) {
                ArgumentType::Register(scalar) => scalar.clone().into(),
                ArgumentType::Stack(offset) => {
                    let stack_pointer_offset =
                        spo[&program_location]
                            .get(stack_pointer)
                            .expect("Failed to get stack variable offset");
                    let offset = stack_pointer_offset + offset as isize;
                    ir::StackVariable::new(offset as isize, 32).into()
                }
            };

            let mut found = false;

            for location in rd[&program_location].locations() {
                let location = location.apply(function)?;
                let dst =
                    location
                        .instruction()
                        .expect("Failed to get rd instruction")
                        .operation()
                        .dst()
                        .expect("Failed to get operation dst");

                if *dst == arg {
                    println!("  {}, {}", dst, location.instruction().unwrap());
                    found = true;
                    break;
                }
            }

            if !found {
                break;
            }
        }
    }

    Ok(())
}