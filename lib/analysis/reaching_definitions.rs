use crate::analysis::{fixed_point, LocationSet};
use crate::error::*;
use crate::ir;
use std::collections::HashMap;

/// Compute reaching definitions for the given function.
pub fn reaching_definitions<V: ir::Value>(
    function: &ir::Function<V>,
) -> Result<HashMap<ir::ProgramLocation, LocationSet>> {
    let reaching_definitions = ReachingDefinitions { function };

    fixed_point::incoming_results(
        &reaching_definitions,
        function,
        fixed_point::fixed_point_forward(&reaching_definitions, function)?,
        LocationSet::new,
    )
}

// We require a struct to implement methods for our analysis over.
struct ReachingDefinitions<'f, V: 'f + ir::Value> {
    function: &'f ir::Function<V>,
}

impl<'r, V: ir::Value> fixed_point::FixedPointAnalysis<'r, LocationSet, V>
    for ReachingDefinitions<'r, V>
{
    fn trans(
        &self,
        location: &ir::RefProgramLocation<'r, V>,
        state: Option<LocationSet>,
    ) -> Result<LocationSet> {
        let mut state = match state {
            Some(state) => state,
            None => LocationSet::new(),
        };

        match *location.function_location() {
            ir::RefFunctionLocation::Instruction(_, instruction) => {
                instruction
                    .operation()
                    .variables_written()
                    .unwrap_or_else(Vec::new)
                    .into_iter()
                    .for_each(|variable_written| {
                        let kill: Vec<ir::ProgramLocation> = state
                            .locations()
                            .iter()
                            .filter(|location| {
                                location
                                    .apply(self.function)
                                    .unwrap()
                                    .instruction()
                                    .unwrap()
                                    .operation()
                                    .variables_written()
                                    .unwrap_or_else(Vec::new)
                                    .into_iter()
                                    .any(|variable| variable == variable_written)
                            })
                            .cloned()
                            .collect();
                        kill.iter().for_each(|location| state.remove(location));
                        state.insert(location.clone().into());
                    });
            }
            ir::RefFunctionLocation::EmptyBlock(_) | ir::RefFunctionLocation::Edge(_) => {}
        }

        Ok(state)
    }

    fn join(&self, state0: LocationSet, state1: &LocationSet) -> Result<LocationSet> {
        Ok(state0.join(state1))
    }
}
