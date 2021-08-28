//! Use-Definition Analysis

use crate::analysis::{reaching_definitions, LocationSet};
use crate::error::*;
use crate::ir;
use std::collections::HashMap;

#[allow(dead_code)]
/// Compute use definition chains for the given function.
pub fn use_def<V: ir::Value>(
    function: &ir::Function<V>,
) -> Result<HashMap<ir::ProgramLocation, LocationSet>> {
    let rd = reaching_definitions::reaching_definitions(function)?;

    use_def_rd(function, &rd)
}

/// Given computed reaching definitions, compute use-def chains
pub fn use_def_rd<V: ir::Value>(
    function: &ir::Function<V>,
    rd: &HashMap<ir::ProgramLocation, LocationSet>,
) -> Result<HashMap<ir::ProgramLocation, LocationSet>> {
    let mut ud: HashMap<ir::ProgramLocation, LocationSet> = HashMap::new();

    for location in rd.keys() {
        let rpl = location.apply(function)?;
        let defs = match rpl.function_location() {
            ir::RefFunctionLocation::Instruction(_, instruction) => instruction
                .operation()
                .variables_read()
                .unwrap_or_else(Vec::new)
                .into_iter()
                .fold(LocationSet::new(), |mut defs, variable_read| {
                    rd[location].locations().iter().for_each(|rd| {
                        rd.apply(function)
                            .unwrap()
                            .instruction()
                            .unwrap()
                            .operation()
                            .variables_written()
                            .unwrap_or_else(Vec::new)
                            .into_iter()
                            .for_each(|variable_written| {
                                if variable_written == variable_read {
                                    defs.insert(rd.clone());
                                }
                            })
                    });
                    defs
                }),
            ir::RefFunctionLocation::Edge(edge) => {
                if let Some(condition_variables) =
                    edge.condition().map(|condition| condition.variables())
                {
                    condition_variables.into_iter().fold(
                        LocationSet::new(),
                        |mut defs, variable_read| {
                            rd[location].locations().iter().for_each(|rd| {
                                rd.apply(function)
                                    .unwrap()
                                    .instruction()
                                    .unwrap()
                                    .operation()
                                    .variables_written()
                                    .unwrap_or_else(Vec::new)
                                    .into_iter()
                                    .for_each(|variable_written| {
                                        if variable_written == variable_read {
                                            defs.insert(rd.clone());
                                        }
                                    })
                            });
                            defs
                        },
                    )
                } else {
                    LocationSet::new()
                }
            }
            ir::RefFunctionLocation::EmptyBlock(_) => LocationSet::new(),
        };
        ud.insert(location.clone(), defs);
    }

    Ok(ud)
}
