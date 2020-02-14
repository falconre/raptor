//! Definition Use Analysis

use crate::analysis::{reaching_definitions, LocationSet};
use crate::error::*;
use crate::ir;
use std::collections::HashMap;

#[allow(dead_code)]
/// Compute definition use chains for the given function.
pub fn def_use<'r, V: ir::Value>(
    function: &'r ir::Function<V>,
) -> Result<HashMap<ir::ProgramLocation, LocationSet>> {
    let rd = reaching_definitions::reaching_definitions(function)?;

    let mut du: HashMap<ir::ProgramLocation, LocationSet> = HashMap::new();

    for (location, _) in &rd {
        du.entry(location.clone()).or_insert(LocationSet::new());
        let rpl = location.apply(function)?;
        match rpl.function_location() {
            ir::RefFunctionLocation::Instruction(_, instruction) => {
                instruction
                    .operation()
                    .variables_read()
                    .unwrap_or(Vec::new())
                    .into_iter()
                    .for_each(|variable_read| {
                        rd[&location].locations().into_iter().for_each(|rd| {
                            rd.apply(function)
                                .unwrap()
                                .instruction()
                                .unwrap()
                                .operation()
                                .variables_written()
                                .unwrap_or(Vec::new())
                                .into_iter()
                                .for_each(|variable_written| {
                                    if variable_written == variable_read {
                                        du.entry(rd.clone())
                                            .or_insert(LocationSet::new())
                                            .insert(location.clone());
                                    }
                                })
                        })
                    });
            }
            ir::RefFunctionLocation::Edge(ref edge) => {
                if let Some(condition_variables) =
                    edge.condition().map(|condition| condition.variables())
                {
                    condition_variables.into_iter().for_each(|variable_read| {
                        rd[&location].locations().into_iter().for_each(|rd| {
                            rd.apply(function)
                                .unwrap()
                                .instruction()
                                .unwrap()
                                .operation()
                                .variables_written()
                                .unwrap_or(Vec::new())
                                .into_iter()
                                .for_each(|variable_written| {
                                    if variable_written == variable_read {
                                        du.entry(rd.clone())
                                            .or_insert(LocationSet::new())
                                            .insert(location.clone());
                                    }
                                })
                        })
                    });
                }
            }
            ir::RefFunctionLocation::EmptyBlock(_) => {}
        }
    }

    Ok(du)
}
