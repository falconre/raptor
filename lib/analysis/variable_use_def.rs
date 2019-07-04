use analysis::{use_def, LocationSet};
use error::*;
use ir;
use std::collections::HashMap;

pub fn variable_use_def<'f, V: ir::Value>(
    function: &'f ir::Function<V>,
) -> Result<HashMap<ir::ProgramLocation, HashMap<ir::Variable, LocationSet>>> {
    variable_use_def_ud(function, &use_def(function)?)
}

/// Given already computed use-def chains, compute usedef for each variable
pub fn variable_use_def_ud<'f, V: ir::Value>(
    function: &'f ir::Function<V>,
    ud: &HashMap<ir::ProgramLocation, LocationSet>,
) -> Result<HashMap<ir::ProgramLocation, HashMap<ir::Variable, LocationSet>>> {
    let mut result: HashMap<ir::ProgramLocation, HashMap<ir::Variable, LocationSet>> =
        HashMap::new();

    for location in function.program_locations() {
        let variables = location
            .instruction()
            .map(|instruction| instruction.variables_read())
            .unwrap_or(
                location
                    .edge()
                    .and_then(|edge| edge.condition().map(|c| c.variables())),
            );

        let variables = match variables {
            Some(variables) => variables,
            None => continue,
        };

        let mut location_result = HashMap::new();

        for variable in variables {
            let mut variable_result = LocationSet::new();
            for write_location in ud[&location.clone().into()].locations() {
                let writes_variable = write_location
                    .apply(function)?
                    .instruction()
                    .unwrap()
                    .variables_written()
                    .unwrap()
                    .contains(&variable);
                if writes_variable {
                    variable_result.insert(write_location.to_owned());
                }
            }
            location_result.insert(variable.to_owned(), variable_result);
        }
        result.insert(location.clone().into(), location_result);
    }

    Ok(result)
}
