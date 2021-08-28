use crate::error::*;
use crate::ir::*;
use falcon::{il, RC};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Program<V: Value> {
    functions: BTreeMap<usize, RC<Function<V>>>,
}

impl<V: Value> Program<V> {
    pub fn new() -> Program<V> {
        Program {
            functions: BTreeMap::new(),
        }
    }

    pub fn from_il(program: &il::Program) -> Result<Program<Constant>> {
        let functions: ::std::result::Result<BTreeMap<usize, RC<Function<Constant>>>, Error> =
            program.functions_map().into_iter().try_fold(
                BTreeMap::new(),
                |mut functions, (index, function)| {
                    functions.insert(index, RC::new(Function::<Constant>::from_il(function)?));
                    Ok(functions)
                },
            );
        Ok(Program {
            functions: functions?,
        })
    }

    pub fn function(&self, index: usize) -> Option<&Function<V>> {
        self.functions.get(&index).map(|function| function.as_ref())
    }

    pub fn functions(&self) -> Vec<&Function<V>> {
        self.functions
            .iter()
            .map(|(_, function)| function.as_ref())
            .collect::<Vec<&Function<V>>>()
    }

    pub fn function_by_name(&self, name: &str) -> Option<&Function<V>> {
        self.functions
            .iter()
            .find(|(_, function)| function.name() == name)
            .map(|(_, function)| function.as_ref())
    }

    pub fn replace_function(&mut self, index: usize, function: Function<V>) {
        self.functions.insert(index, RC::new(function));
    }
}

impl<V: Value> Default for Program<V> {
    fn default() -> Self {
        Self::new()
    }
}
