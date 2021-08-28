use crate::ir;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
pub struct XRefs {
    from_to: HashMap<u64, HashSet<u64>>,
    to_from: HashMap<u64, HashSet<u64>>,
}

impl XRefs {
    pub fn new() -> XRefs {
        XRefs {
            from_to: HashMap::new(),
            to_from: HashMap::new(),
        }
    }

    pub fn from_program(program: &ir::Program<ir::Constant>) -> XRefs {
        let mut xrefs = XRefs::new();

        let instructions = program
            .functions()
            .into_iter()
            .flat_map(move |function| function.locations())
            .filter(move |location| location.instruction().is_some())
            .map(move |location| location.instruction().unwrap().clone());

        for instruction in instructions {
            if instruction.address().is_none() {
                continue;
            }
            match instruction.operation() {
                ir::Operation::Assign { src, .. } => {
                    let option_value = src.constant().and_then(|constant| constant.value_u64());
                    if let Some(value) = option_value {
                        xrefs.insert(instruction.address().unwrap(), value);
                    }
                }
                ir::Operation::Load { index, .. } => {
                    let option_value = index.constant().and_then(|constant| constant.value_u64());
                    if let Some(value) = option_value {
                        xrefs.insert(instruction.address().unwrap(), value);
                    }
                }
                ir::Operation::Store { index, src } => {
                    let option_value = index.constant().and_then(|constant| constant.value_u64());
                    if let Some(value) = option_value {
                        xrefs.insert(instruction.address().unwrap(), value);
                    }
                    let option_value = src.constant().and_then(|constant| constant.value_u64());
                    if let Some(value) = option_value {
                        xrefs.insert(instruction.address().unwrap(), value);
                    }
                }
                ir::Operation::Return(result) => {
                    let option_value = result
                        .as_ref()
                        .and_then(|result| result.constant())
                        .and_then(|constant| constant.value_u64());
                    if let Some(value) = option_value {
                        xrefs.insert(instruction.address().unwrap(), value);
                    }
                }
                ir::Operation::Call(call) => {
                    if let Some(arguments) = call.arguments() {
                        for argument in arguments {
                            let argument_value = argument
                                .constant()
                                .and_then(|constant| constant.value_u64());
                            if let Some(value) = argument_value {
                                xrefs.insert(instruction.address().unwrap(), value);
                            }
                        }
                    }
                }
                ir::Operation::Branch { .. }
                | ir::Operation::Intrinsic(_)
                | ir::Operation::Nop(_) => {}
            }
        }

        xrefs
    }

    pub fn insert(&mut self, from: u64, to: u64) {
        self.from_to
            .entry(from)
            .or_insert(HashSet::new())
            .insert(to);
        self.to_from
            .entry(to)
            .or_insert(HashSet::new())
            .insert(from);
    }

    pub fn from_to(&self) -> &HashMap<u64, HashSet<u64>> {
        &self.from_to
    }
    pub fn to_from(&self) -> &HashMap<u64, HashSet<u64>> {
        &self.to_from
    }

    pub fn from(&self, from: u64) -> Option<&HashSet<u64>> {
        self.from_to.get(&from)
    }

    pub fn to(&self, to: u64) -> Option<&HashSet<u64>> {
        self.to_from.get(&to)
    }
}
