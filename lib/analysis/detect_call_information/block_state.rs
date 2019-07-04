use analysis::detect_call_information::TaggedConstant;
use error::*;
use ir;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct BlockState<'b> {
    variables: HashMap<ir::Variable, TaggedConstant<'b>>,
    memory: HashMap<ir::Constant, TaggedConstant<'b>>,
}

impl<'b> BlockState<'b> {
    pub fn new() -> BlockState<'b> {
        BlockState {
            variables: HashMap::new(),
            memory: HashMap::new(),
        }
    }

    pub fn eval(&self, expression: &ir::Expression<ir::Constant>) -> Result<Option<ir::Constant>> {
        let expression: ::std::result::Result<ir::Expression<ir::Constant>, Error> =
            self.variables.iter().try_fold(
                expression.clone(),
                |expression, (variable, tagged_constant)| {
                    Ok(expression
                        .replace_variable(&variable, &tagged_constant.constant().clone().into())?)
                },
            );

        let expression = expression?;

        if expression.all_constants() {
            Ok(Some(ir::eval(&expression)?))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn load<'i>(
        &self,
        index: &'i ir::Constant,
        bits: usize,
    ) -> Option<&TaggedConstant<'b>> {
        match self.memory.get(index) {
            Some(value) => {
                if index.bits() == bits {
                    Some(value)
                } else {
                    None
                }
            }
            None => None,
        }
    }

    pub(crate) fn store(&mut self, index: ir::Constant, value: Option<TaggedConstant<'b>>) {
        match value {
            Some(value) => {
                self.memory.insert(index, value);
            }
            None => {
                self.memory.remove(&index);
            }
        }
    }

    pub(crate) fn clear_memory(&mut self) {
        self.memory.clear();
    }

    pub(crate) fn set(
        &mut self,
        variable: ir::Variable,
        value: Option<TaggedConstant<'b>>,
    ) -> Result<()> {
        if let Some(tagged_constant) = &value {
            if variable.bits() != tagged_constant.constant().bits() {
                bail!(
                    "mismatch bits {} {:?}",
                    variable,
                    tagged_constant.constant()
                );
            }
        }
        match value {
            Some(value) => {
                self.variables.insert(variable, value);
            }
            None => {
                self.variables.remove(&variable);
            }
        }
        Ok(())
    }

    pub(crate) fn get<'v>(&self, variable: &'v ir::Variable) -> Option<&TaggedConstant<'b>> {
        self.variables.get(variable)
    }
}
