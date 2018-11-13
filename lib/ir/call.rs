use ir::*;
use std::fmt;


#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum CallTarget<V: Value> {
    Expression(Expression<V>),
    Symbol(String),
    FunctionId(usize)
}


impl<V: Value> CallTarget<V> {
    pub fn expression(&self) -> Option<&Expression<V>> {
        match self {
            CallTarget::Expression(expression) => Some(expression),
            _ => None
        }
    }

    pub fn expression_mut(&mut self) -> Option<&mut Expression<V>> {
        match self {
            CallTarget::Expression(expression) => Some(expression),
            _ => None
        }
    }

    pub fn symbol(&self) -> Option<&str> {
        match self {
            CallTarget::Symbol(string) => Some(string.as_str()),
            _ => None
        }
    }

    pub fn function_id(&self) -> Option<usize> {
        match self {
            CallTarget::FunctionId(function_id) => Some(*function_id),
            _ => None
        }
    }
}


#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Call<V: Value> {
    target: CallTarget<V>,
    arguments: Option<Vec<Expression<V>>>,
    // Variables that are written by the call
    variables_written: Option<Vec<Variable>>
}


impl<V: Value> Call<V> {
    pub fn new(target: CallTarget<V>) -> Call<V> {
        Call {
            target: target,
            arguments: None,
            variables_written: None
        }
    }

    pub fn variables_read(&self) -> Option<Vec<&Variable>> {
        if let Some(arguments) = self.arguments() {
            let mut variables: Vec<&Variable> =
                arguments.iter()
                    .flat_map(|expression| expression.variables())
                    .collect();
            if let Some(expression) = self.target().expression() {
                variables.append(&mut expression.variables());
            }
            Some(variables)
        }
        else {
            None
        }
    }

    pub fn target(&self) -> &CallTarget<V> { &self.target }
    pub fn target_mut(&mut self) -> &mut CallTarget<V> { &mut self.target }
    pub fn set_target(&mut self, target: CallTarget<V>) {
        self.target = target;
    }

    pub fn variables_written(&self) -> Option<&[Variable]> {
        self.variables_written.as_ref().map(|v| v.as_slice())
    }
    pub fn set_variables_written(
        &mut self,
        variables_written: Option<Vec<Variable>>
    ) {
        self.variables_written = variables_written;
    }

    pub fn arguments(&self) -> Option<&[Expression<V>]> {
        self.arguments.as_ref().map(|v| v.as_slice())
    }

    pub fn arguments_mut(&mut self) -> Option<&mut [Expression<V>]> {
        self.arguments.as_mut().map(|v| v.as_mut())
    }

    pub fn set_empty_arguments(&mut self) {
        self.arguments = Some(Vec::new());
    }

    pub fn push_argument(&mut self, argument: Expression<V>) {
        if self.arguments.is_none() {
            self.arguments = Some(vec![argument]);
        }
        else {
            self.arguments.as_mut().unwrap().push(argument);
        }
    }

    pub fn clear_arguments(&mut self) {
        self.arguments = None;
    }
}


impl<V: Value> fmt::Display for Call<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.target() {
            CallTarget::Expression(expression) =>
                write!(f, "call({})", expression),
            CallTarget::Symbol(string) =>
                match self.arguments() {
                    Some(arguments) =>
                        write!(f, "{}({})",
                            string,
                            arguments
                                .iter()
                                .map(|argument| format!("{}", argument))
                                .collect::<Vec<String>>()
                                .join(", ")),
                    None => write!(f, "{}(?)", string)
                }
                
            CallTarget::FunctionId(function_id) =>
                write!(f, "id_0x{:x}()", function_id)
        }
    }
}