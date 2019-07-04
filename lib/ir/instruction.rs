use falcon::il;
use ir::*;
use std::fmt;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Instruction<V: Value> {
    index: usize,
    operation: Operation<V>,
    address: Option<u64>,
    comment: Option<String>,
}

impl<V: Value> Instruction<V> {
    pub fn new(index: usize, operation: Operation<V>, address: Option<u64>) -> Instruction<V> {
        Instruction {
            index: index,
            operation: operation,
            address: address,
            comment: None,
        }
    }

    pub fn from_il(instruction: &il::Instruction) -> Instruction<Constant> {
        Instruction {
            index: instruction.index(),
            operation: Operation::<Constant>::from_il(instruction.operation()),
            address: instruction.address(),
            comment: None,
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn operation(&self) -> &Operation<V> {
        &self.operation
    }

    pub fn operation_mut(&mut self) -> &mut Operation<V> {
        &mut self.operation
    }

    pub fn address(&self) -> Option<u64> {
        self.address.clone()
    }

    pub fn comment(&self) -> Option<&str> {
        self.comment.as_ref().map(|comment| comment.as_str())
    }

    pub fn set_comment(&mut self, comment: Option<String>) {
        self.comment = comment;
    }

    pub fn expressions(&self) -> Vec<&Expression<V>> {
        self.operation.expressions()
    }

    pub fn expressions_mut(&mut self) -> Vec<&mut Expression<V>> {
        self.operation.expressions_mut()
    }

    pub fn variables_read(&self) -> Option<Vec<&Variable>> {
        self.operation().variables_read()
    }

    pub fn variables_written(&self) -> Option<Vec<&Variable>> {
        self.operation().variables_written()
    }

    pub fn variables(&self) -> Option<Vec<&Variable>> {
        self.operation.variables()
    }
}

impl<V: Value> fmt::Display for Instruction<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let comment = self
            .comment()
            .map(|comment| format!(" // {}", comment))
            .unwrap_or(String::new());

        match self.address() {
            Some(address) => write!(
                f,
                "{:X} {:02X} {}{}",
                address,
                self.index(),
                self.operation(),
                comment
            ),
            None => write!(f, "{:02X} {}{}", self.index(), self.operation(), comment),
        }
    }
}
