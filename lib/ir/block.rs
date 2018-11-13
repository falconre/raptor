use falcon::{graph, il};
use ir::*;
use std::fmt;


#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Block<V: Value> {
    index: usize,
    instructions: Vec<Instruction<V>>
}


impl<V: Value> Block<V> {
    pub fn from_il(block: &il::Block) -> Block<Constant> {
        Block {
            index: block.index(),
            instructions:
                block.instructions()
                    .iter()
                    .map(|instruction| Instruction::<Constant>::from_il(instruction))
                    .collect::<Vec<Instruction<Constant>>>()
        }
    }

    pub fn index(&self) -> usize { self.index }
    pub fn instruction(&self, index: usize) -> Option<&Instruction<V>> {
        self.instructions()
            .iter()
            .find(|instruction| instruction.index() == index)
    }
    pub fn instruction_mut(&mut self, index: usize) -> Option<&mut Instruction<V>> {
        self.instructions_mut()
            .iter_mut()
            .find(|instruction| instruction.index() == index)
    }
    pub fn instructions(&self) -> &[Instruction<V>] { &self.instructions }
    pub fn instructions_mut(&mut self) -> &mut [Instruction<V>] {
        &mut self.instructions
    }
    pub fn replace_with_nop(&mut self, index: usize) -> Result<()> {
        let instruction = match self.instruction_mut(index) {
            Some(instruction) => instruction,
            None => bail!("Invalid index for Block::replace_with_nop")
        };
        *instruction.operation_mut() = Operation::Nop;
        Ok(())
    }
}


impl<V: Value> graph::Vertex for Block<V> {
    fn index(&self) -> usize { self.index }
    fn dot_label(&self) -> String { format!("{}", self) }
}


impl<V: Value> fmt::Display for Block<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "[ Block 0x{:02x} ]", self.index())?;
        for instruction in self.instructions() {
            if instruction.operation().is_nop() {
                continue;
            }
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}