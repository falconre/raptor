use crate::error::*;
use crate::ir::*;
use falcon::il;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Function<V: Value> {
    address: u64,
    index: Option<usize>,
    control_flow_graph: ControlFlowGraph<V>,
    parameters: Option<Vec<Variable>>,
    transient_variables: Vec<Variable>,
    name: String,
}

impl<V: Value> Function<V> {
    pub fn from_il(function: &il::Function) -> Result<Function<Constant>> {
        Ok(Function {
            address: function.address(),
            index: function.index(),
            control_flow_graph: ControlFlowGraph::<Constant>::from_il(
                function.control_flow_graph(),
            )?,
            parameters: None,
            transient_variables: Vec::new(),
            name: function.name(),
        })
    }

    pub fn address(&self) -> u64 {
        self.address
    }
    pub fn index(&self) -> Option<usize> {
        self.index
    }
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn set_index(&mut self, index: Option<usize>) {
        self.index = index;
    }

    pub fn parameters(&self) -> Option<&[Variable]> {
        self.parameters.as_deref()
    }
    pub fn set_parameters(&mut self, parameters: Option<Vec<Variable>>) {
        self.parameters = parameters;
    }

    pub fn transient_variables(&self) -> &[Variable] {
        &self.transient_variables
    }
    pub fn add_transient_variable(&mut self, variable: Variable) {
        self.transient_variables.push(variable);
    }

    pub fn control_flow_graph(&self) -> &ControlFlowGraph<V> {
        &self.control_flow_graph
    }
    pub fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph<V> {
        &mut self.control_flow_graph
    }

    pub fn block(&self, index: usize) -> Result<&Block<V>> {
        self.control_flow_graph().block(index)
    }
    pub fn block_mut(&mut self, index: usize) -> Result<&mut Block<V>> {
        self.control_flow_graph_mut().block_mut(index)
    }
    pub fn blocks(&self) -> Vec<&Block<V>> {
        self.control_flow_graph().blocks()
    }
    pub fn blocks_mut(&mut self) -> Vec<&mut Block<V>> {
        self.control_flow_graph_mut().blocks_mut()
    }

    pub fn instructions(&self) -> impl Iterator<Item = &Instruction<V>> {
        self.blocks()
            .into_iter()
            .flat_map(|block| block.instructions().iter())
    }

    pub fn edge(&self, head: usize, tail: usize) -> Result<&Edge<V>> {
        self.control_flow_graph().edge(head, tail)
    }
    pub fn edges(&self) -> Vec<&Edge<V>> {
        self.control_flow_graph().edges()
    }
    pub fn edges_mut(&mut self) -> Vec<&mut Edge<V>> {
        self.control_flow_graph_mut().edges_mut()
    }

    pub fn locations(&self) -> Vec<RefFunctionLocation<V>> {
        let mut locations = Vec::new();

        for block in self.blocks() {
            let instructions = block.instructions();
            if instructions.is_empty() {
                locations.push(RefFunctionLocation::EmptyBlock(block));
            } else {
                for instruction in instructions {
                    locations.push(RefFunctionLocation::Instruction(block, instruction));
                }
            }
        }

        for edge in self.edges() {
            locations.push(RefFunctionLocation::Edge(edge))
        }

        locations
    }

    pub fn program_locations(&self) -> Vec<RefProgramLocation<V>> {
        let mut locations = Vec::new();

        for block in self.blocks() {
            let instructions = block.instructions();
            if instructions.is_empty() {
                locations.push(RefProgramLocation::new(
                    self,
                    RefFunctionLocation::EmptyBlock(block),
                ));
            } else {
                for instruction in instructions {
                    locations.push(RefProgramLocation::new(
                        self,
                        RefFunctionLocation::Instruction(block, instruction),
                    ));
                }
            }
        }

        for edge in self.edges() {
            locations.push(RefProgramLocation::new(
                self,
                RefFunctionLocation::Edge(edge),
            ));
        }

        locations
    }
}
