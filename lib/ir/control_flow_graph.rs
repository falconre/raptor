use error::*;
use falcon::{graph::Graph, il};
use ir::*;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct ControlFlowGraph<V: Value> {
    graph: Graph<Block<V>, Edge<V>>,
    entry: Option<usize>,
    exit: Option<usize>,
}

impl<V: Value> ControlFlowGraph<V> {
    pub fn from_il(
        control_flow_graph: &il::ControlFlowGraph,
    ) -> Result<ControlFlowGraph<Constant>> {
        let mut graph: Graph<Block<Constant>, Edge<Constant>> = Graph::new();

        control_flow_graph
            .blocks()
            .into_iter()
            .try_for_each(|block| graph.insert_vertex(Block::<Constant>::from_il(block)))?;

        control_flow_graph
            .edges()
            .into_iter()
            .try_for_each(|edge| graph.insert_edge(Edge::<Constant>::from_il(edge)))?;

        Ok(ControlFlowGraph {
            graph: graph,
            entry: control_flow_graph.entry().clone(),
            exit: control_flow_graph.exit().clone(),
        })
    }

    pub fn graph(&self) -> &Graph<Block<V>, Edge<V>> {
        &self.graph
    }
    pub fn graph_mut(&mut self) -> &mut Graph<Block<V>, Edge<V>> {
        &mut self.graph
    }
    pub fn entry(&self) -> Option<usize> {
        self.entry.clone()
    }
    pub fn exit(&self) -> Option<usize> {
        self.exit.clone()
    }

    pub fn block(&self, index: usize) -> Result<&Block<V>> {
        Ok(self.graph.vertex(index)?)
    }
    pub fn block_mut(&mut self, index: usize) -> Result<&mut Block<V>> {
        Ok(self.graph.vertex_mut(index)?)
    }
    pub fn blocks(&self) -> Vec<&Block<V>> {
        self.graph.vertices()
    }
    pub fn blocks_mut(&mut self) -> Vec<&mut Block<V>> {
        self.graph.vertices_mut()
    }

    pub fn edge(&self, head: usize, tail: usize) -> Result<&Edge<V>> {
        Ok(self.graph().edge(head, tail)?)
    }
    pub fn edge_mut(&mut self, head: usize, tail: usize) -> Result<&mut Edge<V>> {
        Ok(self.graph.edge_mut(head, tail)?)
    }
    pub fn edges(&self) -> Vec<&Edge<V>> {
        self.graph.edges()
    }
    pub fn edges_mut(&mut self) -> Vec<&mut Edge<V>> {
        self.graph.edges_mut()
    }

    pub fn edges_out(&self, block_index: usize) -> Result<&Vec<Edge<V>>> {
        Ok(self.graph.edges_out(block_index)?)
    }

    pub fn edges_in(&self, block_index: usize) -> Result<&Vec<Edge<V>>> {
        Ok(self.graph.edges_in(block_index)?)
    }
}
