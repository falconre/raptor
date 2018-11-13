use falcon::{graph, il};
use ir::*;
use std::fmt;


#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Edge<V: Value> {
    head: usize,
    tail: usize,
    condition: Option<Expression<V>>,
    comment: Option<String>
}


impl<V: Value> Edge<V> {
    pub fn from_il(edge: &il::Edge) -> Edge<Constant> {
        Edge {
            head: edge.head(),
            tail: edge.tail(),
            condition: edge.condition().map(|expression|
                Expression::from_il(expression)),
            comment: edge.comment().clone()
        }
    }

    pub fn head(&self) -> usize { self.head }
    pub fn tail(&self) -> usize { self.tail }
    pub fn condition(&self) -> Option<&Expression<V>> {
        self.condition.as_ref()
    }
    pub fn comment(&self) -> Option<&str> {
        self.comment.as_ref().map(|s| s.as_str())
    }
}


impl<V: Value> graph::Edge for Edge<V> {
    fn head(&self) -> usize { self.head }
    fn tail(&self) -> usize { self.tail }
    fn dot_label(&self) -> String {
        format!("{}", self)
    }
}


impl<V: Value> fmt::Display for Edge<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(condition) = self.condition() {
            write!(f,
                   "(0x{:02x} -> 0x{:02x}) ? {}",
                   self.head(),
                   self.tail(),
                   condition)
        }
        else {
            write!(f, "(0x{:x} -> 0x{:x})", self.head(), self.tail())
        }
    }
}