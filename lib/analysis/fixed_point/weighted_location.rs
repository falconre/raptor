/// Weighted locations allow us to prioritze locations when performing fixed
/// point calculations.
use crate::ir;
use std::cmp::{Ord, Ordering, PartialOrd};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct WeightedLocation {
    weight: usize,
    location: ir::ProgramLocation,
}

impl WeightedLocation {
    pub(crate) fn new(weight: usize, location: ir::ProgramLocation) -> WeightedLocation {
        WeightedLocation { weight, location }
    }

    // pub(crate) fn weight(&self) -> usize { self.weight }
    pub(crate) fn weight(&self) -> usize {
        self.weight
    }
    pub(crate) fn location(&self) -> &ir::ProgramLocation {
        &self.location
    }
}

impl PartialOrd for WeightedLocation {
    fn partial_cmp(&self, other: &WeightedLocation) -> Option<Ordering> {
        Some(self.weight().cmp(&other.weight()))
    }
}

impl Ord for WeightedLocation {
    fn cmp(&self, other: &WeightedLocation) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}
