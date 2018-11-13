use error::*;
use std::collections::HashSet;
use ir;
use std::cmp::{Ordering, PartialOrd};


#[derive(Clone, Debug, PartialEq)]
pub struct LocationSet {
    locations: HashSet<ir::ProgramLocation>
}


impl LocationSet {
    pub fn new() -> LocationSet {
        LocationSet {
            locations: HashSet::default()
        }
    }

    pub fn insert(&mut self, location: ir::ProgramLocation) {
        self.locations.insert(location);
    }

    pub fn contains(&self, location: &ir::ProgramLocation) -> bool {
        self.locations.contains(location)
    }

    pub fn locations(&self) -> &HashSet<ir::ProgramLocation> {
        &self.locations
    }

    pub fn remove(&mut self, location: &ir::ProgramLocation) {
        self.locations.remove(location);
    }

    pub fn is_empty(&self) -> bool {
        self.locations.is_empty()
    }

    pub fn apply<'f, V: 'f + ir::Value>(&self, function: &'f ir::Function<V>)
        -> Result<RefLocationSet<'f, V>> {

        let mut rls = RefLocationSet::new();
        for location in self.locations() {
            rls.insert(location.apply(function)?);
        }
        Ok(rls)
    }

    pub fn join(mut self, other: &LocationSet) -> LocationSet {
        other.locations
            .iter()
            .for_each(|location| self.insert(location.clone()));
        self
    }
}


impl PartialOrd for LocationSet {
    fn partial_cmp(&self, rhs: &LocationSet) -> Option<Ordering> {
        let mut order =
            self.locations.iter()
                .fold(Ordering::Equal, |order, location|
                    if order == Ordering::Greater {
                        Ordering::Greater
                    }
                    else if !rhs.contains(location) {
                        Ordering::Greater
                    }
                    else {
                        Ordering::Equal
                    });

        for location in rhs.locations() {
            if !self.contains(location) {
                if order == Ordering::Greater {
                    return None;
                }
                else {
                    order = Ordering::Less;
                }
            }
        }
        Some(order)
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct RefLocationSet<'f, V: 'f + ir::Value> {
    locations: HashSet<ir::RefProgramLocation<'f, V>>
}


impl<'f, V: ir::Value> RefLocationSet<'f, V> {
    pub fn new() -> RefLocationSet<'f, V> {
        RefLocationSet {
            locations: HashSet::default()
        }
    }

    pub fn insert(&mut self, location: ir::RefProgramLocation<'f, V>) {
        self.locations.insert(location);
    }

    pub fn contains(&self, location: &ir::RefProgramLocation<'f, V>) -> bool {
        self.locations.contains(location)
    }

    pub fn locations(&self) -> &HashSet<ir::RefProgramLocation<'f, V>> {
        &self.locations
    }

    pub fn remove(&mut self, location: &ir::RefProgramLocation<'f, V>) {
        self.locations.remove(location);
    }

    pub fn is_empty(&self) -> bool {
        self.locations.is_empty()
    }
}


impl<'f, V: ir::Value> PartialOrd for RefLocationSet<'f, V> {
    fn partial_cmp(&self, rhs: &RefLocationSet<'f, V>) -> Option<Ordering> {
        let mut order =
            self.locations.iter()
                .fold(Ordering::Equal, |order, location|
                    if order == Ordering::Greater {
                        Ordering::Greater
                    }
                    else if !rhs.contains(location) {
                        Ordering::Greater
                    }
                    else {
                        Ordering::Equal
                    });

        for location in rhs.locations() {
            if !self.contains(location) {
                if order == Ordering::Greater {
                    return None;
                }
                else {
                    order = Ordering::Less;
                }
            }
        }
        Some(order)
    }
}