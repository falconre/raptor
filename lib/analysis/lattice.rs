/// Quickly lattice things for fixed point analysis
use crate::error::*;
use crate::ir;
use std::cmp::{Ordering, PartialOrd};
use std::collections::HashMap;
use std::fmt;

pub trait LatticedValue: Clone + fmt::Debug {
    fn join(&self, other: &Self) -> Result<Self>;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Lattice<V> {
    Top(usize),
    Value(V),
    Bottom(usize),
}

impl<V: LatticedValue> Lattice<V> {
    pub fn is_top(&self) -> bool {
        match self {
            Lattice::Top(_) => true,
            Lattice::Value(_) | Lattice::Bottom(_) => false,
        }
    }

    pub fn value(&self) -> Option<&V> {
        match self {
            Lattice::Value(v) => Some(v),
            _ => None,
        }
    }

    pub fn join(&self, other: &Lattice<V>) -> Result<Lattice<V>> {
        Ok(match self {
            Lattice::Top(bits) => Lattice::Top(*bits),
            Lattice::Value(lhs) => match other {
                Lattice::Top(bits) => Lattice::Top(*bits),
                Lattice::Value(rhs) => Lattice::Value(lhs.join(rhs)?),
                Lattice::Bottom(_) => Lattice::Value(lhs.clone()),
            },
            Lattice::Bottom(_) => other.clone(),
        })
    }
}

impl<V: PartialOrd> PartialOrd for Lattice<V> {
    fn partial_cmp(&self, rhs: &Lattice<V>) -> Option<Ordering> {
        match self {
            Lattice::Top(_) => match rhs {
                Lattice::Top(_) => Some(Ordering::Equal),
                Lattice::Value(_) | Lattice::Bottom(_) => Some(Ordering::Greater),
            },
            Lattice::Value(vl) => match rhs {
                Lattice::Top(_) => Some(Ordering::Less),
                Lattice::Value(vr) => vl.partial_cmp(vr),
                Lattice::Bottom(_) => Some(Ordering::Greater),
            },
            Lattice::Bottom(_) => match rhs {
                Lattice::Top(_) | Lattice::Value(_) => Some(Ordering::Less),
                Lattice::Bottom(_) => Some(Ordering::Equal),
            },
        }
    }
}

impl<V> From<V> for Lattice<V> {
    fn from(v: V) -> Lattice<V> {
        Lattice::Value(v)
    }
}

impl<V: fmt::Display> fmt::Display for Lattice<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lattice::Top(bits) => write!(f, "⊤:{}", bits),
            Lattice::Value(value) => value.fmt(f),
            Lattice::Bottom(bits) => write!(f, "⊥:{}", bits),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LatticedVariables<V> {
    variables: HashMap<ir::Variable, Lattice<V>>,
}

impl<V: LatticedValue> LatticedVariables<V> {
    pub fn new() -> LatticedVariables<V> {
        LatticedVariables {
            variables: HashMap::new(),
        }
    }

    pub fn variable(&self, variable: &ir::Variable) -> Option<&Lattice<V>> {
        self.variables.get(variable)
    }

    pub fn variable_value(&self, variable: &ir::Variable) -> Option<&V> {
        self.variable(variable).and_then(|lattice| lattice.value())
    }

    pub fn set_variable(&mut self, variable: ir::Variable, l: Lattice<V>) {
        self.variables.insert(variable, l);
    }

    pub fn set_variable_value(&mut self, variable: ir::Variable, v: V) {
        self.variables.insert(variable, Lattice::Value(v));
    }

    pub fn top(&mut self) {
        for (variable, value) in self.variables.iter_mut() {
            *value = Lattice::Top(variable.bits());
        }
    }

    pub fn join(mut self, other: &LatticedVariables<V>) -> Result<LatticedVariables<V>> {
        for (variable, other_value) in &other.variables {
            let new_value = match self.variables.get(variable) {
                Some(self_value) => self_value.join(other_value)?,
                None => other_value.clone(),
            };
            self.variables.insert(variable.clone(), new_value);
        }
        Ok(self)
    }
}

impl<V: PartialOrd + fmt::Debug> PartialOrd for LatticedVariables<V> {
    fn partial_cmp(&self, rhs: &LatticedVariables<V>) -> Option<Ordering> {
        let mut order = Ordering::Equal;

        for (variable, lattice) in &self.variables {
            match rhs.variables.get(variable) {
                Some(rhs_lattice) => match lattice.partial_cmp(rhs_lattice)? {
                    Ordering::Greater => {
                        if order == Ordering::Less {
                            println!("None 0 on {} {:?}", variable, lattice);
                            return None;
                        } else {
                            order = Ordering::Greater;
                        }
                    }
                    Ordering::Less => {
                        if order == Ordering::Greater {
                            println!("None 1 on {} {:?}", variable, lattice);
                            return None;
                        } else {
                            order = Ordering::Less;
                        }
                    }
                    Ordering::Equal => {}
                },
                None => {
                    if order == Ordering::Less {
                        println!("None 2 on {} {:?}", variable, lattice);
                        return None;
                    } else {
                        order = Ordering::Greater;
                    }
                }
            }
        }

        for variable in rhs.variables.keys() {
            if self.variables.get(variable).is_none() {
                if order == Ordering::Greater {
                    println!("None 3 on {}", variable);
                    return None;
                }
                order = Ordering::Less;
            }
        }

        Some(order)
    }
}
impl<V: LatticedValue> Default for LatticedVariables<V> {
    fn default() -> Self {
        Self::new()
    }
}
