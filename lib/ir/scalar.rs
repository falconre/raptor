use falcon::il;
use serde::{Deserialize, Serialize};
use std::fmt;

/// A `Scalar` is a variable which holds a single value.
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Scalar {
    name: String,
    bits: usize,
    ssa: Option<usize>,
}

/// A scalar value for Falcon IL.
impl Scalar {
    /// Create a new `Scalar` with the given name and bitness.
    pub fn new<S>(name: S, bits: usize) -> Scalar
    where
        S: Into<String>,
    {
        Scalar {
            name: name.into(),
            bits: bits,
            ssa: None,
        }
    }

    /// Gets the bitness of the `Scalar`.
    pub fn bits(&self) -> usize {
        self.bits
    }

    /// Gets the name of the `Scalar`.
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn ssa(&self) -> Option<usize> {
        self.ssa.clone()
    }

    pub fn set_ssa(&mut self, ssa: Option<usize>) {
        self.ssa = ssa;
    }

    /// An identifier for the `Scalar`. This is the string which is displayed
    /// when printing the IL.
    pub fn identifier(&self) -> String {
        let ssa = match self.ssa() {
            Some(ssa) => format!(".{}", ssa),
            None => String::from(""),
        };

        format!("{}:{}{}", self.name, self.bits, ssa)
    }
}

impl From<il::Scalar> for Scalar {
    fn from(s: il::Scalar) -> Scalar {
        Scalar::new(s.name(), s.bits())
    }
}

impl fmt::Display for Scalar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.identifier())
    }
}
