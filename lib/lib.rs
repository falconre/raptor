extern crate falcon;
#[macro_use]
extern crate error_chain;
extern crate falcon_z3;
extern crate goblin;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
extern crate serde;
#[macro_use]
extern crate serde_derive;

pub mod analysis;
pub mod features;
pub mod ir;
pub mod modules;
pub mod solver;
pub mod translator;

pub mod data;

pub mod error {
    error_chain! {
        types {
            Error, ErrorKind, ResultExt, Result;
        }

        foreign_links {
            Falcon(::falcon::error::Error);
            FalconZ3(::falcon_z3::error::Error);
            Goblin(::goblin::error::Error);
            NullError(::std::ffi::NulError);
        }

        errors {
            SolverReference {
                description("A reference was present in an expression passed to the solver")
                display("A reference was present in an expression passed to the solver")
            }
            SolverDereference {
                description("A dereference was present in an expression passed to the solver")
                display("A dereference was present in an expression passed to the solver")
            }
            Analysis(m: String) {
                description("An error in the analysis")
                display("Analysis error: {}", m)
            }
            EvalNonConstant(variable: String) {
                description("eval can only execute over constant values")
                display("A variable \"{}\" was found while evaluating expression", variable)
            }
            Sort {
                description("Sort error")
                display("Sort error, bits differ incorrectly")
            }
            OwnedLocationApplication {
                description("An owned location \
                             (ProgramLocation/FunctionLocation) could not be \
                             applied to a function")
                display("Failed to apply an owned location")
            }
        }
    }
}

pub fn falcon_result<T>(raptor_result: error::Result<T>) -> falcon::error::Result<T> {
    raptor_result.map_err(|e| format!("Raptor Error: {}", e.description()).into())
}
