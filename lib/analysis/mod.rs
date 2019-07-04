//! Various analyses implemented over Raptor IR

pub mod constants;
pub mod detect_call_information;
pub mod fixed_point;
pub mod ksets;
pub mod stack_pointer_offsets;
pub mod strided_intervals;
pub mod transient_assignments;

mod argument_identification;
mod dead_code_elimination;
mod def_use;
mod location_set;
mod parameter_identification;
mod reaching_definitions;
mod use_def;
mod variable_use_def;

pub use self::argument_identification::argument_identification;
pub use self::constants::Constant;
pub use self::dead_code_elimination::dead_code_elimination;
pub use self::def_use::def_use;
pub use self::location_set::{LocationSet, RefLocationSet};
pub use self::parameter_identification::parameter_identification;
pub use self::reaching_definitions::reaching_definitions;
pub use self::use_def::use_def;
pub use self::variable_use_def::variable_use_def;
