//! Translator from Falcon IL to Raptor IR

pub mod calls;

mod function_translator;
mod program_translator;
mod translation_information;

pub use self::function_translator::FunctionTranslator;
pub use self::program_translator::ProgramTranslator;
pub use self::translation_information::TranslationInformation;