use falcon::analysis::calling_convention::CallingConvention;
use falcon::architecture::Architecture;
use falcon::loader::Symbol;
use falcon::memory::backing;
use ir;
use std::collections::HashMap;


/// A standard set of information needed to translate things
pub struct TranslationInformation<'i> {
    architecture: &'i Architecture,
    calling_convention: &'i CallingConvention,
    backing: &'i backing::Memory,
    symbols: &'i HashMap<u64, Symbol>
}


impl<'i> TranslationInformation<'i> {
    pub fn new(
        architecture: &'i Architecture,
        calling_convention: &'i CallingConvention,
        backing: &'i backing::Memory,
        symbols: &'i HashMap<u64, Symbol>
    ) -> TranslationInformation<'i> {
        TranslationInformation {
            architecture: architecture,
            calling_convention: calling_convention,
            backing: backing,
            symbols: symbols
        }
    }

    pub fn architecture(&self) -> &Architecture { self.architecture }
    pub fn calling_convention(&self) -> &CallingConvention {
        &self.calling_convention
    }
    pub fn backing(&self) -> &backing::Memory { &self.backing }
    pub fn symbols(&self) -> &HashMap<u64, Symbol> { &self.symbols }

    pub fn stack_pointer(&self) -> ir::Scalar {
        self.architecture.stack_pointer().into()
    }

    pub fn symbol(&self, address: u64) -> Option<&Symbol> {
        self.symbols.get(&address)
    }
}