use crate::ir;
use falcon::analysis::calling_convention::CallingConvention;
use falcon::architecture::Architecture;
use falcon::loader::{Loader, Symbol};
use falcon::memory::{backing, MemoryPermissions};
use std::collections::HashMap;

/// A standard set of information needed to translate things
pub struct TranslationInformation<'i> {
    architecture: &'i dyn Architecture,
    calling_convention: &'i CallingConvention,
    backing: &'i backing::Memory,
    symbols: &'i HashMap<u64, Symbol>,
    loader: &'i dyn Loader,
}

impl<'i> TranslationInformation<'i> {
    pub fn new(
        architecture: &'i dyn Architecture,
        calling_convention: &'i CallingConvention,
        backing: &'i backing::Memory,
        symbols: &'i HashMap<u64, Symbol>,
        loader: &'i dyn Loader,
    ) -> TranslationInformation<'i> {
        TranslationInformation {
            architecture: architecture,
            calling_convention: calling_convention,
            backing: backing,
            symbols: symbols,
            loader: loader,
        }
    }

    pub fn architecture(&self) -> &dyn Architecture {
        self.architecture
    }

    pub fn calling_convention(&self) -> &CallingConvention {
        &self.calling_convention
    }

    pub fn backing(&self) -> &backing::Memory {
        &self.backing
    }

    pub fn loader(&self) -> &dyn Loader {
        self.loader
    }

    pub fn symbols(&self) -> &HashMap<u64, Symbol> {
        &self.symbols
    }

    pub fn stack_pointer(&self) -> ir::Scalar {
        self.architecture.stack_pointer().into()
    }

    pub fn symbol(&self, address: u64) -> Option<&Symbol> {
        self.symbols.get(&address)
    }

    // If true, we never resolve jump tables for this address
    pub fn prohibited_jump_table_area(&self, address: u64) -> bool {
        if self
            .backing()
            .permissions(address)
            .map(|permissions| !permissions.contains(MemoryPermissions::EXECUTE))
            .unwrap_or(false)
        {
            return true;
        }

        let elf = match self.loader().as_any().downcast_ref::<falcon::loader::Elf>() {
            Some(elf) => elf,
            None => return false,
        };

        let elf = elf.elf();

        for section_header in elf.section_headers {
            let name: &str = match elf.shdr_strtab.get_at(section_header.sh_name) {
                Some(name) => name,
                None => continue,
            };

            if name != ".plt" {
                continue;
            }

            if section_header.sh_addr <= address
                && (section_header.sh_addr + section_header.sh_size) > address
            {
                return true;
            }
        }

        false
    }
}
