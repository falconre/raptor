use crate::error::*;
use crate::ir;
use crate::translator::{FunctionTranslator, TranslationInformation};
use falcon;
use falcon::analysis::calling_convention::CallingConvention;
use falcon::architecture::Architecture;
use falcon::loader::{Elf, Loader, Symbol};
use falcon::translator::{Options, TranslationMemory};
use std::collections::HashMap;

pub struct ProgramTranslator<'t> {
    architecture: Box<dyn Architecture>,
    calling_convention: CallingConvention,
    loader: &'t dyn Loader,
    backing: falcon::memory::backing::Memory,
    symbols: HashMap<u64, Symbol>,
    // relocations: HashMap<u64, Symbol>
}

impl<'t> ProgramTranslator<'t> {
    pub fn new(
        architecture: Box<dyn Architecture>,
        calling_convention: CallingConvention,
        loader: &'t dyn Loader,
    ) -> Result<ProgramTranslator<'t>> {
        let mut translator = ProgramTranslator {
            architecture,
            calling_convention,
            loader,
            backing: loader.memory()?,
            symbols: loader.symbols_map(),
        };

        translator.init_symbols()?;

        let mut starting_symbols = translator
            .symbols
            .iter()
            .map(|(_, symbol)| symbol)
            .cloned()
            .collect::<Vec<Symbol>>();

        starting_symbols.sort();

        Ok(translator)
    }

    fn plt_symbols(&self) -> Result<Vec<Symbol>> {
        let elf = match self.loader().as_any().downcast_ref::<Elf>() {
            Some(elf) => elf,
            None => return Ok(Vec::new()),
        };

        let translator = elf.architecture().translator();

        let elf = elf.elf();

        let mut plt_symbols = Vec::new();

        // Find the plt section
        for section_header in elf.section_headers {
            let name: &str = match elf.shdr_strtab.get_at(section_header.sh_name) {
                Some(name) => name,
                None => continue,
            };

            if name != ".plt" {
                continue;
            }

            let start = section_header.sh_addr;

            for i in 0..(section_header.sh_size / 4) {
                let plt_address = start + (i * 4);

                // Translate the block for the plt entry
                let btr = translator.translate_block(
                    &self.backing().get_bytes(plt_address, 16),
                    plt_address,
                    &Options::default(),
                );

                let btr = match btr {
                    Ok(btr) => btr,
                    Err(_) => continue,
                };

                let cfg = btr.blockify()?;
                let block = cfg.block(cfg.entry().unwrap())?;

                let instruction = match block.instructions().first() {
                    Some(instruction) => ir::Instruction::<ir::Constant>::from_il(instruction),
                    None => continue,
                };

                if let ir::Operation::Load { index, .. } = instruction.operation() {
                    if index.all_constants() {
                        let got_address = match ir::eval(index)?.value_u64() {
                            Some(address) => address,
                            None => continue,
                        };

                        if let Some(symbol) = self.symbol(got_address).cloned() {
                            plt_symbols.push(Symbol::new(symbol.name(), plt_address));
                        }
                    }
                }
            }
        }

        Ok(plt_symbols)
    }

    pub fn function_translator(&self) -> FunctionTranslator {
        FunctionTranslator::new(self.translation_information())
    }

    fn init_symbols(&mut self) -> Result<()> {
        for symbol in self.plt_symbols()? {
            self.symbols.insert(symbol.address(), symbol);
        }
        Ok(())
    }

    pub fn translation_information(&'t self) -> TranslationInformation<'t> {
        TranslationInformation::new(
            self.architecture.as_ref(),
            self.calling_convention(),
            self.backing(),
            &self.symbols,
            self.loader(),
        )
    }

    pub fn architecture(&self) -> &dyn Architecture {
        self.architecture.as_ref()
    }

    pub fn backing(&self) -> &falcon::memory::backing::Memory {
        &self.backing
    }

    pub fn calling_convention(&self) -> &CallingConvention {
        &self.calling_convention
    }

    pub fn loader(&self) -> &dyn Loader {
        self.loader
    }

    pub fn stack_pointer(&self) -> ir::Scalar {
        self.architecture().stack_pointer().into()
    }

    pub fn symbol(&self, address: u64) -> Option<&Symbol> {
        self.symbols.get(&address)
    }
}
