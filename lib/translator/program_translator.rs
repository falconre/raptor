use error::*;
use falcon;
use falcon::analysis::calling_convention::CallingConvention;
use falcon::architecture::Architecture;
use falcon::loader::{Elf, Loader, Symbol};
use falcon::translator::TranslationMemory;
use ir;
use std::collections::HashMap;
use translator::{FunctionTranslator, TranslationInformation};

pub struct ProgramTranslator<'t> {
    architecture: Box<Architecture>,
    calling_convention: CallingConvention,
    loader: &'t Loader,
    backing: falcon::memory::backing::Memory,
    symbols: HashMap<u64, Symbol>,
    // relocations: HashMap<u64, Symbol>
}

impl<'t> ProgramTranslator<'t> {
    pub fn new(
        architecture: Box<Architecture>,
        calling_convention: CallingConvention,
        loader: &'t Loader,
    ) -> Result<ProgramTranslator<'t>> {
        let mut translator = ProgramTranslator {
            architecture: architecture,
            calling_convention: calling_convention,
            loader: loader,
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

        for symbol in starting_symbols {
            trace!(
                "Starting symbols: 0x{:08x} = {}",
                symbol.address(),
                symbol.name()
            );
        }

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
            let name: &str = match elf.shdr_strtab.get(section_header.sh_name) {
                Some(name) => name?,
                None => continue,
            };

            if name != ".plt" {
                continue;
            }

            trace!("found plt");

            let start = section_header.sh_addr;

            for i in 0..(section_header.sh_size / 4) {
                let plt_address = start + (i * 4);

                // Translate the block for the plt entry
                let btr = translator
                    .translate_block(&self.backing().get_bytes(plt_address, 16), plt_address);

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

                match instruction.operation() {
                    ir::Operation::Load { index, .. } => {
                        if index.all_constants() {
                            let got_address = match ir::eval(index)?.value_u64() {
                                Some(address) => address,
                                None => continue,
                            };

                            trace!(
                                "Looking for symbol plt_address=0x{:x}, got_address=0x{:x}",
                                plt_address,
                                got_address
                            );

                            if let Some(symbol) = self.symbol(got_address).cloned() {
                                plt_symbols.push(Symbol::new(symbol.name(), plt_address));
                            }
                        }
                    }
                    _ => {}
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
        )
    }

    pub fn architecture(&self) -> &Architecture {
        self.architecture.as_ref()
    }

    pub fn backing(&self) -> &falcon::memory::backing::Memory {
        &self.backing
    }

    pub fn calling_convention(&self) -> &CallingConvention {
        &self.calling_convention
    }

    pub fn loader(&self) -> &Loader {
        self.loader
    }

    pub fn stack_pointer(&self) -> ir::Scalar {
        self.architecture().stack_pointer().into()
    }

    pub fn symbol(&self, address: u64) -> Option<&Symbol> {
        self.symbols.get(&address)
    }
}
