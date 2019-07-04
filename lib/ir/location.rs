use ir::*;
use std::collections::VecDeque;
use std::fmt;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum RefFunctionLocation<'r, V: Value + 'r> {
    Instruction(&'r Block<V>, &'r Instruction<V>),
    EmptyBlock(&'r Block<V>),
    Edge(&'r Edge<V>),
}

impl<'r, V: Value> RefFunctionLocation<'r, V> {
    pub fn from_block(block: &'r Block<V>) -> RefFunctionLocation<'r, V> {
        if let Some(instruction) = block.instructions().first() {
            RefFunctionLocation::Instruction(block, instruction)
        } else {
            RefFunctionLocation::EmptyBlock(block)
        }
    }

    pub fn from_address(function: &Function<V>, address: u64) -> Option<RefFunctionLocation<V>> {
        for block in function.blocks() {
            for instruction in block.instructions() {
                if instruction.address().map(|a| a == address).unwrap_or(false) {
                    return Some(RefFunctionLocation::Instruction(block, instruction));
                }
            }
        }
        None
    }

    pub fn instruction(&self) -> Option<&Instruction<V>> {
        match self {
            RefFunctionLocation::Instruction(_, instruction) => Some(instruction),
            _ => None,
        }
    }

    pub fn block(&self) -> Option<&Block<V>> {
        match self {
            RefFunctionLocation::Instruction(block, _) => Some(block),
            RefFunctionLocation::EmptyBlock(block) => Some(block),
            _ => None,
        }
    }

    pub fn empty_block(&self) -> Option<&Block<V>> {
        match self {
            RefFunctionLocation::EmptyBlock(empty_block) => Some(empty_block),
            _ => None,
        }
    }

    pub fn edge(&self) -> Option<&Edge<V>> {
        match self {
            RefFunctionLocation::Edge(edge) => Some(edge),
            _ => None,
        }
    }

    pub fn address(&self) -> Option<u64> {
        self.instruction()
            .and_then(|instruction| instruction.address())
    }
}

impl<'r, V: Value> fmt::Display for RefFunctionLocation<'r, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RefFunctionLocation::Instruction(_, instruction) => instruction.fmt(f),
            RefFunctionLocation::Edge(edge) => edge.fmt(f),
            RefFunctionLocation::EmptyBlock(block) => write!(f, "[ block 0x{:x} ]", block.index()),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct RefProgramLocation<'r, V: Value + 'r> {
    function: &'r Function<V>,
    function_location: RefFunctionLocation<'r, V>,
}

impl<'r, V: Value> RefProgramLocation<'r, V> {
    pub fn new(
        function: &'r Function<V>,
        function_location: RefFunctionLocation<'r, V>,
    ) -> RefProgramLocation<'r, V> {
        RefProgramLocation {
            function: function,
            function_location: function_location,
        }
    }

    pub fn address(&self) -> Option<u64> {
        self.function_location
            .instruction()
            .and_then(|i| i.address())
    }

    pub fn block(&self) -> Option<&Block<V>> {
        self.function_location.block()
    }

    pub fn edge(&self) -> Option<&Edge<V>> {
        self.function_location.edge()
    }

    pub fn function(&self) -> &'r Function<V> {
        self.function
    }

    pub fn function_location(&self) -> &RefFunctionLocation<'r, V> {
        &self.function_location
    }

    pub fn instruction(&self) -> Option<&Instruction<V>> {
        self.function_location.instruction()
    }

    pub fn from_function(function: &Function<V>) -> Option<RefProgramLocation<V>> {
        let block = function
            .block(function.control_flow_graph().entry()?)
            .ok()?;

        Some(match block.instructions().first() {
            Some(instruction) => RefProgramLocation::new(
                function,
                RefFunctionLocation::Instruction(block, instruction),
            ),
            None => RefProgramLocation::new(function, RefFunctionLocation::EmptyBlock(block)),
        })
    }

    pub fn from_address(program: &Program<V>, address: u64) -> Option<RefProgramLocation<V>> {
        let mut function = None;

        for f in program.functions() {
            if f.address() > address {
                continue;
            }

            if function == None {
                function = Some(f);
            }

            if f.address() > function.unwrap().address() {
                function = Some(f);
            }
        }

        if let Some(function) = function {
            match RefFunctionLocation::from_address(function, address) {
                Some(rfl) => return Some(RefProgramLocation::new(function, rfl)),
                None => {}
            }
        }

        for function in program.functions() {
            match RefFunctionLocation::from_address(function, address) {
                Some(rfl) => return Some(RefProgramLocation::new(function, rfl)),
                None => {}
            }
        }

        None
    }

    pub fn forward(&self) -> Vec<RefProgramLocation<'r, V>> {
        match self.function_location() {
            RefFunctionLocation::Instruction(block, instruction) => {
                let instructions = block.instructions();
                for i in 0..instructions.len() {
                    if instructions[i].index() == instruction.index() {
                        if i + 1 < block.instructions().len() {
                            return vec![RefProgramLocation::new(
                                self.function(),
                                RefFunctionLocation::Instruction(block, &instructions[i + 1]),
                            )];
                        }
                    }
                }

                self.function()
                    .control_flow_graph()
                    .edges_out(block.index())
                    .expect("Failed to get edges out")
                    .iter()
                    .map(|edge| {
                        RefProgramLocation::new(self.function(), RefFunctionLocation::Edge(edge))
                    })
                    .collect::<Vec<RefProgramLocation<V>>>()
            }
            RefFunctionLocation::EmptyBlock(block) => self
                .function()
                .control_flow_graph()
                .edges_out(block.index())
                .expect("Failed to get edges out")
                .iter()
                .map(|edge| {
                    RefProgramLocation::new(self.function(), RefFunctionLocation::Edge(edge))
                })
                .collect::<Vec<RefProgramLocation<V>>>(),
            RefFunctionLocation::Edge(edge) => {
                let block = self
                    .function()
                    .block(edge.tail())
                    .expect("Failed to get block");

                let ref_function_location = if let Some(instruction) = block.instructions().first()
                {
                    RefFunctionLocation::Instruction(block, instruction)
                } else {
                    RefFunctionLocation::EmptyBlock(block)
                };

                vec![RefProgramLocation::new(
                    self.function(),
                    ref_function_location,
                )]
            }
        }
    }

    pub fn backward(&self) -> Vec<RefProgramLocation<'r, V>> {
        match self.function_location() {
            RefFunctionLocation::Instruction(block, instruction) => {
                let instructions = block.instructions();
                for i in (0..instructions.len()).rev() {
                    if instructions[i].index() == instruction.index() {
                        if i > 0 {
                            return vec![RefProgramLocation::new(
                                self.function(),
                                RefFunctionLocation::Instruction(block, &instructions[i - 1]),
                            )];
                        }
                    }
                }

                self.function()
                    .control_flow_graph()
                    .edges_in(block.index())
                    .expect("Failed to get edges out")
                    .iter()
                    .map(|edge| {
                        RefProgramLocation::new(self.function(), RefFunctionLocation::Edge(edge))
                    })
                    .collect::<Vec<RefProgramLocation<V>>>()
            }
            RefFunctionLocation::EmptyBlock(block) => self
                .function()
                .control_flow_graph()
                .edges_in(block.index())
                .expect("Failed to get edges out")
                .iter()
                .map(|edge| {
                    RefProgramLocation::new(self.function(), RefFunctionLocation::Edge(edge))
                })
                .collect::<Vec<RefProgramLocation<V>>>(),
            RefFunctionLocation::Edge(edge) => {
                let block = self
                    .function()
                    .block(edge.head())
                    .expect("Failed to get block");

                let ref_function_location = if let Some(instruction) = block.instructions().last() {
                    RefFunctionLocation::Instruction(block, instruction)
                } else {
                    RefFunctionLocation::EmptyBlock(block)
                };

                vec![RefProgramLocation::new(
                    self.function(),
                    ref_function_location,
                )]
            }
        }
    }

    pub fn next_instructions(&self) -> Vec<RefProgramLocation<'r, V>> {
        let mut next_instructions = Vec::new();

        let mut queue = VecDeque::new();
        queue.push_back(self.clone());
        while queue.len() > 0 {
            let location = queue.pop_front().unwrap();

            for location in location.forward() {
                if location.instruction().is_some() {
                    next_instructions.push(location);
                } else {
                    queue.push_back(location);
                }
            }
        }

        next_instructions
    }
}

impl<'r, V: Value> fmt::Display for RefProgramLocation<'r, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "f:0x{:X} {}",
            self.function().index().unwrap_or(0),
            self.function_location()
        )
    }
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum FunctionLocation {
    Instruction(usize, usize),
    Edge(usize, usize),
    EmptyBlock(usize),
}

impl FunctionLocation {
    pub fn block_index(&self) -> Option<usize> {
        match *self {
            FunctionLocation::Instruction(block_index, _) => Some(block_index),
            FunctionLocation::EmptyBlock(block_index) => Some(block_index),
            _ => None,
        }
    }

    pub fn instruction_index(&self) -> Option<usize> {
        match *self {
            FunctionLocation::Instruction(_, instruction_index) => Some(instruction_index),
            _ => None,
        }
    }

    pub fn edge_head(&self) -> Option<usize> {
        match *self {
            FunctionLocation::Edge(head, _) => Some(head),
            _ => None,
        }
    }

    pub fn edge_tail(&self) -> Option<usize> {
        match *self {
            FunctionLocation::Edge(_, tail) => Some(tail),
            _ => None,
        }
    }

    pub fn apply<'f, V: Value>(
        &self,
        function: &'f Function<V>,
    ) -> Result<RefFunctionLocation<'f, V>> {
        fn apply_<'f, V: Value>(
            fl: &FunctionLocation,
            function: &'f Function<V>,
        ) -> Option<RefFunctionLocation<'f, V>> {
            match fl {
                FunctionLocation::Instruction(block_index, instruction_index) => {
                    let block = function.block(*block_index).ok()?;
                    let instruction = block.instruction(*instruction_index)?;
                    Some(RefFunctionLocation::Instruction(block, instruction))
                }
                FunctionLocation::Edge(head_index, tail_index) => Some(RefFunctionLocation::Edge(
                    function.edge(*head_index, *tail_index).ok()?,
                )),
                FunctionLocation::EmptyBlock(block_index) => Some(RefFunctionLocation::EmptyBlock(
                    function.block(*block_index).ok()?,
                )),
            }
        }

        match apply_(self, function) {
            Some(location) => Ok(location),
            None => Err(ErrorKind::OwnedLocationApplication.into()),
        }
    }
}

impl<'r, V: Value> From<RefFunctionLocation<'r, V>> for FunctionLocation {
    fn from(rfl: RefFunctionLocation<'r, V>) -> FunctionLocation {
        match rfl {
            RefFunctionLocation::Instruction(block, instruction) => {
                FunctionLocation::Instruction(block.index(), instruction.index())
            }
            RefFunctionLocation::EmptyBlock(block) => FunctionLocation::EmptyBlock(block.index()),
            RefFunctionLocation::Edge(edge) => FunctionLocation::Edge(edge.head(), edge.tail()),
        }
    }
}

impl fmt::Display for FunctionLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionLocation::Instruction(block_index, instruction_index) => write!(
                f,
                "FunctionLocation::Instruction({}, {})",
                block_index, instruction_index
            ),
            FunctionLocation::EmptyBlock(block_index) => {
                write!(f, "FunctionLocation::EmptyBlock({})", block_index)
            }
            FunctionLocation::Edge(head_index, tail_index) => {
                write!(f, "FunctionLocation::Edge({}, {}", head_index, tail_index)
            }
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct ProgramLocation {
    function_index: usize,
    function_location: FunctionLocation,
}

impl ProgramLocation {
    pub fn new(function_index: usize, function_location: FunctionLocation) -> ProgramLocation {
        ProgramLocation {
            function_index: function_index,
            function_location: function_location,
        }
    }

    pub fn apply<'f, V: 'f + Value>(
        &self,
        function: &'f Function<V>,
    ) -> Result<RefProgramLocation<'f, V>> {
        Ok(RefProgramLocation::new(
            function,
            self.function_location.apply(function)?,
        ))
    }

    pub fn function_index(&self) -> usize {
        self.function_index
    }
    pub fn function_location(&self) -> &FunctionLocation {
        &self.function_location
    }
}

impl<'r, V: Value> From<RefProgramLocation<'r, V>> for ProgramLocation {
    fn from(rpl: RefProgramLocation<'r, V>) -> ProgramLocation {
        ProgramLocation::new(
            rpl.function()
                .index()
                .expect("Could not get function index"),
            rpl.function_location.clone().into(),
        )
    }
}
