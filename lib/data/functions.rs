use std::collections::HashMap;

lazy_static! {
    /// Statically defined functions types
    pub static ref FUNCTIONS: Functions = Functions::create();
}

#[derive(Clone, Debug)]
pub enum ParameterType {
    VoidPointer,
    UnboundedArray,
    BoundedArray(usize),
    Pointer(Box<ParameterType>),
    CharPointer,
    Value,
}

impl ParameterType {
    pub fn void_pointer(&self) -> bool {
        match self {
            ParameterType::VoidPointer => true,
            _ => false,
        }
    }

    pub fn unbounded_array(&self) -> bool {
        match self {
            ParameterType::UnboundedArray => true,
            _ => false,
        }
    }

    pub fn bounded_array(&self) -> Option<usize> {
        match self {
            ParameterType::BoundedArray(size) => Some(*size),
            _ => None,
        }
    }

    pub fn pointer(&self) -> Option<&ParameterType> {
        match self {
            ParameterType::Pointer(pointer) => Some(pointer.as_ref()),
            _ => None,
        }
    }

    pub fn value(&self) -> bool {
        match self {
            ParameterType::Value => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    name: String,
    parameters: Vec<ParameterType>,
    return_type: Option<ParameterType>,
    no_return: bool,
}

impl FunctionDeclaration {
    pub fn new<S: Into<String>>(
        name: S,
        parameters: Vec<ParameterType>,
        return_type: Option<ParameterType>,
        no_return: bool,
    ) -> FunctionDeclaration {
        FunctionDeclaration {
            name: name.into(),
            parameters: parameters,
            return_type: return_type,
            no_return: no_return,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn parameters(&self) -> &[ParameterType] {
        &self.parameters
    }
    pub fn return_type(&self) -> Option<&ParameterType> {
        self.return_type.as_ref()
    }
    pub fn no_return(&self) -> bool {
        self.no_return
    }
}

#[derive(Clone, Debug)]
pub struct Functions {
    functions: HashMap<String, FunctionDeclaration>,
}

impl Functions {
    pub fn create() -> Functions {
        let mut functions = Functions {
            functions: HashMap::new(),
        };

        functions.push_function_declaration(FunctionDeclaration::new(
            "__stack_chk_fail",
            vec![],
            None,
            true,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "_IO_getc",
            vec![],
            Some(ParameterType::Value),
            true,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "access",
            vec![ParameterType::CharPointer, ParameterType::Value],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "feof",
            vec![ParameterType::VoidPointer],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "fflush",
            vec![ParameterType::VoidPointer],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "fopen",
            vec![ParameterType::CharPointer, ParameterType::CharPointer],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "fprintf",
            vec![
                ParameterType::VoidPointer,
                ParameterType::CharPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "free",
            vec![ParameterType::VoidPointer],
            None,
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "getenv",
            vec![ParameterType::CharPointer],
            Some(ParameterType::CharPointer),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "malloc",
            vec![ParameterType::VoidPointer],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "memcpy",
            vec![
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::Value,
            ],
            Some(ParameterType::VoidPointer),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "memset",
            vec![
                ParameterType::VoidPointer,
                ParameterType::Value,
                ParameterType::Value,
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "printf",
            vec![
                ParameterType::CharPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "puts",
            vec![ParameterType::CharPointer],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "read",
            vec![
                ParameterType::Value,
                ParameterType::VoidPointer,
                ParameterType::Value,
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "rename",
            vec![ParameterType::CharPointer, ParameterType::CharPointer],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "sprintf",
            vec![
                ParameterType::CharPointer,
                ParameterType::CharPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "snprintf",
            vec![
                ParameterType::CharPointer,
                ParameterType::Value,
                ParameterType::CharPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "strcmp",
            vec![ParameterType::CharPointer, ParameterType::CharPointer],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "strncmp",
            vec![
                ParameterType::CharPointer,
                ParameterType::CharPointer,
                ParameterType::Value,
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "strchr",
            vec![ParameterType::CharPointer, ParameterType::Value],
            Some(ParameterType::CharPointer),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "strcpy",
            vec![ParameterType::CharPointer, ParameterType::CharPointer],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "strcspn",
            vec![ParameterType::CharPointer, ParameterType::CharPointer],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "strncpy",
            vec![
                ParameterType::CharPointer,
                ParameterType::CharPointer,
                ParameterType::Value,
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "strrchr",
            vec![ParameterType::CharPointer, ParameterType::Value],
            Some(ParameterType::CharPointer),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "strlen",
            vec![ParameterType::CharPointer],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "strstr",
            vec![ParameterType::CharPointer, ParameterType::CharPointer],
            Some(ParameterType::CharPointer),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "strtol",
            vec![ParameterType::CharPointer],
            Some(ParameterType::CharPointer),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "system",
            vec![ParameterType::CharPointer],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "vsnprintf",
            vec![
                ParameterType::CharPointer,
                ParameterType::Value,
                ParameterType::CharPointer,
                ParameterType::Value,
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "write",
            vec![
                ParameterType::Value,
                ParameterType::VoidPointer,
                ParameterType::Value,
            ],
            Some(ParameterType::Value),
            false,
        ));

        // CGC calls
        functions.push_function_declaration(FunctionDeclaration::new(
            "_terminate",
            vec![ParameterType::Value],
            None,
            true,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "transmit",
            vec![
                ParameterType::Value,
                ParameterType::VoidPointer,
                ParameterType::Value,
                ParameterType::Pointer(Box::new(ParameterType::Value)),
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "receive",
            vec![
                ParameterType::Value,
                ParameterType::VoidPointer,
                ParameterType::Value,
                ParameterType::Pointer(Box::new(ParameterType::Value)),
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "fdwait",
            vec![
                ParameterType::Value,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
                ParameterType::VoidPointer,
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "allocate",
            vec![
                ParameterType::Value,
                ParameterType::Value,
                ParameterType::Pointer(Box::new(ParameterType::VoidPointer)),
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "deallocate",
            vec![ParameterType::VoidPointer, ParameterType::Value],
            Some(ParameterType::Value),
            false,
        ));

        functions.push_function_declaration(FunctionDeclaration::new(
            "random",
            vec![
                ParameterType::VoidPointer,
                ParameterType::Value,
                ParameterType::Pointer(Box::new(ParameterType::Value)),
            ],
            Some(ParameterType::Value),
            false,
        ));

        functions
    }

    pub fn push_function_declaration(&mut self, function_declaration: FunctionDeclaration) {
        self.functions.insert(
            function_declaration.name().to_string(),
            function_declaration,
        );
    }

    pub fn get(&self, name: &str) -> Option<&FunctionDeclaration> {
        self.functions.get(name)
    }
}
