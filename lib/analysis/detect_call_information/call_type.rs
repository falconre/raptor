use analysis::detect_call_information::ReturnType;
use falcon::analysis::calling_convention::{CallingConvention, ReturnAddressType};
use falcon::architecture::Architecture;
use ir;

pub struct CallType {
    return_type: ReturnType,
    stack_pointer: ir::Variable,
}

impl CallType {
    pub fn new_arch(
        architecture: &Architecture,
        calling_convention: &CallingConvention,
    ) -> CallType {
        match calling_convention.return_address_type() {
            ReturnAddressType::Register(scalar) => CallType::new(
                ReturnType::Register(scalar.clone().into()),
                architecture.stack_pointer().into(),
            ),
            ReturnAddressType::Stack(_) => {
                CallType::new(ReturnType::PushStack, architecture.stack_pointer().into())
            }
        }
    }

    pub fn new(return_type: ReturnType, stack_pointer: ir::Variable) -> CallType {
        CallType {
            return_type: return_type,
            stack_pointer: stack_pointer,
        }
    }

    pub fn return_type(&self) -> &ReturnType {
        &self.return_type
    }

    pub fn stack_pointer(&self) -> &ir::Variable {
        &self.stack_pointer
    }
}
