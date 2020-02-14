use crate::ir;

#[derive(Clone, Debug)]
pub struct CallSite<'f> {
    branch: ir::RefFunctionLocation<'f, ir::Constant>,
    set_return_address: Vec<ir::RefFunctionLocation<'f, ir::Constant>>,
}

impl<'f> CallSite<'f> {
    pub fn new(
        branch: ir::RefFunctionLocation<'f, ir::Constant>,
        set_return_address: Vec<ir::RefFunctionLocation<'f, ir::Constant>>,
    ) -> CallSite<'f> {
        CallSite {
            branch: branch,
            set_return_address: set_return_address,
        }
    }

    pub fn branch(&self) -> &ir::RefFunctionLocation<'f, ir::Constant> {
        &self.branch
    }

    pub fn set_return_address(&self) -> &[ir::RefFunctionLocation<'f, ir::Constant>] {
        &self.set_return_address
    }
}
