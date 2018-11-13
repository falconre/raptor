use ir;


#[derive(Clone, Debug)]
pub struct ReturnSite<'f> {
    branch: ir::RefFunctionLocation<'f, ir::Constant>,
    get_return_address: Vec<ir::RefFunctionLocation<'f, ir::Constant>>
}


impl<'f> ReturnSite<'f> {
    pub fn new(
        branch: ir::RefFunctionLocation<'f, ir::Constant>,
        get_return_address: Vec<ir::RefFunctionLocation<'f, ir::Constant>>
    ) -> ReturnSite<'f> {
        ReturnSite {
            branch: branch,
            get_return_address: get_return_address
        }
    }

    pub fn branch(&self) -> &ir::RefFunctionLocation<'f, ir::Constant> {
        &self.branch
    }

    pub fn get_return_address(&self) -> &[ir::RefFunctionLocation<'f, ir::Constant>] {
        &self.get_return_address
    }
}