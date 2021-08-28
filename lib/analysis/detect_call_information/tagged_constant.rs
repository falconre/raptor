use crate::falcon_result;
use crate::ir;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TaggedConstant<'t> {
    constant: ir::Constant,
    locations: Vec<ir::RefFunctionLocation<'t, ir::Constant>>,
}

impl<'t> TaggedConstant<'t> {
    pub fn new(
        constant: ir::Constant,
        mut locations: Vec<ir::RefFunctionLocation<'t, ir::Constant>>,
    ) -> TaggedConstant<'t> {
        locations.sort();
        locations.dedup();
        TaggedConstant {
            constant,
            locations,
        }
    }

    pub fn constant(&self) -> &ir::Constant {
        &self.constant
    }

    pub fn locations(&self) -> &[ir::RefFunctionLocation<'t, ir::Constant>] {
        &self.locations
    }
}

impl<'t> ::falcon::memory::Value for TaggedConstant<'t> {
    fn constant(constant: ::falcon::il::Constant) -> TaggedConstant<'t> {
        TaggedConstant::new(constant, Vec::new())
    }

    fn bits(&self) -> usize {
        self.constant().bits()
    }

    fn shl(&self, bits: usize) -> ::falcon::error::Result<TaggedConstant<'t>> {
        let constant = falcon_result(ir::eval(&falcon_result(ir::shl(
            self.constant().clone().into(),
            ir::expr_const(bits as u64, self.constant().bits()),
        ))?))?;
        Ok(TaggedConstant::new(constant, self.locations().to_vec()))
    }

    fn shr(&self, bits: usize) -> ::falcon::error::Result<TaggedConstant<'t>> {
        let constant = falcon_result(ir::eval(&falcon_result(ir::shr(
            self.constant().clone().into(),
            ir::expr_const(bits as u64, self.constant().bits()),
        ))?))?;
        Ok(TaggedConstant::new(constant, self.locations().to_vec()))
    }

    fn trun(&self, bits: usize) -> ::falcon::error::Result<TaggedConstant<'t>> {
        let constant = falcon_result(ir::eval(&falcon_result(ir::trun(
            bits,
            self.constant().clone().into(),
        ))?))?;
        Ok(TaggedConstant::new(constant, self.locations().to_vec()))
    }

    fn zext(&self, bits: usize) -> ::falcon::error::Result<TaggedConstant<'t>> {
        let constant = falcon_result(ir::eval(&falcon_result(ir::zext(
            bits,
            self.constant().clone().into(),
        ))?))?;
        Ok(TaggedConstant::new(constant, self.locations().to_vec()))
    }

    fn or(&self, other: &TaggedConstant<'t>) -> ::falcon::error::Result<TaggedConstant<'t>> {
        let constant = falcon_result(ir::eval(&falcon_result(ir::or(
            other.constant().clone().into(),
            self.constant().clone().into(),
        ))?))?;
        Ok(TaggedConstant::new(
            constant,
            self.locations()
                .iter()
                .chain(other.locations())
                .cloned()
                .collect(),
        ))
    }
}
