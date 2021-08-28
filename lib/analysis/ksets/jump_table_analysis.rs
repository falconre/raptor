use crate::analysis::fixed_point;
use crate::analysis::ksets::{KSet, KSetValue};
use crate::analysis::strided_intervals;
use crate::analysis::strided_intervals::StridedInterval;
use crate::error::*;
use crate::ir;
use falcon::il;
use falcon::memory::backing;
use std::cmp::{Ordering, PartialOrd};
use std::collections::{BTreeSet, HashMap};

pub fn jump_table_analysis(
    function: &ir::Function<ir::Constant>,
    strided_intervals: &HashMap<ir::ProgramLocation, strided_intervals::State>,
    backing: &backing::Memory,
) -> Result<Vec<JumpTable>> {
    let jump_table_analysis = JumpTableAnalysis::new(strided_intervals, backing);

    let states: HashMap<ir::ProgramLocation, State> =
        fixed_point::fixed_point_forward(&jump_table_analysis, function)?;

    let states = fixed_point::incoming_results(&jump_table_analysis, function, states, State::new)?;

    let mut jump_tables = Vec::new();

    for (location, state) in states {
        let rpl = location.apply(function)?;

        if let Some(instruction) = rpl.instruction() {
            if !instruction.operation().is_branch() {
                continue;
            }
            let target = instruction
                .operation()
                .target()
                .ok_or("Failed to get branch target")?;

            // This only works if the target is a scalar
            let target_scalar = match target.scalar() {
                Some(scalar) => scalar,
                None => continue,
            };

            let kset = state.eval(&target.into())?;

            if kset.value().value().is_some() {
                let mut jump_table_entries = Vec::new();
                for address in kset.value().value().ok_or("Failed to get kset value")? {
                    let condition = il::Expression::cmpeq(
                        il::scalar(target_scalar.name(), target_scalar.bits()).into(),
                        address.clone().into(),
                    )?;
                    let jump_table_entry = JumpTableEntry::new(
                        address
                            .value_u64()
                            .ok_or("Failed to get jump table value")?,
                        condition,
                    );
                    jump_table_entries.push(jump_table_entry);
                }

                jump_table_entries.sort();

                let jump_table = JumpTable::new(location.clone(), jump_table_entries);
                jump_tables.push(jump_table);
            }
        }
    }

    Ok(jump_tables)
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct JumpTable {
    location: ir::ProgramLocation,
    entries: Vec<JumpTableEntry>,
}

impl JumpTable {
    pub fn new(location: ir::ProgramLocation, entries: Vec<JumpTableEntry>) -> JumpTable {
        JumpTable { location, entries }
    }

    /// Get the program location of the branch for the jump table
    pub fn location(&self) -> &ir::ProgramLocation {
        &self.location
    }
    /// Get the jump table entries
    pub fn entries(&self) -> &[JumpTableEntry] {
        &self.entries
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct JumpTableEntry {
    address: u64,
    condition: il::Expression,
}

impl JumpTableEntry {
    pub fn new(address: u64, condition: il::Expression) -> JumpTableEntry {
        JumpTableEntry { address, condition }
    }

    /// Get the target address for the jump table
    pub fn address(&self) -> u64 {
        self.address
    }
    /// Get the condition which guards this entry in the jump table
    pub fn condition(&self) -> &il::Expression {
        &self.condition
    }
}

#[derive(Debug)]
struct JumpTableAnalysis<'j> {
    strided_intervals: &'j HashMap<ir::ProgramLocation, strided_intervals::State>,
    backing: &'j backing::Memory,
}

impl<'j> JumpTableAnalysis<'j> {
    fn new(
        strided_intervals: &'j HashMap<ir::ProgramLocation, strided_intervals::State>,
        backing: &'j backing::Memory,
    ) -> JumpTableAnalysis<'j> {
        JumpTableAnalysis {
            strided_intervals,
            backing,
        }
    }

    fn backing(&self) -> &backing::Memory {
        self.backing
    }

    /// Given a KSet of addresses, load those from the backing.
    ///
    /// If a single address cannot be loaded, the entire result is set to Top
    fn load_kset(&self, kset: &KSet, bits: usize) -> Result<KSet> {
        let mut hashset = BTreeSet::new();

        if let Some(addresses) = kset.value().value() {
            for address in addresses {
                let value = self.backing().get(
                    address
                        .value_u64()
                        .ok_or("Failed to get address for load_kset")?,
                    bits,
                );
                match value {
                    Some(value) => {
                        hashset.insert(value);
                    }
                    None => return Ok(KSet::new_top(bits)),
                }
            }
        } else {
            return Ok(KSet::new_top(bits));
        }

        Ok(KSet::new(hashset.len(), KSetValue::Value(hashset)))
    }

    fn eval(
        &self,
        program_location: &ir::ProgramLocation,
        expression: &ir::Expression<StridedInterval>,
    ) -> Result<StridedInterval> {
        self.strided_intervals
            .get(program_location)
            .ok_or("Failed to get program location for strided intervals")?
            .eval(expression)
    }
}

#[derive(Clone, Debug, PartialEq)]
struct State {
    variables: HashMap<ir::Variable, KSet>,
}

impl State {
    fn new() -> State {
        State {
            variables: HashMap::new(),
        }
    }

    fn variables(&self) -> &HashMap<ir::Variable, KSet> {
        &self.variables
    }

    fn join(mut self, other: &State) -> Result<State> {
        for (variable, kset) in other.variables() {
            let kset: KSet = self
                .variables
                .get(variable)
                .map(|k| k.join(kset))
                .unwrap_or(kset.clone());
            self.variables.insert(variable.clone(), kset);
        }
        Ok(self)
    }

    fn top(&mut self) {
        self.variables
            .iter_mut()
            .for_each(|(_, kset)| *kset = KSet::new_top(kset.bits()));
    }

    fn set(&mut self, variable: ir::Variable, kset: KSet) {
        self.variables.insert(variable, kset);
    }

    fn get(&self, variable: &ir::Variable) -> Option<&KSet> {
        self.variables.get(variable)
    }

    fn eval(&self, expression: &ir::Expression<KSet>) -> Result<KSet> {
        Ok(match expression {
            ir::Expression::LValue(lvalue) => match lvalue.as_ref() {
                ir::LValue::Variable(variable) => self
                    .get(variable)
                    .cloned()
                    .unwrap_or(KSet::new_top(variable.bits())),
                ir::LValue::Dereference(dereference) => KSet::new_top(dereference.bits()),
            },
            ir::Expression::RValue(rvalue) => match rvalue.as_ref() {
                ir::RValue::Value(value) => value.clone(),
                ir::RValue::Reference(reference) => KSet::new_top(reference.bits()),
            },
            ir::Expression::Add(lhs, rhs) => self.eval(lhs)?.add(&self.eval(rhs)?)?,
            ir::Expression::Sub(lhs, rhs) => self.eval(lhs)?.sub(&self.eval(rhs)?)?,
            ir::Expression::Mul(lhs, rhs) => self.eval(lhs)?.mul(&self.eval(rhs)?)?,
            ir::Expression::Divu(lhs, rhs) => self.eval(lhs)?.divu(&self.eval(rhs)?)?,
            ir::Expression::Modu(lhs, rhs) => self.eval(lhs)?.modu(&self.eval(rhs)?)?,
            ir::Expression::Divs(lhs, rhs) => self.eval(lhs)?.divs(&self.eval(rhs)?)?,
            ir::Expression::Mods(lhs, rhs) => self.eval(lhs)?.mods(&self.eval(rhs)?)?,
            ir::Expression::And(lhs, rhs) => self.eval(lhs)?.and(&self.eval(rhs)?)?,
            ir::Expression::Or(lhs, rhs) => self.eval(lhs)?.or(&self.eval(rhs)?)?,
            ir::Expression::Xor(lhs, rhs) => self.eval(lhs)?.xor(&self.eval(rhs)?)?,
            ir::Expression::Shl(lhs, rhs) => self.eval(lhs)?.shl(&self.eval(rhs)?)?,
            ir::Expression::Shr(lhs, rhs) => self.eval(lhs)?.shr(&self.eval(rhs)?)?,
            ir::Expression::Cmpeq(lhs, rhs) => self.eval(lhs)?.cmpeq(&self.eval(rhs)?)?,
            ir::Expression::Cmpneq(lhs, rhs) => self.eval(lhs)?.cmpneq(&self.eval(rhs)?)?,
            ir::Expression::Cmplts(lhs, rhs) => self.eval(lhs)?.cmplts(&self.eval(rhs)?)?,
            ir::Expression::Cmpltu(lhs, rhs) => self.eval(lhs)?.cmpltu(&self.eval(rhs)?)?,
            ir::Expression::Zext(bits, rhs) => self.eval(rhs)?.zext(*bits)?,
            ir::Expression::Sext(bits, rhs) => self.eval(rhs)?.sext(*bits)?,
            ir::Expression::Trun(bits, rhs) => self.eval(rhs)?.trun(*bits)?,
            ir::Expression::Ite(cond, then, else_) => {
                KSet::ite(&self.eval(cond)?, &self.eval(then)?, &self.eval(else_)?)?
            }
        })
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, rhs: &State) -> Option<Ordering> {
        let mut order = Ordering::Equal;

        for (variable, kset) in self.variables() {
            match rhs.variables().get(variable) {
                Some(rkset) => match kset.partial_cmp(rkset)? {
                    Ordering::Greater => {
                        if order == Ordering::Less {
                            return None;
                        } else {
                            order = Ordering::Greater;
                        }
                    }
                    Ordering::Less => {
                        if order == Ordering::Greater {
                            return None;
                        } else {
                            order = Ordering::Less;
                        }
                    }
                    Ordering::Equal => {}
                },
                None => {
                    if order == Ordering::Less {
                        return None;
                    } else {
                        order = Ordering::Greater;
                    }
                }
            }
        }

        for (variable, _) in rhs.variables() {
            if self.variables().get(variable).is_none() {
                if order == Ordering::Greater {
                    return None;
                } else {
                    order = Ordering::Less;
                }
            }
        }

        Some(order)
    }
}

impl<'f, 'j> fixed_point::FixedPointAnalysis<'f, State, ir::Constant> for JumpTableAnalysis<'j> {
    fn trans(
        &self,
        location: &ir::RefProgramLocation<ir::Constant>,
        state: Option<State>,
    ) -> Result<State> {
        let mut state = state.unwrap_or(State::new());

        match location.function_location() {
            ir::RefFunctionLocation::Instruction(_, instruction) => {
                match instruction.operation() {
                    ir::Operation::Assign { dst, src } => {
                        let src = state.eval(&src.into())?;

                        assert!(dst.bits() == src.bits());
                        state.set(dst.clone(), src);
                    }
                    ir::Operation::Branch { .. }
                    | ir::Operation::Call { .. }
                    | ir::Operation::Intrinsic(_)
                    | ir::Operation::Return(_) => {
                        state.top();
                    }
                    ir::Operation::Load { dst, index } => {
                        // Get the strided interval for this index, if one
                        // exists
                        let index = self.eval(&location.clone().into(), &index.into())?;
                        // Turn that into a ksets
                        let index = KSet::from_strided_interval(&index);

                        state.set(dst.clone(), self.load_kset(&index, dst.bits())?);
                    }
                    ir::Operation::Store { .. } | ir::Operation::Nop(_) => {}
                }
            }
            ir::RefFunctionLocation::Edge(_) | ir::RefFunctionLocation::EmptyBlock(_) => {}
        }

        Ok(state)
    }

    fn join(&self, state0: State, state1: &State) -> Result<State> {
        state0.join(state1)
    }
}
