//! A fixed-point engine for data-flow analysis.

use error::*;
use ir;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fmt::Debug;

mod weighted_location;
use self::weighted_location::WeightedLocation;

/// A trait which implements a forward, flow-sensitive analysis to a
/// fixed point.
pub trait FixedPointAnalysis<'f, State: 'f + Clone + Debug + PartialOrd, V: ir::Value> {
    /// Given an input state for a block, create an output state for this
    /// block.
    fn trans(
        &self,
        location: &ir::RefProgramLocation<'f, V>,
        state: Option<State>,
    ) -> Result<State>;

    /// Given two states, join them into one state.
    fn join(&self, state0: State, state1: &State) -> Result<State>;
}

/// A forward, work-list data-flow analysis algorithm.
///
/// When force is true, the partial order over inputs is forced by joining
/// states which do not inherently enforce the partial order.
pub fn fixed_point_forward<'f, Analysis, State, V>(
    analysis: &Analysis,
    function: &'f ir::Function<V>,
) -> Result<HashMap<ir::ProgramLocation, State>>
where
    Analysis: FixedPointAnalysis<'f, State, V>,
    State: 'f + Clone + Debug + PartialOrd,
    V: ir::Value,
{
    let mut states: HashMap<ir::ProgramLocation, State> = HashMap::new();
    let mut queue: BinaryHeap<WeightedLocation> = BinaryHeap::new();
    let mut in_queue: HashSet<WeightedLocation> = HashSet::new();

    // Compute the post order for this CFG
    let post_order = function.control_flow_graph().graph().compute_post_order(
        function
            .control_flow_graph()
            .entry()
            .ok_or("Could not find entry for function")?,
    )?;

    // Our queue is a BinaryHeap, which is a max heap, so we want items of
    // highest priority to have the highest weight. If we reversed this post
    // order to obtain the reverse post order, our weights would be backwards,
    // so we keep the weights as they currently are.

    // Create a mapping of block indices to their weights
    let block_weights = post_order
        .into_iter()
        .enumerate()
        .map(|(weight, block_index)| (block_index, weight))
        .collect::<HashMap<usize, usize>>();

    // Find the entry block to the function.
    let entry_index = function
        .control_flow_graph()
        .entry()
        .ok_or("Function's control flow graph must have entry")?;
    let entry_block = function.control_flow_graph().block(entry_index)?;

    // Add either the first instruction in the first block, or an empty block,
    // to start us off
    let weighted_location = match entry_block.instructions().first() {
        Some(ref instruction) => {
            let location = ir::RefFunctionLocation::Instruction(entry_block, instruction);
            let location = ir::RefProgramLocation::new(function, location);
            WeightedLocation::new(block_weights[&entry_index], location.into())
        }
        None => {
            let location = ir::RefFunctionLocation::EmptyBlock(entry_block);
            let location = ir::RefProgramLocation::new(function, location);
            WeightedLocation::new(block_weights[&entry_index], location.into())
        }
    };

    queue.push(weighted_location.clone());
    in_queue.insert(weighted_location);

    // This is our fixed-point loop
    while !queue.is_empty() {
        // Pop the next weighted location off the queue, and grab the location
        // in it.
        let weighted_location = queue.pop().unwrap();
        in_queue.remove(&weighted_location);
        let location = weighted_location.location();
        let rpl = location.apply(function).unwrap();
        // let location =
        //     weighted_location.location()
        //         .apply(function)
        //         .ok_or("Failed to apply weighted location")?;

        loop {
            // Join all previous states for this location
            let state =
                rpl.backward()
                    .iter()
                    .fold(None, |s, p| match states.get(&p.clone().into()) {
                        Some(in_state) => match s {
                            Some(s) => Some(analysis.join(s, in_state).unwrap()),
                            None => Some(in_state.clone()),
                        },
                        None => s,
                    });

            // Compute the transform over this location.
            let state = analysis.trans(&rpl, state)?;

            use std::cmp::Ordering;
            let state_is_less = states
                .get(&location)
                .map(|state| match state.partial_cmp(state) {
                    Some(ordering) => ordering == Ordering::Less,
                    None => true,
                })
                .unwrap_or(false);

            if state_is_less {
                panic!("State is less!");
            }

            // If nothing changes, go to the next item in the queue.
            if let Some(in_state) = states.get(&location) {
                if state == *in_state {
                    break;
                }
            }

            // Update the state for this location
            states.insert(location.clone(), state);

            // For all locations left to analyze
            for successor in rpl.forward() {
                // Create a weighted location for this successor location. If
                // there is no block index (it's an edge), use the index of the
                // predecessor/head of the edge

                let weighted_location = match successor.function_location() {
                    ir::RefFunctionLocation::EmptyBlock(block)
                    | ir::RefFunctionLocation::Instruction(block, _) => WeightedLocation::new(
                        block_weights[&block.index()],
                        successor.clone().into(),
                    ),
                    ir::RefFunctionLocation::Edge(edge) => {
                        WeightedLocation::new(block_weights[&edge.head()], successor.clone().into())
                    }
                };
                if !in_queue.contains(&weighted_location) {
                    queue.push(weighted_location);
                }
            }

            break;
        }
    }

    Ok(states)
}

/// We are computing out-going results, but sometimes we want in-coming results.
/// This takes a fixed-point result for out-going results, and turns it into a
/// result for incoming results.
pub fn incoming_results<'f, Analysis, F, State, V>(
    analysis: &Analysis,
    function: &'f ir::Function<V>,
    result: HashMap<ir::ProgramLocation, State>,
    new_state: F,
) -> Result<HashMap<ir::ProgramLocation, State>>
where
    Analysis: FixedPointAnalysis<'f, State, V>,
    F: Fn() -> State,
    State: 'f + Clone + Debug + PartialOrd,
    V: ir::Value,
{
    let mut new_result = HashMap::new();
    for (location, _) in &result {
        let mut state = new_state();
        let rpl = location.apply(function)?;
        for location in rpl.backward() {
            state = analysis.join(state, &result[&location.into()])?;
        }
        new_result.insert(location.clone(), state);
    }

    Ok(new_result)
}
