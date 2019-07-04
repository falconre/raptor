//! Bindings around the z3 solver for Raptor IR
//!
//! The z3 solver only works over certain expression types. Expressions given to
//! the z3 solver must not include references or dereferences.

use error::*;
use falcon_z3::{Ast, Check, Config, Context, Model, Optimize, Solver};
use ir;

fn return_solver_result(
    solver: &Solver,
    context: &Context,
    ast: &Ast,
    bits: usize,
) -> Result<Option<ir::Constant>> {
    Ok(match solver.check() {
        Check::Unsat | Check::Unknown => None,
        Check::Sat => Model::new(&context, &solver)
            .and_then(|model| model.get_const_interp(&ast))
            .and_then(|constant_ast| constant_ast.get_numeral_decimal_string(&context))
            .and_then(|numeral_dec_str| {
                ir::Constant::from_decimal_string(&numeral_dec_str, bits).ok()
            }),
    })
}

fn return_optimize_result(
    optimize: &Optimize,
    context: &Context,
    ast: &Ast,
    bits: usize,
) -> Result<Option<ir::Constant>> {
    Ok(match optimize.check() {
        Check::Unsat | Check::Unknown => None,
        Check::Sat => Model::new_optimize(&context, &optimize)
            .and_then(|model| model.get_const_interp(&ast))
            .and_then(|constant_ast| constant_ast.get_numeral_decimal_string(&context))
            .and_then(|numeral_dec_str| {
                ir::Constant::from_decimal_string(&numeral_dec_str, bits).ok()
            }),
    })
}

fn solver_init(
    solver: &Solver,
    context: &Context,
    constraints: &[ir::Expression<ir::Constant>],
) -> Result<()> {
    let sort = context.mk_bv_sort(1);
    let one = context.mk_numeral(1, &sort)?;

    for constraint in constraints {
        solver.assert(&context.eq(&one, &expression_to_ast(&context, constraint)?));
    }

    Ok(())
}

fn optimize_init(
    optimize: &Optimize,
    context: &Context,
    constraints: &[ir::Expression<ir::Constant>],
) -> Result<()> {
    let sort = context.mk_bv_sort(1);
    let one = context.mk_numeral(1, &sort)?;

    for constraint in constraints {
        optimize.assert(&context.eq(&one, &expression_to_ast(&context, constraint)?));
    }

    Ok(())
}

/// Maximize the given value by the given constraints.
pub fn maximize(
    constraints: &[ir::Expression<ir::Constant>],
    value: &ir::Expression<ir::Constant>,
) -> Result<Option<ir::Constant>> {
    let config = Config::new().enable_model();
    let context = Context::new(config);
    let optimize = Optimize::new(&context);

    optimize_init(&optimize, &context, constraints)?;

    let optimize_result = context.mk_var("OPTIMIZE_RESULT", &context.mk_bv_sort(value.bits()))?;

    optimize.assert(&context.eq(&optimize_result, &expression_to_ast(&context, value)?));

    optimize.maximize(&optimize_result);

    return_optimize_result(&optimize, &context, &optimize_result, value.bits())
}

/// Minimize the given value by the given constraints.
pub fn minimize(
    constraints: &[ir::Expression<ir::Constant>],
    value: &ir::Expression<ir::Constant>,
) -> Result<Option<ir::Constant>> {
    let config = Config::new().enable_model();
    let context = Context::new(config);
    let optimize = Optimize::new(&context);

    optimize_init(&optimize, &context, constraints)?;

    let optimize_result = context.mk_var("OPTIMIZE_RESULT", &context.mk_bv_sort(value.bits()))?;

    optimize.assert(&context.eq(&optimize_result, &expression_to_ast(&context, value)?));

    optimize.minimize(&optimize_result);

    return_optimize_result(&optimize, &context, &optimize_result, value.bits())
}

/// Solve for a possible solution to the given value with the given constraints.
pub fn solve(
    constraints: &[ir::Expression<ir::Constant>],
    value: &ir::Expression<ir::Constant>,
) -> Result<Option<ir::Constant>> {
    let config = Config::new().enable_model();
    let context = Context::new(config);
    let solver = Solver::new(&context);

    solver_init(&solver, &context, constraints)?;

    let solver_result = context.mk_var("SOLVER_RESULT", &context.mk_bv_sort(value.bits()))?;

    solver.assert(&context.eq(&solver_result, &expression_to_ast(&context, value)?));

    return_solver_result(&solver, &context, &solver_result, value.bits())
}

fn expression_to_ast(context: &Context, expression: &ir::Expression<ir::Constant>) -> Result<Ast> {
    Ok(match expression {
        ir::Expression::RValue(rvalue) => match rvalue.as_ref() {
            ir::RValue::Value(constant) => {
                if let Some(value) = constant.value_u64() {
                    let sort = context.mk_bv_sort(constant.bits());
                    context.mk_numeral(value, &sort)?
                } else {
                    let big_uint = constant.value();
                    let sort = context.mk_bv_sort(8);
                    let bytes = big_uint.to_bytes_le();
                    let mut v = if bytes.len() == 0 {
                        context.mk_numeral(0, &sort)?
                    } else {
                        context.mk_numeral(bytes[0] as u64, &sort)?
                    };
                    for i in 1..(constant.bits() / 8) {
                        let numeral = if bytes.len() <= i {
                            context.mk_numeral(0, &sort)?
                        } else {
                            context.mk_numeral(bytes[i] as u64, &sort)?
                        };
                        v = context.concat(&numeral, &v);
                    }
                    v
                }
            }
            ir::RValue::Reference(_) => Err(ErrorKind::SolverReference)?,
        },
        ir::Expression::LValue(lvalue) => match lvalue.as_ref() {
            ir::LValue::Variable(variable) => match variable {
                ir::Variable::Scalar(scalar) => {
                    let sort = context.mk_bv_sort(scalar.bits());
                    context.mk_var(scalar.name(), &sort)?
                }
                ir::Variable::StackVariable(stack_variable) => {
                    let sort = context.mk_bv_sort(stack_variable.bits());
                    context.mk_var(stack_variable.name(), &sort)?
                }
            },
            ir::LValue::Dereference(_) => Err(ErrorKind::SolverDereference)?,
        },
        ir::Expression::Add(ref lhs, ref rhs) => context.bvadd(
            &expression_to_ast(context, lhs)?,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Sub(ref lhs, ref rhs) => context.bvsub(
            &expression_to_ast(context, lhs)?,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Mul(ref lhs, ref rhs) => context.bvmul(
            &expression_to_ast(context, lhs)?,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Divu(ref lhs, ref rhs) => context.bvudiv(
            &expression_to_ast(context, lhs)?,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Modu(ref lhs, ref rhs) => context.bvurem(
            &expression_to_ast(context, lhs)?,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Divs(ref lhs, ref rhs) => context.bvsdiv(
            &expression_to_ast(context, lhs)?,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Mods(ref lhs, ref rhs) => context.bvsrem(
            &expression_to_ast(context, lhs)?,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::And(ref lhs, ref rhs) => context.bvand(
            &expression_to_ast(context, lhs)?,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Or(ref lhs, ref rhs) => context.bvor(
            &expression_to_ast(context, lhs)?,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Xor(ref lhs, ref rhs) => context.bvxor(
            &expression_to_ast(context, lhs)?,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Shl(ref lhs, ref rhs) => context.bvshl(
            &expression_to_ast(context, lhs)?,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Shr(ref lhs, ref rhs) => context.bvlshr(
            &expression_to_ast(context, lhs)?,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Cmpeq(ref lhs, ref rhs) => {
            let sort = context.mk_bv_sort(1);
            context.ite(
                &context.eq(
                    &expression_to_ast(context, lhs)?,
                    &expression_to_ast(context, rhs)?,
                ),
                &context.mk_numeral(1, &sort)?,
                &context.mk_numeral(0, &sort)?,
            )
        }
        ir::Expression::Cmpneq(ref lhs, ref rhs) => {
            let sort = context.mk_bv_sort(1);
            context.ite(
                &context.eq(
                    &expression_to_ast(context, lhs)?,
                    &expression_to_ast(context, rhs)?,
                ),
                &context.mk_numeral(0, &sort)?,
                &context.mk_numeral(1, &sort)?,
            )
        }
        ir::Expression::Cmplts(ref lhs, ref rhs) => {
            let sort = context.mk_bv_sort(1);
            context.ite(
                &context.bvslt(
                    &expression_to_ast(context, lhs)?,
                    &expression_to_ast(context, rhs)?,
                ),
                &context.mk_numeral(1, &sort)?,
                &context.mk_numeral(0, &sort)?,
            )
        }
        ir::Expression::Cmpltu(ref lhs, ref rhs) => {
            let sort = context.mk_bv_sort(1);
            context.ite(
                &context.bvult(
                    &expression_to_ast(context, lhs)?,
                    &expression_to_ast(context, rhs)?,
                ),
                &context.mk_numeral(1, &sort)?,
                &context.mk_numeral(0, &sort)?,
            )
        }
        ir::Expression::Zext(bits, ref rhs) => context.zero_ext(
            (bits - rhs.bits()) as u32,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Sext(bits, ref rhs) => context.sign_ext(
            (bits - rhs.bits()) as u32,
            &expression_to_ast(context, rhs)?,
        ),
        ir::Expression::Trun(bits, ref rhs) => {
            context.extract((bits - 1) as u32, 0, &expression_to_ast(context, rhs)?)
        }
        ir::Expression::Ite(ref cond, ref then, ref else_) => context.ite(
            &context.eq(
                &expression_to_ast(context, cond)?,
                &context.mk_numeral(1, &context.mk_bv_sort(1))?,
            ),
            &expression_to_ast(context, then)?,
            &expression_to_ast(context, else_)?,
        ),
    })
}
