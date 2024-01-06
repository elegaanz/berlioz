use std::collections::HashMap;

use comemo::Tracked;

use crate::{
    ast::*,
    resolve::{resolve, Source},
};

#[derive(Clone)]
pub struct Env {
    bindings: HashMap<String, Expression>,
}

impl Env {
    pub fn empty() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }
}

#[comemo::track]
impl Env {
    fn resolve(&self, name: &str) -> Option<&Expression> {
        self.bindings.get(name)
    }

    fn write(&mut self, name: String, expr: Expression) {
        self.bindings.insert(name, expr);
    }
}

pub trait Stream {
    fn eval(&self, env: Tracked<Env>, source: Tracked<Source>, time: u64) -> Option<i64>;
}

impl Stream for Call {
    #[comemo::memoize]
    fn eval(&self, env: Tracked<Env>, source: Tracked<Source>, time: u64) -> Option<i64> {
        // TODO: arguments
        // TODO: look for binding in context too
        // TODO: built ins
        let ident = self.identifier()?;
        let mut params = self.all_expression();
        let name = ident.text();
        if let Some(binding) = resolve(name, source) {
            let expr = binding.all_expression().next()?;
            expr.eval(env, source, time)
        } else if let Some(expr) = env.resolve(name) {
            expr.eval(env, source, time)
        } else {
            match name {
                "sin" => Some((((time as f64) / 1000.0f64).sin() * (i64::MAX as f64)) as i64), // TODO: use the argument as freq
                "loop" => {
                    let param = params.next();
                    if param.is_none() {
                        println!("Loop takes one parameter");
                        return None;
                    }
                    let param = param.unwrap();
                    if params.next().is_some() {
                        println!("Loop takes only one paramter, the second one will be ignored");
                    }
                    let one_duration = duration(time, param.clone(), env, source);
                    let time = if one_duration == 0 {
                        0
                    } else {
                        time % one_duration
                    };
                    param.eval(env, source, time)
                }
                _ => {
                    println!("Unknown function: {}", name);
                    None
                }
            }
        }
    }
}

impl Stream for Expression {
    #[comemo::memoize]

    fn eval(&self, env: Tracked<Env>, source: Tracked<Source>, time: u64) -> Option<i64> {
        match self.kind()? {
            ExprKind::Mul(m) => m.eval(env, source, time),
            ExprKind::Sum(s) => s.eval(env, source, time),
            ExprKind::Call(c) => c.eval(env, source, time),
            ExprKind::Constant(c) => c.eval(env, source, time),
            ExprKind::Sequence(s) => s.eval(env, source, time),
        }
    }
}

impl Stream for Mul {
    #[comemo::memoize]
    fn eval(&self, env: Tracked<Env>, source: Tracked<Source>, time: u64) -> Option<i64> {
        let mut exprs = self.all_expression();
        let left = exprs.next()?;
        let right = exprs.next()?;
        let eval_left = left.eval(env, source, time);
        let eval_right = right.eval(env, source, time);
        match (eval_left, eval_right) {
            (Some(l), Some(r)) => Some(l * r),
            (Some(_), None) | (None, Some(_)) => Some(0),
            (None, None) => {
                println!("End of product");
                None
            }
        }
    }
}

impl Stream for Sum {
    #[comemo::memoize]
    fn eval(&self, env: Tracked<Env>, source: Tracked<Source>, time: u64) -> Option<i64> {
        let mut exprs = self.all_expression();
        let left = exprs.next()?;
        let right = exprs.next()?;
        let eval_left = left.eval(env, source, time);
        let eval_right = right.eval(env, source, time);
        match (eval_left, eval_right) {
            (Some(l), Some(r)) => Some(l + r),
            (Some(x), None) | (None, Some(x)) => Some(x),
            (None, None) => {
                println!("End of sum");
                None
            }
        }
    }
}

impl Stream for Constant {
    #[comemo::memoize]
    fn eval(&self, _env: Tracked<Env>, _source: Tracked<Source>, _time: u64) -> Option<i64> {
        // TODO: units and all the stuff
        Some((self.all_number().next()?.text().parse::<f64>().ok()? * (i64::MAX as f64)) as i64)
    }
}

impl Stream for Sequence {
    #[comemo::memoize]
    fn eval(&self, env: Tracked<Env>, source: Tracked<Source>, mut time: u64) -> Option<i64> {
        for expr in self.all_expression() {
            dbg!(&expr);
            let dur = duration(time, expr.clone(), env, source);

            dbg!(time, dur);
            if time <= dur {
                return expr.eval(env, source, time);
            } else {
                time -= dur;
            }
        }
        None
    }
}

fn duration(mut max: u64, expr: Expression, env: Tracked<Env>, source: Tracked<Source>) -> u64 {
    let mut min = 0u64;
    loop {
        let half = min + ((max - min) / 2);
        if expr.eval(env, source, half).is_none() {
            min = half;
        } else {
            max = half;
        }

        if min.abs_diff(max) <= 1 {
            return max;
        }
    }
}
