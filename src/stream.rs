use std::{
    collections::HashMap,
    hash::Hash,
    ops::{Add, Div, Mul as MulOp, Sub},
};

use comemo::{Tracked, TrackedMut};

use crate::{
    ast::*,
    resolve::{resolve, Source},
};

#[derive(Debug, Clone, Copy)]
pub struct Value(f64);

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_be_bytes().hash(state);
    }
}

impl Eq for Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_be_bytes() == other.0.to_be_bytes()
    }
}

macro_rules! impl_op {
    ($trait:ident, $f:ident, $op:tt) => {
        impl $trait<Value> for Value {
            type Output = Self;

            fn $f(self, rhs: Value) -> Self::Output {
                Value(self.0 $op rhs.0)
            }
        }
    };
}

impl_op!(Add, add, +);
impl_op!(Sub, sub, -);
impl_op!(MulOp, mul, *);
impl_op!(Div, div, /);

#[derive(Clone)]
pub struct Env {
    bindings: HashMap<String, Value>,
    rate: u64,
    tempo: u64,
}

impl Env {
    pub fn empty() -> Self {
        Self {
            bindings: HashMap::new(),
            rate: 48_000,
            tempo: 120,
        }
    }
}

#[comemo::track]
impl Env {
    fn resolve(&self, name: &str) -> Option<Value> {
        self.bindings.get(name).copied()
    }

    fn bind(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    fn state(&self) -> Vec<String> {
        self.bindings.keys().cloned().collect()
    }

    fn restore(&mut self, state: Vec<String>) {
        let keys: Vec<_> = self.bindings.keys().cloned().collect();
        for k in keys {
            if !state.contains(&k) {
                self.bindings.remove(&k);
            }
        }
    }

    fn rate(&self) -> u64 {
        self.rate
    }

    fn tempo(&self) -> u64 {
        self.tempo
    }
}

pub trait Stream {
    fn eval(&self, env: TrackedMut<Env>, source: Tracked<Source>, time: u64) -> Option<Value>;
}

impl Stream for Call {
    #[comemo::memoize]
    fn eval(&self, mut env: TrackedMut<Env>, source: Tracked<Source>, time: u64) -> Option<Value> {
        let ident = self.identifier().log_err("Function call has no name")?;
        let mut params = self.all_expression();
        let name = ident.text();
        if let Some(binding) = resolve(name, source) {
            let expr = binding
                .all_expression()
                .next()
                .log_err("No expression in binding")?;
            let prev_env = env.state();
            // TODO: check arity
            let eval_params: Vec<_> = params
                .map(|p| p.eval(TrackedMut::reborrow_mut(&mut env), source, time))
                .collect();
            for (arg, val) in binding
                .all_identifier()
                .skip(1)
                .zip(eval_params.into_iter())
            {
                env.bind(
                    arg.text().to_owned(),
                    val.log_err("Argument could not be evaluated")?,
                );
            }
            let res = expr
                .eval(TrackedMut::reborrow_mut(&mut env), source, time)
                .log_err(format!("Function call ({}) returned None", name));
            env.restore(prev_env);
            res
        } else if let Some(expr) = env.resolve(name) {
            Some(expr)
        } else {
            match name {
                "sin" => {
                    let freq = params.next();
                    if freq.is_none() {
                        println!("sin takes one paramter");
                        return None;
                    }
                    if params.next().is_some() {
                        println!("sin takes only one paramter, the second one will be ignored");
                    }
                    let freq = freq
                        .unwrap()
                        .eval(TrackedMut::reborrow_mut(&mut env), source, time)
                        .log_err("sin frequency is None")?
                        .0;
                    Some(Value(((time as f64) * freq / env.rate() as f64).sin()))
                }
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
                    let one_duration = duration(
                        time,
                        param.clone(),
                        TrackedMut::reborrow_mut(&mut env),
                        source,
                    );
                    let time = if one_duration == 0 {
                        0
                    } else if time > one_duration {
                        time % one_duration
                    } else {
                        time
                    };
                    param.eval(env, source, time)
                }
                "linear_adsr" => {
                    let one_sec = Value(env.rate() as f64);
                    let hundred_percent = Value(1.0);
                    let params = [
                        params
                            .next()
                            .log_err("linear_adsr: expected attack_duration")?
                            .eval(TrackedMut::reborrow_mut(&mut env), source, time)
                            .unwrap_or(one_sec),
                        params
                            .next()
                            .log_err("linear_adsr: expected attack_value")?
                            .eval(TrackedMut::reborrow_mut(&mut env), source, time)
                            .unwrap_or(hundred_percent),
                        params
                            .next()
                            .log_err("linear_adsr: expected delay_duration")?
                            .eval(TrackedMut::reborrow_mut(&mut env), source, time)
                            .unwrap_or(one_sec),
                        params
                            .next()
                            .log_err("linear_adsr: expected delay_value")?
                            .eval(TrackedMut::reborrow_mut(&mut env), source, time)
                            .unwrap_or(hundred_percent),
                        params
                            .next()
                            .log_err("linear_adsr: expected sustain_duration")?
                            .eval(TrackedMut::reborrow_mut(&mut env), source, time)
                            .unwrap_or(one_sec),
                        params
                            .next()
                            .log_err("linear_adsr: expected sustain_value")?
                            .eval(TrackedMut::reborrow_mut(&mut env), source, time)
                            .unwrap_or(hundred_percent),
                        params
                            .next()
                            .log_err("linear_adsr: expected release_duration")?
                            .eval(TrackedMut::reborrow_mut(&mut env), source, time)
                            .unwrap_or(one_sec),
                    ];
                    let durations = &[params[0], params[2], params[4], params[6]];
                    let points = &[Value(0.0), params[1], params[3], params[5], Value(0.0)];
                    let mut progress = Value(time as f64);
                    for (duration, segment) in durations.iter().zip(points.windows(2)) {
                        if progress.0 < duration.0 {
                            return Some(
                                segment[0] + (segment[1] - segment[0]) * progress / *duration,
                            );
                        } else {
                            progress = progress - *duration;
                        }
                    }
                    None
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
    fn eval(&self, env: TrackedMut<Env>, source: Tracked<Source>, time: u64) -> Option<Value> {
        match self.kind()? {
            ExprKind::Mul(m) => m.eval(env, source, time).log_err("err: expr: mul"),
            ExprKind::Sum(s) => s.eval(env, source, time).log_err("err: expr: sum"),
            ExprKind::Call(c) => c.eval(env, source, time).log_err("err: expr: call"),
            ExprKind::Constant(c) => c.eval(env, source, time).log_err("err: expr: constant"),
            ExprKind::Sequence(s) => s.eval(env, source, time).log_err("err: expr: seq"),
        }
    }
}

impl Stream for Mul {
    #[comemo::memoize]
    fn eval(&self, mut env: TrackedMut<Env>, source: Tracked<Source>, time: u64) -> Option<Value> {
        let mut exprs = self.all_expression();
        let left = exprs.next()?;
        let right = exprs.next()?;
        let eval_left = left.eval(TrackedMut::reborrow_mut(&mut env), source, time);
        let eval_right = right.eval(env, source, time);
        match (eval_left, eval_right) {
            (Some(l), Some(r)) => Some(l * r),
            (Some(_), None) | (None, Some(_)) => Some(Value(0.0)),
            (None, None) => {
                println!("End of product");
                None
            }
        }
    }
}

impl Stream for Sum {
    #[comemo::memoize]
    fn eval(&self, mut env: TrackedMut<Env>, source: Tracked<Source>, time: u64) -> Option<Value> {
        let mut exprs = self.all_expression();
        let left = exprs.next()?;
        let right = exprs.next()?;
        let eval_left = left.eval(TrackedMut::reborrow_mut(&mut env), source, time);
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

trait LogErr {
    fn log_err<D: std::fmt::Display>(self, err: D) -> Self;
}

impl<T> LogErr for Option<T> {
    fn log_err<D: std::fmt::Display>(self, err: D) -> Self {
        if self.is_none() {
            println!("{}", err);
        }
        self
    }
}

impl Stream for Constant {
    #[comemo::memoize]
    fn eval(&self, env: TrackedMut<Env>, _source: Tracked<Source>, _time: u64) -> Option<Value> {
        let mut numbers = self.all_number();
        let num = numbers
            .next()
            .log_err("Expected a number")?
            .text()
            .parse::<f64>()
            .ok()
            .log_err("Failed to parse numerator")?;
        let denom = numbers
            .next()
            .map(|x| x.text().to_owned())
            .unwrap_or("1".to_owned())
            .parse::<f64>()
            .ok()
            .log_err("Failed to parse denominator")?;
        let unit = match self.unit().map(|u| u.text().to_owned()).as_deref() {
            Some("Hz") => 1f64 / env.rate() as f64,
            Some("%") => 0.01f64,
            Some("b") => env.rate() as f64 * 60f64 / env.tempo() as f64,
            Some("s") => env.rate() as f64,
            _ => 1f64,
        };
        Some(Value(num / denom * unit))
    }
}

impl Stream for Sequence {
    #[comemo::memoize]
    fn eval(
        &self,
        mut env: TrackedMut<Env>,
        source: Tracked<Source>,
        mut time: u64,
    ) -> Option<Value> {
        for expr in self.all_expression() {
            let dur = duration(
                time,
                expr.clone(),
                TrackedMut::reborrow_mut(&mut env),
                source,
            );

            if time <= dur {
                return expr.eval(env, source, time).log_err("Sequence is done");
            } else {
                time -= dur;
            }
        }
        None
    }
}

fn duration(
    mut max: u64,
    expr: Expression,
    mut env: TrackedMut<Env>,
    source: Tracked<Source>,
) -> u64 {
    let mut min = 0u64;
    loop {
        let half = min + ((max - min) / 2);
        if expr
            .eval(TrackedMut::reborrow_mut(&mut env), source, half)
            .is_none()
        {
            max = half;
        } else {
            min = half;
        }

        if min.abs_diff(max) <= 1 {
            return max;
        }
    }
}
