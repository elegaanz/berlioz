use comemo::Track;

use crate::stream::{Env, Stream};

mod ast;
mod lexer;
mod parser;
mod resolve;
mod stream;

fn main() {
    let source = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let time = std::env::args()
        .nth(2)
        .and_then(|x| x.parse().ok())
        .unwrap_or(0);
    let source = resolve::Source::new(source);
    let main = resolve::resolve("main", source.track());
    match main {
        Some(main) => {
            dbg!(&main);
            let expr = main.all_expression().next().unwrap();
            let env = Env::empty();
            dbg!(expr.eval(env.track(), source.track(), time));
        }
        None => println!("Error: please provide a main node"),
    }
}
