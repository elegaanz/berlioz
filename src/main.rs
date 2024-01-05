use comemo::Track;

mod ast;
mod lexer;
mod parser;
mod resolve;

fn main() {
    let source = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let source = resolve::Source::new(source);
    let main = resolve::resolve("main", source.track());
    match main {
        Some(main) => {
            dbg!(&main);
            dbg!(main.all_expression().next());
        }
        None => println!("Error: please provide a main node"),
    }
}
