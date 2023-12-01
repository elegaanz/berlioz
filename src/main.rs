use logos::Logos;

mod ast;
mod lexer;
mod parser;

#[derive(Debug)]
enum Indent {
    Tab,
    Spaces(u8),
}

impl Indent {
    fn is_empty(&self) -> bool {
        matches!(self, Indent::Spaces(0))
    }
}

fn indent_to_str(i: &Indent) -> String {
    match i {
        Indent::Tab => "\t".to_owned(),
        Indent::Spaces(n) => " ".repeat(*n as usize),
    }
}

fn indents_to_str(i: &[Indent]) -> String {
    i.iter().map(indent_to_str).collect::<Vec<_>>().join("")
}

fn parse_indent(s: &str) -> Result<Indent, ()> {
    match s {
        "" | "\n" => Ok(Indent::Spaces(0)),
        " " => Ok(Indent::Spaces(1)),
        "  " => Ok(Indent::Spaces(2)),
        "   " => Ok(Indent::Spaces(3)),
        "    " => Ok(Indent::Spaces(4)),
        "        " => Ok(Indent::Spaces(8)),
        "\t" => Ok(Indent::Tab),
        _ => Err(()),
    }
}

struct IndentState {
    at_line_start: bool,
    current_level: Vec<Indent>,
}

fn main() {
    let source = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let tokens = lexer::SyntaxKind::lexer(&source)
        .spanned()
        .filter_map(|(token, span)| match token {
            Ok(token) => Some((token, &source[span])),
            Err(_) => {
                println!(
                    "The lexer didn't understand this token, it was ignored: {}",
                    &source[span]
                );
                None
            }
        })
        .chain(std::iter::once((lexer::SyntaxKind::Eof, "")))
        .scan(
            IndentState {
                at_line_start: true,
                current_level: Vec::new(),
            },
            |state, (tok, src)| {
                let mut res = None;
                if state.at_line_start {
                    let mut res_vec = Vec::new();
                    let mut prev_level = indents_to_str(&state.current_level);
                    if matches!(tok, lexer::SyntaxKind::Space | lexer::SyntaxKind::LineBreak) {
                        if src.len() > prev_level.len() {
                            let new = &src[prev_level.len()..];
                            match parse_indent(new) {
                                Ok(i) => {
                                    if !i.is_empty() {
                                        state.current_level.push(i);
                                        res_vec.push((lexer::SyntaxKind::Indent, ""));
                                    }
                                }
                                Err(_) => return Some(Vec::new()), // TODO: fail with "incoherent indent" error
                            }
                        }

                        while src.len() < prev_level.len() {
                            res_vec.push((lexer::SyntaxKind::Dedent, ""));
                            state.current_level.pop();
                            prev_level = indents_to_str(&state.current_level);
                        }

                        res_vec.push((tok, src));
                        res = Some(res_vec)
                    }
                }

                if tok == lexer::SyntaxKind::Eof {
                    let mut res_vec = vec![];
                    while !state.current_level.is_empty() {
                        res_vec.push((lexer::SyntaxKind::Dedent, ""));
                        state.current_level.pop();
                    }
                    res_vec.push((tok, src));
                    res = Some(res_vec);
                }

                state.at_line_start = tok == lexer::SyntaxKind::LineBreak;

                res.or(Some(vec![(tok, src)]))
            },
        )
        .flatten()
        .collect::<Vec<_>>();
    let input = rowan_nom::Input::from(&tokens[..]);
    let (tree, errors) = parser::root(input);
    dbg!(errors);
    dbg!(tree);
}
