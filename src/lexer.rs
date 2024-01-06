use enum_ordinalize::Ordinalize;
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy, Ordinalize)]
#[repr(u16)]
pub enum SyntaxKind {
    #[regex("[ \t\r\u{0C}]+")]
    Space,

    #[regex("\n")]
    LineBreak,

    #[token("(")]
    LPar,

    #[token(")")]
    RPar,

    #[token(",")]
    Comma,

    #[token("=")]
    Equal,

    #[token("*")]
    Star,

    #[token("+")]
    Plus,

    #[token(";")]
    Semicolon,

    #[regex("\\d+(\\.\\d+)?")]
    Number,

    #[token("/")]
    Slash,

    #[regex("(%|[a-zA-Z_][a-zA-Z0-9_]*)")]
    Identifier,

    #[regex("'([^']|\\\\')*'")]
    Str,

    Eof,

    Dedent,
    Indent,

    Constant,
    Sum,
    Mul,
    Call,
    Sequence,
    Expression,
    Binding,
    Root,

    Error,
}

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

pub fn lex(source: &str) -> Vec<(SyntaxKind, &str)> {
    SyntaxKind::lexer(&source)
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
        .chain(std::iter::once((SyntaxKind::Eof, "")))
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
                    if matches!(tok, SyntaxKind::Space | SyntaxKind::LineBreak) {
                        if src.len() > prev_level.len() {
                            let new = &src[prev_level.len()..];
                            match parse_indent(new) {
                                Ok(i) => {
                                    if !i.is_empty() {
                                        state.current_level.push(i);
                                        res_vec.push((SyntaxKind::Indent, ""));
                                    }
                                }
                                Err(_) => return Some(Vec::new()), // TODO: fail with "incoherent indent" error
                            }
                        }

                        while src.len() < prev_level.len() {
                            res_vec.push((SyntaxKind::Dedent, ""));
                            state.current_level.pop();
                            prev_level = indents_to_str(&state.current_level);
                        }

                        res_vec.push((tok, src));
                        res = Some(res_vec)
                    }
                }

                if tok == SyntaxKind::Eof {
                    let mut res_vec = vec![];
                    while !state.current_level.is_empty() {
                        res_vec.push((SyntaxKind::Dedent, ""));
                        state.current_level.pop();
                    }
                    res_vec.push((tok, src));
                    res = Some(res_vec);
                }

                state.at_line_start = tok == SyntaxKind::LineBreak;

                res.or(Some(vec![(tok, src)]))
            },
        )
        .flatten()
        .collect::<Vec<_>>()
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use super::SyntaxKind::{self, *};

    #[test]
    fn lex() {
        let lexer =
            SyntaxKind::lexer("bass note = sin note * linear_adsr 1s 100% 1s 90% 2s 90% 1s");
        assert_eq!(
            lexer.filter_map(Result::ok).collect::<Vec<_>>(),
            vec![
                Identifier, Space, Identifier, Space, Equal, Space, Identifier, Space, Identifier,
                Space, Star, Space, Identifier, Space, Number, Identifier, Space, Number,
                Identifier, Space, Number, Identifier, Space, Number, Identifier, Space, Number,
                Identifier, Space, Number, Identifier, Space, Number, Identifier,
            ]
        )
    }
}
