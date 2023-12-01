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

    #[regex("\\d+(.\\d+)?")]
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
    Parameter,
    Sequence,
    Expression,
    Binding,
    Root,

    Error,
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
