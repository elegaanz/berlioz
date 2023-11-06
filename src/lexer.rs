use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq)]
#[repr(u16)]
pub enum Token {
    #[regex("[ \t\r\u{0C}]")]
    Space,

    #[regex("\n")]
    LineBreak,

    #[token("=")]
    Equal,

    #[token("*")]
    Star,

    #[token("+")]
    Plus,

    #[token(";")]
    Semicolon,

    #[regex("\\d+(/\\d+)?[a-zA-Z]*")]
    Number,

    #[regex("[a-zA-Z_]+")]
    Identifier,

    #[regex("'([^']|\\\\')*'")]
    Str,
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use super::Token::{self, *};

    #[test]
    fn lex() {
        let lexer = Token::lexer("bass note = sin note * linear_adsr 1s 100% 1s 90% 2s 90% 1s");
        assert_eq!(
            lexer.filter_map(Result::ok).collect::<Vec<_>>(),
            vec![
                Identifier, Space, Identifier, Space, Equal, Space, Identifier, Space, Identifier,
                Space, Star, Space, Identifier, Space, Number, Space, Number, Space, Number, Space,
                Number, Space, Number, Space, Number, Space, Number
            ]
        )
    }
}
