use std::ops::Range;

use enum_ordinalize::Ordinalize;
use rowan_nom::{
    alt, expect, fallible, fold_many1, join, many0, node, opt, peek, t, t_any, t_raw, DummyError,
    RowanNomError,
};

use crate::lexer::SyntaxKind::{self, *};

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct BerliozLang;

pub type SyntaxNode = rowan::SyntaxNode<BerliozLang>;
pub type SyntaxToken = rowan::SyntaxToken<BerliozLang>;

impl rowan::Language for BerliozLang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from_ordinal(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

impl rowan_nom::RowanNomLanguage for BerliozLang {
    fn get_error_kind() -> Self::Kind {
        SyntaxKind::Error
    }

    fn is_trivia(kind: Self::Kind) -> bool {
        matches!(kind, SyntaxKind::Space | SyntaxKind::LineBreak)
    }
}

#[derive(Debug)]
pub struct ParserError {
    pub span: Range<usize>,
    pub msg: String,
    pub cause: Option<Box<ParserError>>,
}

impl RowanNomError<BerliozLang> for ParserError {
    fn from_message(message: &str) -> Self {
        ParserError {
            span: 0..0,
            msg: message.to_string(),
            cause: None,
        }
    }

    fn from_expected(position: usize, message: &str) -> Self {
        Self {
            span: position..position,
            msg: message.to_string(),
            cause: None,
        }
    }

    fn from_expected_eof(range: Range<usize>) -> Self {
        Self {
            span: range,
            msg: "expected eof, found token".to_string(),
            cause: None,
        }
    }

    fn from_unexpected_eof(position: usize) -> Self {
        Self {
            span: position..position,
            msg: "unexpected eof".to_string(),
            cause: None,
        }
    }

    fn from_unexpected_token(span: Range<usize>, expected: SyntaxKind, found: SyntaxKind) -> Self {
        Self {
            span,
            msg: format!("expected {expected:?}, found {found:?}"),
            cause: None,
        }
    }

    fn with_context(self, ctx: &'static str) -> Self {
        Self {
            span: self.span.clone(),
            msg: ctx.to_string(),
            cause: Some(Box::new(self)),
        }
    }
}

type Children = rowan_nom::Children<BerliozLang, ParserError>;
type Input<'slice, 'src> = rowan_nom::Input<'slice, 'src, BerliozLang>;
type IResult<'slice, 'src, E = ParserError> =
    rowan_nom::IResult<'slice, 'src, BerliozLang, ParserError, E>;

pub fn many_delimited<'slice, 'src: 'slice, IE: RowanNomError<BerliozLang>>(
    mut left: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
    mut repeat: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
    mut separator: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
    mut right: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
) -> impl FnMut(Input<'slice, 'src>) -> IResult<'slice, 'src, IE> {
    use std::ops::ControlFlow;

    fn preceded_with_junk<'slice, 'src: 'slice, IE: RowanNomError<BerliozLang>>(
        mut parser: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
        mut right: impl nom::Parser<Input<'slice, 'src>, Children, IE>,
    ) -> impl FnMut(
        Input<'slice, 'src>,
    ) -> nom::IResult<Input<'slice, 'src>, ControlFlow<Children, Children>, IE> {
        move |mut input| {
            let mut children = Children::empty();

            loop {
                if let Ok((input, new_children)) = right.parse(input.clone()) {
                    break Ok((input, ControlFlow::Break(children + new_children)));
                } else if let Ok((input, new_children)) = parser.parse(input.clone()) {
                    break Ok((input, ControlFlow::Continue(children + new_children)));
                } else if let Ok((new_input, new_children)) =
                    t_any::<_, _, DummyError>(input.clone())
                {
                    input = new_input;
                    // TODO: more specific "UnexpectedToken" node below ?
                    children += new_children.into_node(Error);

                    let err = ParserError::from_message("many_preceded is skipping");
                    children += Children::from_err(err);
                } else {
                    // TODO: maybe don't error, but consider eof as a RIGHT equivalent + silent error
                    break Err(nom::Err::Error(IE::from_unexpected_eof(input.src_pos())));
                }
            }
        }
    }

    macro_rules! preceded_with_junk {
        ($parser:expr, $input:expr, &mut $children:ident) => {
            match preceded_with_junk(|i| $parser.parse(i), |i| right.parse(i))($input)? {
                (input, ControlFlow::Break(new_children)) => {
                    return Ok((input, $children + new_children));
                }
                (input, ControlFlow::Continue(new_children)) => {
                    $children += new_children;
                    input
                }
            }
        };
    }

    move |input| {
        let (input, mut children) = left.parse(input)?;

        let mut input = preceded_with_junk!(repeat, input, &mut children);

        loop {
            input = preceded_with_junk!(separator, input, &mut children);
            input = preceded_with_junk!(repeat, input, &mut children);
        }
    }
}

macro_rules! parse_ops {
    ($operator:ident => $operator_node:ident) => {
        ::nom::Parser::map(t(SyntaxKind::$operator), |c| (c, SyntaxKind::$operator_node))
    };
    {$($operator:ident => $operator_node:ident,)*} => {
        alt((
            $(parse_ops!($operator => $operator_node),
            )*
        ))
    };
}

fn parse_expression_left<'slice, 'src: 'slice>(
    mut parse_operator: impl nom::Parser<Input<'slice, 'src>, (Children, SyntaxKind), ParserError>,
    mut next: impl nom::Parser<Input<'slice, 'src>, Children, ParserError> + Copy,
) -> impl FnMut(Input<'slice, 'src>) -> IResult<'slice, 'src> {
    move |input| {
        fold_many1(
            next,
            |input| {
                let (input, (a, n)) = parse_operator.parse(input)?;
                let (input, should_dedent) =
                    match t::<BerliozLang, ParserError, ParserError>(Indent)(input.clone()) {
                        Ok((input, _)) => (input, true),
                        Err(_) => (input, false),
                    };
                let (input, b) = next.parse(input)?;
                let input = if should_dedent {
                    expect(
                        t(Dedent),
                        "The block indentation is wrong after this expression",
                    )(input)?
                    .0
                } else {
                    input
                };
                Ok((input, (a + b, n)))
            },
            |a, (b, n)| (a + b).into_node(n).into_node(Expression),
        )(input)
    }
}

fn constant<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        Expression,
        node(
            Constant,
            alt((
                join((
                    t(Number),
                    opt(join((t(Slash), t(Number)))),
                    opt(t(Identifier)),
                )),
                t(Str),
            )),
        ),
    )(input)
}

fn call<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((
        node(
            Expression,
            node(
                Call,
                join((
                    t(Identifier),
                    fallible(alt((
                        many_delimited(t(LPar), expression, t(Comma), t(RPar)),
                        block,
                        t_raw(LineBreak),
                        alt((peek(t(RPar)), peek(t(Comma)))),
                    ))),
                )),
            ),
        ),
        terminal,
    ))(input)
}

fn terminal<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((join((t(LPar), expression, t(RPar))), constant))(input)
}

fn add<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_left(
        parse_ops! {
            Plus => Sum
        },
        call,
    )(input)
}

fn mul<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    parse_expression_left(
        parse_ops! {
            Star => Mul
        },
        add,
    )(input)
}

fn block<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    alt((
        node(
            Expression,
            node(
                Sequence,
                many_delimited(t(Indent), expression, t_raw(LineBreak), t(Dedent)),
            ),
        ),
        mul,
    ))(input)
}

fn expression<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    fallible(block)(input)
}

fn binding<'slice, 'src>(input: Input<'slice, 'src>) -> IResult<'slice, 'src> {
    node(
        Binding,
        join((
            t(Identifier),
            opt(many_delimited(t(LPar), t(Identifier), t(Comma), t(RPar))),
            t(Equal),
            expression,
        )),
    )(input)
}

pub fn root(input: Input<'_, '_>) -> (rowan::SyntaxNode<BerliozLang>, Vec<ParserError>) {
    let (_rest, out) =
        rowan_nom::root_node::<_, _, ParserError>(Root, many0(binding))(input).unwrap();
    out
}

#[cfg(test)]
mod tests {
    use logos::Logos;
    use nom::Parser;
    use rowan::SyntaxNode;
    use rowan_nom::{many0, t_any, Children, Input};

    use super::{BerliozLang, ParserError};

    fn parse<'slice, 'src: 'slice>(
        tokens: &'slice [(crate::lexer::SyntaxKind, &'src str)],
        parser: impl Parser<
            Input<'slice, 'src, BerliozLang>,
            Children<BerliozLang, ParserError>,
            ParserError,
        >,
    ) -> SyntaxNode<BerliozLang> {
        let input = rowan_nom::Input::from(&tokens[..]);
        let (rest, out) = rowan_nom::root_node::<_, _, ParserError>(
            crate::lexer::SyntaxKind::Root,
            parser,
        )(input)
        .unwrap();

        let rest = rowan_nom::root_node::<_, ParserError, ParserError>(
            crate::lexer::SyntaxKind::Root,
            many0(t_any),
        )(rest)
        .unwrap()
        .1;
        assert!(!rest.0.children().any(|_| true));
        assert_eq!(out.1.len(), 0);

        out.0
    }

    fn lex(src: &str) -> Vec<(crate::lexer::SyntaxKind, &str)> {
        let lexer = crate::lexer::SyntaxKind::lexer(&src);
        let tokens = lexer
            .spanned()
            .filter_map(|(r, s)| r.map(|r| (r, &src[s])).ok())
            .collect::<Vec<_>>();
        tokens
    }

    #[test]
    fn const_bind() {
        let src = "C1 = 32.70320Hz";
        parse(&lex(src)[..], many0(super::binding));
    }

    #[test]
    fn const_bind2() {
        let src = "C1 = 32.70320Hz\n";
        parse(&lex(src)[..], many0(super::binding));
    }

    #[test]
    fn const_bind3() {
        let src = "\nC1 = 32.70320Hz\n";
        dbg!(parse(&lex(src)[..], many0(super::binding)));
    }

    #[test]
    fn const_bind4() {
        let src = "\nC1 = 32.70320Hz\n";
        dbg!(parse(&lex(src)[..], super::binding));
    }

    #[test]
    fn constant() {
        let src = "32.70320Hz";
        dbg!(parse(&lex(src)[..], super::constant));
    }
}
