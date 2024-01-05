#[allow(warnings, unused)]
mod generated {
    include!(concat!(env!("OUT_DIR"), "/ast_generated.rs"));
}

#[doc(inline)]
pub use generated::*;

use crate::{
    lexer::SyntaxKind,
    parser::{SyntaxNode, SyntaxToken},
};

pub trait AstNode {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;
    fn expect(syntax: SyntaxNode) -> Self
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
    fn clone_for_update(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_for_update()).unwrap()
    }
    fn clone_subtree(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_subtree()).unwrap()
    }
}

pub trait AstToken {
    fn can_cast(token: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn expect(syntax: SyntaxToken) -> Self
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &str {
        self.syntax().text()
    }
}

fn debug_ast_node_and_children(indent: usize, node: &SyntaxNode, name: &str) -> String {
    format!(
        "{}{}@{:?}\n{}",
        "  ".repeat(indent),
        name,
        node.text_range(),
        node.children()
            .map(|ch| debug_ast_node_and_children(indent + 1, &ch, &format!("{:?}", ch.kind())))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

fn debug_ast_node<N: AstNode>(
    node: &N,
    f: &mut std::fmt::Formatter<'_>,
    name: &str,
) -> std::fmt::Result {
    // write!(f, "{}@{:?}", name, node.syntax().text_range())
    write!(f, "{}", debug_ast_node_and_children(0, node.syntax(), name))
}
