use comemo::{memoize, track, Tracked};

pub struct Source {
    src: String,
    root: crate::ast::Root,
}

#[memoize]
pub fn resolve(name: &str, source: Tracked<Source>) -> Option<crate::ast::Binding> {
    source.root().all_binding().find(|b| {
        b.all_identifier()
            .next()
            .map(|id| id.syntax.text() == name)
            .unwrap_or(false)
    })
}

impl Source {
    pub fn new(src: String) -> Self {
        let tokens = crate::lexer::lex(&src);
        let input = rowan_nom::Input::from(&tokens[..]);
        let (root, _errors) = crate::parser::root(input);

        dbg!(&root);

        Self {
            src,
            root: crate::ast::Root { syntax: root },
        }
    }
}

#[track]
impl Source {
    fn read(&self) -> &str {
        &self.src
    }

    fn root(&self) -> &crate::ast::Root {
        &self.root
    }
}
