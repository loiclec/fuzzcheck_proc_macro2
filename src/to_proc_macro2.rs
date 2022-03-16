use crate::*;

impl From<Delimiter> for proc_macro2::Delimiter {
    #[no_coverage]
    fn from(x: Delimiter) -> Self {
        match x {
            Delimiter::Parenthesis => proc_macro2::Delimiter::Parenthesis,
            Delimiter::Brace => proc_macro2::Delimiter::Brace,
            Delimiter::Bracket => proc_macro2::Delimiter::Bracket,
        }
    }
}
impl From<Literal> for proc_macro2::Literal {
    #[no_coverage]
    fn from(x: Literal) -> Self {
        match x.repr {
            0 => proc_macro2::Literal::u8_suffixed(1),
            1 => proc_macro2::Literal::u8_unsuffixed(2),
            2 => proc_macro2::Literal::f32_suffixed(0.1),
            3 => proc_macro2::Literal::f32_unsuffixed(1.1),
            4 => proc_macro2::Literal::string("s"),
            5 => proc_macro2::Literal::character('c'),
            6 => proc_macro2::Literal::byte_string(b"bs"),
            _ => unreachable!(),
        }
    }
}

impl From<Group> for proc_macro2::Group {
    #[no_coverage]
    fn from(x: Group) -> Self {
        proc_macro2::Group::new(x.delimiter.into(), x.stream.into())
    }
}

impl From<TokenTree> for proc_macro2::TokenTree {
    #[no_coverage]
    fn from(x: TokenTree) -> Self {
        match x {
            TokenTree::Group(x) => proc_macro2::TokenTree::Group(x.into()),
            TokenTree::Ident(x) => proc_macro2::TokenTree::Ident(x.into()),
            TokenTree::Punct(x) => proc_macro2::TokenTree::Punct(x.into()),
            TokenTree::Literal(x) => proc_macro2::TokenTree::Literal(x.into()),
        }
    }
}

impl From<TokenStream> for proc_macro2::TokenStream {
    #[no_coverage]
    fn from(x: TokenStream) -> Self {
        let mut ts = proc_macro2::TokenStream::new();
        for t in x.inner {
            ts.extend(std::iter::once(Into::<proc_macro2::TokenTree>::into(t)));
        }
        ts
    }
}

impl From<Ident> for proc_macro2::Ident {
    #[no_coverage]
    fn from(x: Ident) -> Self {
        proc_macro2::Ident::new(crate::KEYWORDS[x.sym as usize], proc_macro2::Span::call_site())
    }
}

impl From<Punct> for proc_macro2::Punct {
    #[no_coverage]
    fn from(x: Punct) -> Self {
        proc_macro2::Punct::new(x.ch, x.spacing.into())
    }
}

impl From<Spacing> for proc_macro2::Spacing {
    #[no_coverage]
    fn from(x: Spacing) -> Self {
        match x {
            Spacing::Alone => proc_macro2::Spacing::Alone,
            Spacing::Joint => proc_macro2::Spacing::Joint,
        }
    }
}
