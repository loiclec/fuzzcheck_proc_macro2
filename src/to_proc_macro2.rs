use crate::*;

impl From<Delimiter> for proc_macro2::Delimiter {
    fn from(x: Delimiter) -> Self {
        match x {
            Delimiter::Parenthesis => proc_macro2::Delimiter::Parenthesis,
            Delimiter::Brace => proc_macro2::Delimiter::Brace,
            Delimiter::Bracket => proc_macro2::Delimiter::Bracket,
            Delimiter::None(_) => proc_macro2::Delimiter::None,
        }
    }
}
impl From<Literal> for proc_macro2::Literal {
    fn from(x: Literal) -> Self {
        match x.repr.as_bytes() {
            b"1_u8" => proc_macro2::Literal::u8_suffixed(1),
            b"2" => proc_macro2::Literal::u8_unsuffixed(2),
            b"0.1_f32" => proc_macro2::Literal::f32_suffixed(0.1),
            b"1.1" => proc_macro2::Literal::f32_unsuffixed(1.1),
            b"\"s\"" => proc_macro2::Literal::string("s"),
            b"r#\"raw\"#" => proc_macro2::Literal::string("raw"),
            b"b'a'" => proc_macro2::Literal::character('c'),
            b"b\"bs\"" => proc_macro2::Literal::byte_string(b"bs"),
            _ => unreachable!(),
        }
    }
}

impl From<Group> for proc_macro2::Group {
    fn from(x: Group) -> Self {
        proc_macro2::Group::new(x.delimiter.into(), x.stream.into())
    }
}

impl From<TokenTree> for proc_macro2::TokenTree {
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
    fn from(x: TokenStream) -> Self {
        let mut ts = proc_macro2::TokenStream::new();
        for t in x {
            ts.extend(std::iter::once(Into::<proc_macro2::TokenTree>::into(t)));
        }
        ts
    }
}

impl From<Ident> for proc_macro2::Ident {
    fn from(x: Ident) -> Self {
        // if x.raw {
        //     proc_macro2::Ident::new_raw(&x.sym, Span.into())
        // } else {
        proc_macro2::Ident::new(&x.sym, Span.into())
        // }
    }
}

impl From<Span> for proc_macro2::Span {
    fn from(_x: Span) -> Self {
        proc_macro2::Span::call_site()
    }
}

impl From<Punct> for proc_macro2::Punct {
    fn from(x: Punct) -> Self {
        proc_macro2::Punct::new(x.ch, x.spacing.into())
    }
}

impl From<Spacing> for proc_macro2::Spacing {
    fn from(x: Spacing) -> Self {
        match x {
            Spacing::Alone => proc_macro2::Spacing::Alone,
            Spacing::Joint => proc_macro2::Spacing::Joint,
        }
    }
}
