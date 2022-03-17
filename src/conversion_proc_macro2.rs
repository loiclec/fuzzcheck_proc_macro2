use std::str::FromStr;

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

impl From<TokenStream> for proc_macro2::TokenStream {
    #[no_coverage]
    fn from(x: TokenStream) -> Self {
        let mut ts = proc_macro2::TokenStream::new();
        for t in x.inner {
            match t {
                TokenTree::Group(x) => {
                    ts.extend(std::iter::once(proc_macro2::TokenTree::Group(x.into())));
                }
                TokenTree::Ident(x) => {
                    ts.extend(std::iter::once(proc_macro2::TokenTree::Ident(x.into())));
                }
                TokenTree::Punct(x) => {
                    ts.extend(std::iter::once(proc_macro2::TokenTree::Punct(x.into())));
                }
                TokenTree::Literal(x) => {
                    ts.extend(std::iter::once(proc_macro2::TokenTree::Literal(x.into())));
                }
                TokenTree::Lifetime(x) => {
                    ts.extend([
                        proc_macro2::TokenTree::Punct(proc_macro2::Punct::new('\'', proc_macro2::Spacing::Joint)),
                        proc_macro2::TokenTree::Ident(x.into()),
                    ]);
                }
            }
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

impl From<proc_macro2::Delimiter> for Delimiter {
    #[no_coverage]
    fn from(x: proc_macro2::Delimiter) -> Self {
        match x {
            proc_macro2::Delimiter::Parenthesis => Delimiter::Parenthesis,
            proc_macro2::Delimiter::Brace => Delimiter::Brace,
            proc_macro2::Delimiter::Bracket => Delimiter::Bracket,
            proc_macro2::Delimiter::None => {
                unimplemented!("proc_macro2::Delimiter::None has no representation in fuzzcheck_proc_macro2")
            }
        }
    }
}
impl From<proc_macro2::Literal> for Literal {
    #[no_coverage]
    fn from(x: proc_macro2::Literal) -> Self {
        let repr = match x.to_string().as_str() {
            "1u8" => 0,
            "2" => 1,
            "0.1f32" => 2,
            "1.1" => 3,
            "\"s\"" => 4,
            "'c'" => 5,
            "b\"bs\"" => 6,
            _ => unreachable!(),
        };
        Literal { repr }
    }
}

impl From<proc_macro2::Group> for Group {
    #[no_coverage]
    fn from(x: proc_macro2::Group) -> Self {
        Group {
            delimiter: x.delimiter().into(),
            stream: x.stream().into(),
        }
    }
}

impl From<proc_macro2::TokenStream> for TokenStream {
    #[no_coverage]
    fn from(x: proc_macro2::TokenStream) -> Self {
        let mut ts = TokenStream { inner: vec![] };
        let mut iter = x.into_iter();
        while let Some(t) = iter.next() {
            match t {
                proc_macro2::TokenTree::Group(x) => ts.inner.push(TokenTree::Group(x.into())),
                proc_macro2::TokenTree::Ident(x) => ts.inner.push(TokenTree::Ident(x.into())),
                proc_macro2::TokenTree::Punct(x) => {
                    if x.as_char() == '\'' {
                        if let Some(proc_macro2::TokenTree::Ident(ident)) = iter.next() {
                            ts.inner.push(TokenTree::Lifetime(ident.into()));
                        } else {
                            panic!("A single quote character is not followed by an identifier");
                        }
                    } else {
                        ts.inner.push(TokenTree::Punct(Punct {
                            ch: x.as_char(),
                            spacing: x.spacing().into(),
                        }));
                    }
                }
                proc_macro2::TokenTree::Literal(x) => {
                    ts.inner.push(TokenTree::Literal(x.into()));
                }
            };
        }
        ts
    }
}

impl From<proc_macro2::Ident> for Ident {
    #[no_coverage]
    fn from(x: proc_macro2::Ident) -> Self {
        if let Some(idx) = crate::KEYWORDS.iter().position(|&sym| x == sym) {
            Ident { sym: idx as u8 }
        } else {
            unimplemented!("{x} has no representation in fuzzcheck_proc_macro2");
        }
    }
}

impl From<proc_macro2::Spacing> for Spacing {
    #[no_coverage]
    fn from(x: proc_macro2::Spacing) -> Self {
        match x {
            proc_macro2::Spacing::Alone => Spacing::Alone,
            proc_macro2::Spacing::Joint => Spacing::Joint,
        }
    }
}

impl FromStr for TokenStream {
    type Err = <proc_macro2::TokenStream as FromStr>::Err;

    #[no_coverage]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        proc_macro2::TokenStream::from_str(s).map(
            #[no_coverage]
            |x| x.into(),
        )
    }
}

impl Display for TokenStream {
    #[no_coverage]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ts: proc_macro2::TokenStream = self.clone().into();
        Display::fmt(&ts, f)
    }
}
