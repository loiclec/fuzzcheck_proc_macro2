#![feature(no_coverage)]
#![feature(type_alias_impl_trait)]

use std::fmt;
use std::fmt::{Debug, Display};
use std::hash::Hash;

pub use mutators::KEYWORDS;
mod conversion_proc_macro2;
mod mutators;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Spacing {
    Alone,
    Joint,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Punct {
    ch: char,
    spacing: Spacing,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenTree {
    Group(Group),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TokenStream {
    inner: Vec<TokenTree>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Group {
    delimiter: Delimiter,
    stream: TokenStream,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident {
    sym: u8,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Literal {
    repr: u8,
}

// #[cfg(test)]
// mod tests {
//     #[test]
//     fn test_roundtrip() {
//         let ts = quote::quote!(
//             _/
//         );
//         println!("{ts:?}");
//     }
// }
