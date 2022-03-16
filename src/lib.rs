#![feature(no_coverage)]
#![feature(type_alias_impl_trait)]

use std::cmp::Ordering;
use std::fmt;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::ops::RangeBounds;
use std::str::FromStr;

use parse::Cursor;
use serde::{Deserialize, Serialize};
use unicode_xid::UnicodeXID;

mod mutators;
mod parse;
mod to_proc_macro2;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
    // None(()),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Spacing {
    Alone,
    Joint,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span;

// impl Span {
//     #[no_coverage]
//     pub fn call_site() -> Span {
//         Span
//     }
// }

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Punct {
    ch: char,
    spacing: Spacing,
    // span: Span,
}

// impl Punct {
//     /// Creates a new `Punct` from the given character and spacing.
//     ///
//     /// The `ch` argument must be a valid punctuation character permitted by the
//     /// language, otherwise the function will panic.
//     ///
//     /// The returned `Punct` will have the default span of `Span::call_site()`
//     /// which can be further configured with the `set_span` method below.
//     #[no_coverage]
//     pub fn new(ch: char, spacing: Spacing) -> Self {
//         Punct { ch, spacing }
//     }

//     /// Returns the value of this punctuation character as `char`.
//     #[no_coverage]
//     pub fn as_char(&self) -> char {
//         self.ch
//     }

//     /// Returns the spacing of this punctuation character, indicating whether
//     /// it's immediately followed by another `Punct` in the token stream, so
//     /// they can potentially be combined into a multicharacter operator
//     /// (`Joint`), or it's followed by some other token or whitespace (`Alone`)
//     /// so the operator has certainly ended.
//     #[no_coverage]
//     pub fn spacing(&self) -> Spacing {
//         self.spacing
//     }

//     /// Returns the span for this punctuation character.
//     #[no_coverage]
//     pub fn span(&self) -> Span {
//         Span
//     }

//     /// Configure the span for this punctuation character.
//     #[no_coverage]
//     pub fn set_span(&mut self, _span: Span) {}
// }

/// Prints the punctuation character as a string that should be losslessly
/// convertible back into the same character.
impl Display for Punct {
    #[no_coverage]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.ch, f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TokenTree {
    Group(Group),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
}

impl Display for TokenTree {
    #[no_coverage]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Group(c) => Display::fmt(c, f),
            TokenTree::Ident(c) => Display::fmt(c, f),
            TokenTree::Punct(c) => Display::fmt(c, f),
            TokenTree::Literal(c) => Display::fmt(c, f),
        }
    }
}

impl From<Punct> for TokenTree {
    #[no_coverage]
    fn from(x: Punct) -> Self {
        Self::Punct(x)
    }
}

impl From<Ident> for TokenTree {
    #[no_coverage]
    fn from(x: Ident) -> Self {
        Self::Ident(x)
    }
}

impl From<Group> for TokenTree {
    #[no_coverage]
    fn from(x: Group) -> Self {
        Self::Group(x)
    }
}

impl From<Literal> for TokenTree {
    #[no_coverage]
    fn from(x: Literal) -> Self {
        Self::Literal(x)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TokenStream {
    inner: Vec<TokenTree>,
}
/// `TokenStream::default()` returns an empty stream,
/// i.e. this is equivalent with `TokenStream::new()`.
impl Default for TokenStream {
    #[no_coverage]
    fn default() -> Self {
        TokenStream::new()
    }
}

impl FromStr for TokenStream {
    type Err = LexError;
    #[no_coverage]
    fn from_str(src: &str) -> Result<TokenStream, LexError> {
        let cursor = get_cursor(src);
        crate::parse::token_stream(cursor)
    }
}

impl From<TokenTree> for TokenStream {
    #[no_coverage]
    fn from(token: TokenTree) -> Self {
        TokenStream { inner: vec![token] }
    }
}

impl FromIterator<TokenTree> for TokenStream {
    #[no_coverage]
    fn from_iter<I: IntoIterator<Item = TokenTree>>(tokens: I) -> Self {
        let mut stream = TokenStream::new();
        stream.extend(tokens);
        stream
    }
}

impl FromIterator<TokenStream> for TokenStream {
    #[no_coverage]
    fn from_iter<I: IntoIterator<Item = TokenStream>>(streams: I) -> Self {
        let mut v = Vec::new();

        for mut stream in streams {
            v.extend(stream.take_inner());
        }

        TokenStream { inner: v }
    }
}

impl Extend<TokenTree> for TokenStream {
    #[no_coverage]
    fn extend<I: IntoIterator<Item = TokenTree>>(&mut self, tokens: I) {
        tokens.into_iter().for_each(
            #[no_coverage]
            |token| self.push_token(token),
        );
    }
}

impl Extend<TokenStream> for TokenStream {
    #[no_coverage]
    fn extend<I: IntoIterator<Item = TokenStream>>(&mut self, streams: I) {
        self.inner.extend(streams.into_iter().flatten());
    }
}

impl Display for TokenStream {
    #[no_coverage]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut joint = false;
        for (i, tt) in self.inner.iter().enumerate() {
            if i != 0 && !joint {
                write!(f, " ")?;
            }
            joint = false;
            match tt {
                TokenTree::Group(tt) => Display::fmt(tt, f),
                TokenTree::Ident(tt) => Display::fmt(tt, f),
                TokenTree::Punct(tt) => {
                    joint = tt.spacing() == Spacing::Joint;
                    Display::fmt(tt, f)
                }
                TokenTree::Literal(tt) => Display::fmt(tt, f),
            }?;
        }

        Ok(())
    }
}

impl TokenStream {
    #[no_coverage]
    pub fn new() -> Self {
        TokenStream { inner: Vec::new() }
    }

    #[no_coverage]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    #[no_coverage]
    fn take_inner(&mut self) -> Vec<TokenTree> {
        std::mem::take(&mut self.inner)
    }

    #[no_coverage]
    fn push_token(&mut self, token: TokenTree) {
        // https://github.com/dtolnay/proc-macro2/issues/235
        match token {
            TokenTree::Literal(literal) if literal.repr.starts_with('-') => {
                Self::push_negative_literal(self, literal);
            }
            _ => self.inner.push(token),
        }
    }

    #[no_coverage]
    fn push_negative_literal(stream: &mut TokenStream, mut literal: Literal) {
        literal.repr.remove(0);
        let mut punct = Punct::new('-', Spacing::Alone);
        punct.set_span(Span);
        stream.inner.push(TokenTree::Punct(punct));
        stream.inner.push(TokenTree::Literal(literal));
    }
}

impl From<Vec<TokenTree>> for TokenStream {
    #[no_coverage]
    fn from(inner: Vec<TokenTree>) -> Self {
        TokenStream { inner }
    }
}

// Nonrecursive to prevent stack overflow.
impl Drop for TokenStream {
    #[no_coverage]
    fn drop(&mut self) {
        while let Some(token) = self.inner.pop() {
            let mut group = match token {
                TokenTree::Group(group) => group,
                _ => continue,
            };
            self.inner.extend(group.stream.take_inner());
        }
    }
}

pub(crate) type TokenTreeIter = std::vec::IntoIter<TokenTree>;

impl IntoIterator for TokenStream {
    type Item = TokenTree;
    type IntoIter = TokenTreeIter;

    #[no_coverage]
    fn into_iter(mut self) -> TokenTreeIter {
        self.take_inner().into_iter()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Group {
    delimiter: Delimiter,
    stream: TokenStream,
    // span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Ident {
    sym: String,
    raw: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Literal {
    repr: String,
    // span: Span,
}

macro_rules! numbers {
    ($($name_suffixed:ident, $name_unsuffixed:ident, $kind:ident,)*) => ($(
        #[no_coverage]
        pub fn $name_suffixed(n: $kind) -> Literal {
            Literal {
                repr: $name_suffixed(n)
            }
        }
        #[no_coverage]
        pub fn $name_unsuffixed(n: $kind) -> Literal {
            Literal {
                repr: $name_unsuffixed(n)
            }
        }
    )*)
}

impl Literal {
    numbers! {
        u8_suffixed, u8_unsuffixed, u8,
        u16_suffixed, u16_unsuffixed, u16,
        u32_suffixed, u32_unsuffixed, u32,
        u64_suffixed, u64_unsuffixed, u64,
        u128_suffixed, u128_unsuffixed, u128,
        usize_suffixed, usize_unsuffixed, usize,
        i8_suffixed, i8_unsuffixed, i8,
        i16_suffixed, i16_unsuffixed, i16,
        i32_suffixed, i32_unsuffixed, i32,
        i64_suffixed, i64_unsuffixed, i64,
        i128_suffixed, i128_unsuffixed, i128,
        isize_suffixed, isize_unsuffixed, isize,
    }

    #[no_coverage]
    pub fn f32_suffixed(f: f32) -> Literal {
        let repr = f32_suffixed(f);
        Literal { repr }
    }

    #[no_coverage]
    pub fn f64_suffixed(f: f64) -> Literal {
        let repr = f64_suffixed(f);
        Literal { repr }
    }

    #[no_coverage]
    pub fn f32_unsuffixed(f: f32) -> Literal {
        let repr = f32_unsuffixed(f);
        Literal { repr }
    }

    #[no_coverage]
    pub fn f64_unsuffixed(f: f64) -> Literal {
        let repr = f64_unsuffixed(f);
        Literal { repr }
    }

    #[no_coverage]
    pub fn string(t: &str) -> Literal {
        Literal { repr: string(t) }
    }

    #[no_coverage]
    pub fn character(t: char) -> Literal {
        Literal { repr: character(t) }
    }

    #[no_coverage]
    pub fn byte_string(bytes: &[u8]) -> Literal {
        Literal {
            repr: byte_string(bytes),
        }
    }

    #[no_coverage]
    pub fn span(&self) -> Span {
        Span {}
    }

    #[no_coverage]
    pub fn set_span(&mut self, _span: Span) {}

    #[no_coverage]
    pub fn subspan<R: RangeBounds<usize>>(&self, _range: R) -> Option<Span> {
        None
    }
}
fn get_cursor(src: &str) -> Cursor {
    Cursor { rest: src }
}
#[derive(Clone, Copy, Debug)]
pub struct LexError;

impl FromStr for Literal {
    type Err = LexError;
    #[no_coverage]
    fn from_str(mut repr: &str) -> Result<Self, Self::Err> {
        let negative = repr.starts_with('-');
        if negative {
            repr = &repr[1..];
            if !repr.starts_with(|ch: char| ch.is_ascii_digit()) {
                return Err(LexError);
            }
        }
        let cursor = get_cursor(repr);
        if let Ok((_rest, mut literal)) = parse::literal(cursor) {
            if literal.repr.len() == repr.len() {
                if negative {
                    literal.repr.insert(0, '-');
                }
                return Ok(literal);
            }
        }
        Err(LexError)
    }
}

impl Display for Literal {
    #[no_coverage]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.repr, f)
    }
}

impl Group {
    #[no_coverage]
    pub fn new(delimiter: Delimiter, stream: TokenStream) -> Self {
        Group { delimiter, stream }
    }

    #[no_coverage]
    pub fn delimiter(&self) -> Delimiter {
        self.delimiter
    }

    #[no_coverage]
    pub fn stream(&self) -> TokenStream {
        self.stream.clone()
    }

    #[no_coverage]
    pub fn span(&self) -> Span {
        Span
    }

    #[no_coverage]
    pub fn span_open(&self) -> Span {
        Span
    }

    #[no_coverage]
    pub fn span_close(&self) -> Span {
        Span
    }

    #[no_coverage]
    pub fn set_span(&mut self, _span: Span) {}
}

impl Display for Group {
    // We attempt to match libproc_macro's formatting.
    // Empty parens: ()
    // Nonempty parens: (...)
    // Empty brackets: []
    // Nonempty brackets: [...]
    // Empty braces: { }
    // Nonempty braces: { ... }
    #[no_coverage]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (open, close) = match self.delimiter {
            Delimiter::Parenthesis => ("(", ")"),
            Delimiter::Brace => ("{ ", "}"),
            Delimiter::Bracket => ("[", "]"),
            Delimiter::None(_) => ("", ""),
        };

        f.write_str(open)?;
        Display::fmt(&self.stream, f)?;
        if self.delimiter == Delimiter::Brace && !self.stream.inner.is_empty() {
            f.write_str(" ")?;
        }
        f.write_str(close)?;

        Ok(())
    }
}

impl Ident {
    #[no_coverage]
    fn _new(string: &str, raw: bool, _span: Span) -> Self {
        validate_ident(string);

        Ident {
            sym: string.to_owned(),
            raw,
        }
    }

    #[no_coverage]
    pub fn new(string: &str, span: Span) -> Self {
        Ident::_new(string, false, span)
    }

    #[no_coverage]
    pub fn new_raw(string: &str, span: Span) -> Self {
        Ident::_new(string, true, span)
    }

    #[no_coverage]
    pub fn span(&self) -> Span {
        Span
    }

    #[no_coverage]
    pub fn set_span(&mut self, _span: Span) {}
}

impl Eq for Ident {}

impl PartialEq for Ident {
    #[no_coverage]
    fn eq(&self, other: &Ident) -> bool {
        self.sym == other.sym && self.raw == other.raw
    }
}

impl<T> PartialEq<T> for Ident
where
    T: ?Sized + AsRef<str>,
{
    #[no_coverage]
    fn eq(&self, other: &T) -> bool {
        let other = other.as_ref();
        if self.raw {
            other.starts_with("r#") && self.sym == other[2..]
        } else {
            self.sym == other
        }
    }
}

impl PartialOrd for Ident {
    #[no_coverage]
    fn partial_cmp(&self, other: &Ident) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Ident {
    #[no_coverage]
    fn cmp(&self, other: &Ident) -> Ordering {
        self.to_string().cmp(&other.to_string())
    }
}

impl Hash for Ident {
    #[no_coverage]
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.to_string().hash(hasher);
    }
}

impl Display for Ident {
    #[no_coverage]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.raw {
            f.write_str("r#")?;
        }
        Display::fmt(&self.sym, f)
    }
}

macro_rules! suffixed_numbers_str {
    ($($name:ident => $kind:ident,)*) => ($(
        #[no_coverage]
        fn $name(n: $kind) -> String {
            format!(concat!("{}", stringify!($kind)), n)
        }
    )*)
}

macro_rules! unsuffixed_numbers_str {
    ($($name:ident => $kind:ident,)*) => ($(
        #[no_coverage]
        fn $name(n: $kind) -> String {
            n.to_string()
        }
    )*)
}

suffixed_numbers_str! {
    u8_suffixed => u8,
    u16_suffixed => u16,
    u32_suffixed => u32,
    u64_suffixed => u64,
    u128_suffixed => u128,
    usize_suffixed => usize,
    i8_suffixed => i8,
    i16_suffixed => i16,
    i32_suffixed => i32,
    i64_suffixed => i64,
    i128_suffixed => i128,
    isize_suffixed => isize,
}

unsuffixed_numbers_str! {
    u8_unsuffixed => u8,
    u16_unsuffixed => u16,
    u32_unsuffixed => u32,
    u64_unsuffixed => u64,
    u128_unsuffixed => u128,
    usize_unsuffixed => usize,
    i8_unsuffixed => i8,
    i16_unsuffixed => i16,
    i32_unsuffixed => i32,
    i64_unsuffixed => i64,
    i128_unsuffixed => i128,
    isize_unsuffixed => isize,
}
#[no_coverage]
fn f32_unsuffixed(f: f32) -> String {
    let mut s = f.to_string();
    if !s.contains('.') {
        s.push_str(".0");
    }
    s
}

#[no_coverage]
fn f64_unsuffixed(f: f64) -> String {
    let mut s = f.to_string();
    if !s.contains('.') {
        s.push_str(".0");
    }
    s
}
#[no_coverage]
fn f32_suffixed(f: f32) -> String {
    let mut s = f.to_string();
    if !s.contains('.') {
        s.push_str(".0");
    }
    s.push_str("_f32");
    s
}

#[no_coverage]
fn f64_suffixed(f: f64) -> String {
    let mut s = f.to_string();
    if !s.contains('.') {
        s.push_str(".0");
    }
    s.push_str("_f64");
    s
}

#[no_coverage]
fn string(t: &str) -> String {
    let mut repr = String::with_capacity(t.len() + 2);
    repr.push('"');
    for c in t.chars() {
        if c == '\'' {
            // escape_debug turns this into "\'" which is unnecessary.
            repr.push(c);
        } else {
            repr.extend(c.escape_debug());
        }
    }
    repr.push('"');
    repr
}

#[no_coverage]
fn character(t: char) -> String {
    let mut repr = String::new();
    repr.push('\'');
    if t == '"' {
        // escape_debug turns this into '\"' which is unnecessary.
        repr.push(t);
    } else {
        repr.extend(t.escape_debug());
    }
    repr.push('\'');
    repr
}

#[no_coverage]
fn byte_string(bytes: &[u8]) -> String {
    let mut escaped = "b\"".to_string();
    for b in bytes {
        #[allow(clippy::match_overlapping_arm)]
        match *b {
            b'\0' => escaped.push_str(r"\0"),
            b'\t' => escaped.push_str(r"\t"),
            b'\n' => escaped.push_str(r"\n"),
            b'\r' => escaped.push_str(r"\r"),
            b'"' => escaped.push_str("\\\""),
            b'\\' => escaped.push_str("\\\\"),
            b'\x20'..=b'\x7E' => escaped.push(*b as char),
            _ => escaped.push_str(&format!("\\x{:02X}", b)),
        }
    }
    escaped.push('"');
    escaped
}

#[allow(clippy::manual_range_contains)]
fn validate_ident(string: &str) {
    let validate = string;
    if validate.is_empty() {
        panic!("Ident is not allowed to be empty; use Option<Ident>");
    }

    if validate.bytes().all(|digit| digit >= b'0' && digit <= b'9') {
        panic!("Ident cannot be a number; use Literal instead");
    }

    #[no_coverage]
    fn ident_ok(string: &str) -> bool {
        let mut chars = string.chars();
        let first = chars.next().unwrap();
        if !is_ident_start(first) {
            return false;
        }
        for ch in chars {
            if !is_ident_continue(ch) {
                return false;
            }
        }
        true
    }

    if !ident_ok(validate) {
        panic!("{:?} is not a valid Ident", string);
    }
}

#[no_coverage]
#[allow(clippy::manual_range_contains)]
pub(crate) fn is_ident_start(c: char) -> bool {
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_' || (c > '\x7f' && UnicodeXID::is_xid_start(c))
}

#[no_coverage]
#[allow(clippy::manual_range_contains)]
pub(crate) fn is_ident_continue(c: char) -> bool {
    ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || c == '_'
        || ('0' <= c && c <= '9')
        || (c > '\x7f' && UnicodeXID::is_xid_continue(c))
}

#[cfg(test)]
mod tests {
    use fuzzcheck::mutators::testing_utilities::test_mutator;
    use fuzzcheck::DefaultMutator;

    use super::TokenStream;

    #[test]
    fn test_token_stream_mutator() {
        let m = TokenStream::default_mutator();
        test_mutator(m, 400., 400., false, true, 1000, 1000);
    }
}
