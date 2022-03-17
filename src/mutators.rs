use fuzzcheck::mutators::character_classes::CharacterMutator;
use fuzzcheck::mutators::integer_within_range::U8WithinRangeMutator;
use fuzzcheck::mutators::recursive::RecurToMutator;
use fuzzcheck::mutators::vector::VecMutator;
use fuzzcheck::{make_mutator, DefaultMutator};

use crate::{Delimiter, Group, Ident, Literal, Punct, Spacing, TokenStream, TokenTree};

make_mutator! {
    name: DelimiterMutator,
    default: true,
    type:
        pub enum Delimiter {
            Parenthesis,
            Brace,
            Bracket,
        }
}

make_mutator! {
    name: SpacingMutator,
    default: true,
    type:
        pub enum Spacing {
            Alone,
            Joint,
        }
}

make_mutator! {
    name: PunctMutator,
    default: true,
    type:
    pub struct Punct {
        // ~!@#$%^&*-=+|;:,<.>/?'
        // ~!@#$%^&*-=+|;:,<.>/?'
        #[field_mutator(CharacterMutator = { CharacterMutator::new(vec!['!' ..= '!', '#'..='&', '*' ..= '/', ':' ..= '@', '^' ..= '^', '|' ..= '|', '~' ..= '~']) })]
        ch: char,
        #[field_mutator(<Spacing as DefaultMutator>::Mutator = { Spacing::default_mutator() } )]
        spacing: Spacing,
    }
}

make_mutator! {
    name: TokenTreeMutator,
    default: true,
    type:
        pub enum TokenTree {
            Group(Group),
            Ident(#[field_mutator(<Ident as DefaultMutator>::Mutator = { Ident::default_mutator() } )] Ident),
            Punct(#[field_mutator(<Punct as DefaultMutator>::Mutator = { Punct::default_mutator() } )] Punct),
            Literal(#[field_mutator(<Literal as DefaultMutator>::Mutator = { Literal::default_mutator() } )] Literal),
        }
}

make_mutator! {
    name: TokenStreamMutator,
    default: true,
    recursive: true,
    type:
        pub struct TokenStream {
            #[field_mutator(
                VecMutator<
                    TokenTree,
                    TokenTreeMutator<
                        GroupMutator<
                            RecurToMutator<
                                TokenStreamMutator
                            >
                        >
                    >
                > = {
                    VecMutator::new(
                        TokenTreeMutator::new(
                            GroupMutator::new(
                                Delimiter::default_mutator(),
                                self_.into(),
                            ),
                            Ident::default_mutator(),
                            Punct::default_mutator(),
                            Literal::default_mutator()
                        ),
                        0..=usize::MAX,
                        true
                    )
                }
            )]
            inner: Vec<TokenTree>,
        }
}

make_mutator! {
    name: GroupMutator,
    default: true,
    type:
        pub struct Group {
            #[field_mutator(<Delimiter as DefaultMutator>::Mutator = { Delimiter::default_mutator() } )]
            delimiter: Delimiter,
            stream: TokenStream,
        }
}

pub static KEYWORDS: [&str; 59] = [
    "_",
    "abstract",
    "as",
    "async",
    "auto",
    "await",
    "become",
    "box",
    "break",
    "const",
    "continue",
    "crate",
    "default",
    "do",
    "dyn",
    "else",
    "enum",
    "extern",
    "false",
    "final",
    "fn",
    "for",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "macro",
    "macro_rules",
    "match",
    "mod",
    "move",
    "mut",
    "override",
    "priv",
    "pub",
    "ref",
    "return",
    "Self",
    "self",
    "static",
    "struct",
    "super",
    "trait",
    "true",
    "try",
    "type",
    "typeof",
    "union",
    "unsafe",
    "unsized",
    "use",
    "virtual",
    "where",
    "while",
    "yield",
    "a",
    "b",
    "c",
];

make_mutator! {
    name: IdentMutator,
    default: true,
    type:
        pub struct Ident {
            #[field_mutator(U8WithinRangeMutator = { U8WithinRangeMutator::new(..KEYWORDS.len() as u8) } )]
            sym: u8,
        }
}

make_mutator! {
    name: LiteralMutator,
    default: true,
    type:
        pub struct Literal {
            #[field_mutator(U8WithinRangeMutator = { U8WithinRangeMutator::new(..7) })]
            repr: u8,
        }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use fuzzcheck::mutators::testing_utilities::test_mutator;
    use fuzzcheck::{DefaultMutator, Mutator};

    use super::TokenStream;

    #[test]
    fn test_token_stream_mutator() {
        let m = TokenStream::default_mutator();
        test_mutator(m, 400., 400., false, true, 100, 100);
    }
}
