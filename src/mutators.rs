use fuzzcheck::mutators::character_classes::CharacterMutator;
use fuzzcheck::mutators::integer_within_range::U8WithinRangeMutator;
use fuzzcheck::mutators::map::MapMutator;
use fuzzcheck::mutators::recursive::RecurToMutator;
use fuzzcheck::mutators::unit::UnitMutator;
use fuzzcheck::mutators::vector::VecMutator;
use fuzzcheck::{make_mutator, DefaultMutator, Mutator};

use crate::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

make_mutator! {
    name: DelimiterMutator,
    default: true,
    type:
        pub enum Delimiter {
            Parenthesis,
            Brace,
            Bracket,
            #[ignore_variant]
            None(()),
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
    name: SpanMutator,
    default: true,
    type:
        pub struct Span;
}

make_mutator! {
    name: PunctMutator,
    default: true,
    type:
    pub struct Punct {
        #[field_mutator(CharacterMutator = { CharacterMutator::new(vec!['!' ..= '!', '#'..='\'', '*' ..= '/', ':' ..= '@', '^' ..= '_', '|' ..= '|', '~' ..= '~']) })]
        ch: char,
        #[field_mutator(<Spacing as DefaultMutator>::Mutator = { Spacing::default_mutator() } )]
        spacing: Spacing,
        // span: Span,
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

type IdentSymMutator = impl Mutator<String>;

static KEYWORDS: [&str; 58] = [
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

#[no_coverage]
fn ident_mutator() -> IdentSymMutator {
    MapMutator::new(
        U8WithinRangeMutator::new(..KEYWORDS.len() as u8),
        |string: &String| KEYWORDS.iter().position(|&x| x == string).map(|x| x as u8),
        |x| KEYWORDS[*x as usize].to_owned(),
        |x, _orig_cplx| x.len() as f64,
    )
}

make_mutator! {
    name: IdentMutator,
    default: true,
    type:
        pub struct Ident {
            #[field_mutator(IdentSymMutator = { ident_mutator() } )]
            sym: String,
            #[field_mutator(UnitMutator<bool> = { UnitMutator::new(false, 1.0) } )]
            raw: bool,
        }
}

make_mutator! {
    name: LiteralMutator,
    default: true,
    type:
        pub struct Literal {
            #[field_mutator(LiteralReprMutator = { literal_repr_mutator() })]
            repr: String,
            // span: Span,
        }
}

type LiteralReprMutator = impl Mutator<String>;

#[no_coverage]
fn literal_repr_mutator() -> LiteralReprMutator {
    MapMutator::new(
        U8WithinRangeMutator::new(..8),
        |x: &String| {
            Some(match x.as_bytes() {
                b"1_u8" => 0,
                b"2" => 1,
                b"0.1_f32" => 2,
                b"1.1" => 3,
                b"\"s\"" => 4,
                b"r#\"raw\"#" => 5,
                b"b'a'" => 6,
                b"b\"bs\"" => 7,
                _ => return None,
            })
        },
        |x| {
            match x {
                0 => "1_u8",
                1 => "2",
                2 => "0.1_f32",
                3 => "1.1",
                4 => "\"s\"",
                5 => r###"r#"raw"#"###,
                6 => "b'a'",
                7 => "b\"bs\"",
                _ => unreachable!(),
            }
            .to_owned()
        },
        |x, _orig_cplx| x.len() as f64,
    )
}
