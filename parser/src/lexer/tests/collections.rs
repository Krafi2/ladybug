use super::*;

test_parsers! {
    @expected: ok;
    @output: flatten_tts;

    list {
        "[a b \"c d\"]" => vec![
            Token::Ctrl('['),
            Token::Str("a".into()),
            Token::Space(" ".into()),
            Token::Str("b".into()),
            Token::Space(" ".into()),
            Token::DelimStr("c d".into())
        ],
        "[a[b[c[]]]]" => vec![
            Token::Ctrl('['),
            Token::Str("a".into()),
            Token::Ctrl('['),
            Token::Str("b".into()),
            Token::Ctrl('['),
            Token::Str("c".into()),
            Token::Ctrl('['),
            Token::Ctrl(']'),
            Token::Ctrl(']'),
            Token::Ctrl(']'),
            Token::Ctrl(']'),
        ],
        "[#comment\nno comment]" => vec![
            Token::Ctrl('['),
            Token::Comment("comment".into()),
            Token::Line,
            Token::Str("no".into()),
            Token::Str("comment".into()),
            Token::Ctrl(']'),
        ]
    }
    map {
        "{foo: [bar bar]\nfoo: item(param: val)}" => vec![
            Token::Ctrl('{'),
            Token::IdentOrStr("foo".into()),
            Token::Ctrl(':'),
            Token::Space(" ".into()),
            Token::Ctrl('['),
            Token::Str("bar".into()),
            Token::Str("bar".into()),
            Token::Ctrl(']'),
            Token::Line,
            Token::IdentOrStr("foo".into()),
            Token::Ctrl(':'),
            Token::Space(" ".into()),
            Token::IdentOrStr("item".into()),
            Token::Ctrl('('),
            Token::IdentOrStr("param".into()),
            Token::Ctrl(':'),
            Token::Space(" ".into()),
            Token::IdentOrStr("val".into()),
            Token::Ctrl(')'),
            Token::Ctrl('}'),
        ]
    }
    options {
        "(foo: $bar, hello: world(), with_spaces: hello spaces)" => vec![
            Token::Ctrl('('),
            Token::IdentOrStr("foo".into()),
            Token::Ctrl(':'),
            Token::Space(" ".into()),
            Token::Var("bar".into()),
            Token::Ctrl(','),
            Token::Space(" ".into()),
            Token::IdentOrStr("hello".into()),
            Token::Space(" ".into()),
            Token::IdentOrStr("word".into()),
            Token::Ctrl('('),
            Token::Ctrl(')'),
            Token::Ctrl(','),
            Token::Space(" ".into()),
            Token::IdentOrStr("with_spaces".into()),
            Token::Ctrl(':'),
            Token::Space(" ".into()),
            Token::IdentOrStr("hello spaces".into()),
            Token::Ctrl(')'),
        ]
    }
}
