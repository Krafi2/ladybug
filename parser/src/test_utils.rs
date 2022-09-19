macro_rules! test_parsers {
    (test $parser:ident) => {
        #[test]
        fn $parser() {
            unimplemented!()
        }
    };
    (test $parser:ident $($input:expr => $expected:expr),+) => {
        #[test]
        fn $parser() {
            let parser = ::chumsky::Parser::then_ignore(super::$parser(), chumsky::prelude::end());
            let tests = [$(($input, Ok($expected))),+];

            let errors = tests.into_iter().enumerate().filter_map(|(i, (input, expected))| {
                let output = ::chumsky::Parser::parse(&parser, input.clone()).map(Into::into);
                if output.as_ref() == expected.as_ref() {
                    None
                } else {
                    Some((i, input, output, expected))
                }

            }).collect::<Vec<_>>();

            if !errors.is_empty() {
                for (i, input, output, expected) in errors {
                    eprintln!("[Test {}]", i);
                    eprintln!("input:\n{:#?}\noutput:\n{:#?}\nexpected:\n{:#?}", input, output, expected)
                }
                panic!("Test failed")
            }
        }
    };
    ($($parser:ident { $($inp:expr => $out:expr),* $(,)? })*) => {
        $(
            test_parsers!{ test $parser $($inp => $out),* }
        )*
    };
}
