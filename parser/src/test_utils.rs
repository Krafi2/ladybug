#[allow(unused_macros)]
macro_rules! test_parsers {
    (__test__ $exp_trans:path; $out_trans:path; $parser:ident { $($input:expr => $expected:expr),+ $(,)? } ) => {
        #[test]
        fn $parser() {
            let parser = ::chumsky::Parser::then_ignore(super::$parser(), chumsky::prelude::end());
            let tests = [$( ($input, $exp_trans($expected)) ),+];

            let errors = tests.into_iter().enumerate().filter_map(|(i, (input, expected))| {
                let stream = $crate::stream_from_str(input);
                let output = ::chumsky::Parser::parse(&parser, stream);
                let output = $out_trans(output);
                if output.as_ref() == expected.as_ref() {
                    None
                } else {
                    Some((i, input, output, expected))
                }

            }).collect::<Vec<_>>();

            if !errors.is_empty() {
                for (i, input, output, expected) in errors {
                    eprintln!("[Test {} failed]", i);
                    eprintln!("input:\n{:#?}\noutput:\n{:#?}\nexpected:\n{:#?}", input, output, expected)
                }
                panic!("Test failed")
            }
        }
    };
    (__test__ $exp_trans:path; $out_trans:path; $parser:ident {}) => {
        #[test]
        fn $parser() {
            unimplemented!()
        }
    };
    (@expected: $exp_trans:path; @output: $out_trans:path; $( $parser:ident $body:tt )* ) => {
        $(
            test_parsers!{ __test__ $exp_trans; $out_trans; $parser $body }
        )*
    };
}
