#[allow(unused_macros)]
macro_rules! test_parsers {
    (__test__ $stream:path; $exp_trans:path; $out_trans:path; $parser:ident { $($input:expr => $expected:expr),+ $(,)? } ) => {
        #[test]
        fn $parser() {
            use ::chumsky::Parser;
            let parser = super::$parser().then_ignore(chumsky::prelude::end());
            let tests = [$( ($input, $exp_trans($expected)) ),+];

            let errors = tests.into_iter().enumerate().filter_map(|(i, (input, expected))| {
                let stream = $stream(input.as_ref());
                let output = parser.parse(stream);
                let output = $out_trans(output);
                if &output == &expected {
                    None
                } else {
                    Some((i, input, output, expected))
                }

            }).collect::<Vec<_>>();

            if !errors.is_empty() {
                for (i, input, output, expected) in errors {
                    eprintln!("[Test {} failed]", i);
                    eprintln!("input:\n{:?}\noutput:\n{:?}\nexpected:\n{:?}", input, output, expected)
                }
                panic!("Test failed")
            }
        }
    };
    (__test__ $stream:path; $exp_trans:path; $out_trans:path; $parser:ident {}) => {
        #[test]
        fn $parser() {
            unimplemented!()
        }
    };
    (@stream: $stream:path; @expected: $exp_trans:path; @output: $out_trans:path; $( $parser:ident $body:tt )* ) => {
        $(
            test_parsers!{ __test__ $stream; $exp_trans; $out_trans; $parser $body }
        )*
    };
}
