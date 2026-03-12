#[macro_export]
#[cfg(feature = "parser-debug")]
macro_rules! print_head {
    ($parser:expr, $caller:expr) => {
        println!(
            "=================================================================================="
        );
        println!("========== {} ==========", $caller);
        println!(
            "=================================================================================="
        );
        if $parser.at > 0 {
            println!(
                "  =>  Before  : '{:?}'",
                $parser.tokens[$parser.at - 1].kind.as_str()
            );
        }
        println!(
            "  =>  Now     : '{:?}'",
            $parser.current_token().kind.as_str()
        );

        if $parser.at + 1 < $parser.tokens.len() {
            println!("  =>  Next    : '{:?}'", $parser.peek_next().kind.as_str());
        }
        println!(
            "=================================================================================="
        );
    };
}
#[macro_export]
#[cfg(not(feature = "parser-debug"))]
macro_rules! print_head {
    ($($t:tt)*) => {};
}