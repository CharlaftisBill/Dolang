use Dolang::ast::{Ast, lexer::Lexer, parser::Parser};
use std::fs;

mod common;

#[test]
fn test_all_samples() {
    let samples_dir = "examples";
    let entries = fs::read_dir(samples_dir)
        .expect("Directory 'examples' not found. Create it and add .lang files.");

    for entry in entries {
        let entry = entry.expect("Error reading file");
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("do") {
            let file_name = path.file_stem().unwrap().to_str().unwrap();
            println!("  -------> Testing: '{}'", file_name);

            let mut lexer = Lexer::new(&path.as_os_str().to_str().unwrap()).unwrap();
            let tokens = lexer.lex().unwrap();

            let mut ast = Ast::new();
            let mut parser = Parser::new(&tokens, &lexer.src, &mut ast);
            parser.parse(&lexer.src);

            common::assert_json_snapshot(file_name, &ast);
        }
    }
}
