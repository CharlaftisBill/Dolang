use crate::ast::Ast;
use crate::ast::lexer::Lexer;
use crate::ast::parser::Parser;
use crate::analysis::TypeChecker;

mod ast;
mod analysis;

fn main() {
    println!("Do programming language!");

    // println!("______________| LEX |______________");
    let mut lexer = Lexer::new(
        &"/Users/scripter/Desktop/Projects/Dolang/examples/simple.do.skip_test".to_string(),
    )
    .unwrap();

    let tokens = match lexer.lex() {
        Ok(tok) => tok,
        Err(_) => panic!("lexing FAILED!"),
    };

    // for (index, token) in tokens.iter().enumerate() {
    //     println!(" {index}  ---> {:?}", token)
    // }

    println!("______________| PARSE |______________");
    let mut ast = Ast::new();
    let mut parser = Parser::new(&tokens, &lexer.src, &mut ast);
    parser.parse(&lexer.src);

    // Analysis
    let mut type_analyzer = TypeChecker::new(&ast);
    type_analyzer.scan();
    

    let json = serde_json::to_string_pretty(&ast).unwrap();
    std::fs::write("/tmp/ast.json", json).expect("Unable to write file");
    println!(" AST as JSON exported to: '/tmp/ast.json'");
}
