use crate::lexing::lexer::Lexer;

mod lexing;

fn main() {
    println!("Do programming language!");

    let mut lexer = Lexer::new(&"/Users/scripter/Desktop/Projects/Dolang/examples/simple.do".to_string()).unwrap();
    
   let tokens = match lexer.lex(){
       Ok(tok) => tok,
        Err(_) =>  panic!("lexing FAILED!"),
   };
    
   for token in tokens {
       println!("---> {}",token.to_string())
   };
}
