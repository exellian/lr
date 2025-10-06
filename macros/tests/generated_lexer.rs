use lr::Cursor;
use macros::parser1;

parser1! {
    Foo = "foo";
    Plus = "+";
    Bar = "bar";
    Minus = "-";
    Num = "123";

    Start = Foo Plus Bar Minus Num;
}

#[test]
fn generated_lexer_tokenizes_mixed_input() {
    let mut lexer = match parser::Lexer::new("foo+bar-123") {
        Ok(lexer) => lexer,
        Err(parser::LexError::UnexspectedCharacter { index }) => {
            panic!("lexer failed at index {}", index)
        }
    };

    let mut tokens = Vec::new();
    while let Some(token) = lexer.next() {
        let name = match token {
            parser::Token::Foo(_) => "Foo",
            parser::Token::Plus(_) => "Plus",
            parser::Token::Bar(_) => "Bar",
            parser::Token::Minus(_) => "Minus",
            parser::Token::Num(_) => "Num",
        };
        tokens.push(name);
    }

    assert_eq!(tokens, ["Foo", "Plus", "Bar", "Minus", "Num"]);
}
