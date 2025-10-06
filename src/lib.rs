mod cursor;
mod parse;
mod parser;
mod peek;
mod span;

pub use cursor::Cursor;
pub use parse::Parse;
pub use parser::Parser;
pub use peek::Peek;
pub use span::{Span, SpanOwned};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        #[derive(Clone)]
        struct Lexer {
            tokens: Vec<Token>,
            position: usize,
        }

        impl Lexer {
            fn new(tokens: Vec<Token>) -> Self {
                Self {
                    tokens,
                    position: 0,
                }
            }
        }

        #[derive(Clone)]
        enum Token {
            Ident(Ident),
            Number,
        }

        #[derive(Clone, Debug, PartialEq)]
        struct Ident;

        impl Cursor for Lexer {
            type Item = Token;

            fn next(&mut self) -> Option<Self::Item> {
                let token = self.tokens.get(self.position).cloned();
                if token.is_some() {
                    self.position += 1;
                }
                token
            }

            fn peek(&mut self, k: usize) -> Option<&Self::Item> {
                self.tokens.get(self.position + k)
            }

            fn position(&self) -> usize {
                self.position
            }
        }

        impl Parse for Ident {
            type Error = ();
            type Parser = Lexer;

            fn parse(mut lexer: Self::Parser) -> Result<(Self::Parser, Self), Self::Error> {
                match lexer.next() {
                    Some(Token::Ident(ident)) => Ok((lexer, ident)),
                    _ => Err(()),
                }
            }
        }

        struct IdentToken;

        impl Peek<Lexer> for IdentToken {
            fn peek(token: &Token) -> bool {
                matches!(token, Token::Ident(_))
            }
        }

        let lexer = Lexer::new(vec![Token::Ident(Ident), Token::Number]);
        let mut parser = Parser::new(lexer);

        assert!(parser.peek(IdentToken, 0));
        assert!(!parser.peek(IdentToken, 1));
    }
}
