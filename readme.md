# LR
lr is a simple procedural macro for generating a recursive parser from a BNF-based grammer language.
The following example shows the grammer language:
```rust
fn main() {

    lr::parser1! {
        Plus = "+";
        Minus = "-";
        Mul = "*";
        Div = "/";
        Num = r"([1-9][0-9]*|0)";
        ParenLeft = "(";
        ParenRight = ")";
        
        Expr =  BinAp | UnAp | Group | Num;
        Group = ParenLeft Expr ParenRight;
        BinAp = Expr Op Expr;
        UnAp = Op Expr;
        Op = Minus | Plus | Div | Mul;
    }
    let lexer = parser::Lexer::new("4+3-(23*2)").expect("failed to tokenize input");
    let parser = parser::Parser::new(lexer);
    let (advanced_parser, expr) = parser::Expr::parse(parser).expect("Failed to parse Expr!");
    // do something with the parsed expression
    // ...
}
```

- The framework can also handle left recursive non-terminal rules (e.g: Expr in the example above).

- Current main focus is also to offer additional support for quantity modifiers:

- In the future a naming syntax for sequences will be introduced and 
special builder functions will be integrated that should allow transforming of the parsed syntax tree to enable flawless integration with 
type systems.

## Current state

- Main goal for now is the generation of a lexer for the tokens that can be defined with 
simple strings or regular expressions. Therefore a regex parser and a token parser generator needs to 
be implemented

- Additionally support for quantity modifiers is implemented:

```rust
fn main() {

    lr::parser1! {
        
        Document = Function*; // zero or more functions
        Function = Name BraceLeft FunctionBlock BraceRight;
        FunctionBlock = [Program]; // optional program
        Program = Statement+; // one or more statements
    }
}
```
