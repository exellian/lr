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
    let cursor = lr::CharCursor::new("4+3-(23*2)");
    let lexer = parser::Lexer::new(cursor);
    let parser = parser::Parser::new(lexer);
    let (advanced_parser, expr) = parser::Expr::parse(parser).expect("Failed to parse Expr!");
    // do something with the parsed expression
    // ...
}
```

- It automatically generates also a lexer for the tokens that can be defined with 
simple strings or regular expressions.

- The framework can also handle left recursive non-terminal rules (e.g: Expr in the example above).

- Current main focus is also to offer additional support for quantity modifiers:

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

- Next development focus is on naming syntax for sequences and 
special builder functions that allow transforming of parsed types to enable a flawless integration with 
typing. The development is additionally motivated through the development of the rex web-framework.