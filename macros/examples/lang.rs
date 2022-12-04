#[macro_use]
extern crate macros;

fn main() {
    macros::parser! {
        Plus = "+";
        Minus = "-";
        Mul = "*";
        Div = "/";
        Ident = r"[a-zA-Z][a-zA-Z0-9]*";
        ParenLeft = "(";
        ParenRight = ")";

        Operator = Plus | Minus | Mul | Div;
        BinAp = Expr Operator Expr;
        Ap = Ident ParenLeft ParenRight;
        Expr = BinAp | Ap;
        Test = Expr;
    }
}