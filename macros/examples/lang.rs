#[macro_use]
extern crate macros;

extern crate lr;

fn main() {
    macros::parser! {
        Plus = " + ";
        Minus = " - ";
        Mul = " * ";
        Div = " / ";
        Ident = r"[a - zA - Z][a - zA - Z0 - 9] * ";
        ParenLeft = "(";
        ParenRight = ")";


        //Expr = BinAp | Ap;
        //BinAp = Expr Operator Expr;
        //Operator = Plus | Minus | Mul | Div;

        //Expr = Ap | UnAp;
        Expr = Ap | BinAp | UnAp;
        Ap = Ident ParenLeft ParenRight;
        BinAp = Expr Plus Expr;
        UnAp = Minus Expr;

    }
}