use macros::parser1;

fn main() {
    parser1! {
        Plus = "+";
        Minus = "-";
        Mul = "*";
        Div = "/";
        Ident = r"[a-zA-Z][a-zA-Z0-9]*";
        ParenLeft = "(";
        ParenRight = ")";
        Comma = ",";

        Expr = [Minus] CoreExpr;
        CoreExpr = Minus* Term [Suffix];
        Suffix = Term+ Operator Term;
        Operator = Plus | Minus | Mul | Div;
        Term = Call | Ident | Group;
        Call = Ident ParenLeft [Arguments] ParenRight;
        Arguments = Expr ArgumentTail*;
        ArgumentTail = Comma Expr;
        Group = ParenLeft Expr ParenRight;
    }
}
