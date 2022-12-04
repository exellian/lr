extern crate proc_macro;
use proc_macro::TokenStream;
use std::collections::HashMap;
use quote::quote;
use syn::{parse_macro_input, Token};
use syn::punctuated::Punctuated;

#[derive(Clone)]
struct Regex {
    lit: syn::LitStr
}

#[derive(Clone)]
enum Primitive {
    String(syn::LitStr),
    Regex(Regex)
}

enum Item {
    One(syn::Ident),
    OneOrMore(syn::Ident, Token![+]),
    ZeroOrMore(syn::Ident, Token![*]),
    Optional(syn::token::Bracket, syn::Ident)
}

#[derive(Debug)]
enum Lookahead {
    Leaf(syn::Ident),
    Node(HashMap<Primitive, Lookahead>)
}

struct Enum {
    segments: Punctuated<syn::Ident, Token![|]>,
}

struct Seq {
    segments: Vec<Item>,
}

enum SeqOrEnum {
    Seq(Seq),
    Enum(Enum)
}

enum Value {
    Primitive(Primitive),
    SeqOrEnum(SeqOrEnum)
}

struct Rule {
    ident: syn::Ident,
    eq: Token![=],
    value: Value
}

struct Grammer {
    rules: Punctuated<Rule, Token![;]>
}

macro_rules! err {
    // `()` indicates that the macro takes no argument.
    ($span: expr, $message: expr) => {
        syn::Error::new($span, $message)
        .to_compile_error()
        .into()
    };
}

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let stream = {
        let grammer = parse_macro_input!(input as Grammer);
        let mut rules = HashMap::new();

        for rule in grammer.rules {
            if rules.contains_key(&rule.ident) {
                return err!(rule.ident.span(), format!("duplicate rule {}", rule.ident))
            }
            rules.insert(rule.ident, rule.value);
        }

        for (_, val) in &rules {
            match val {
                Value::Primitive(_) => {}
                Value::SeqOrEnum(soe) => match soe {
                    SeqOrEnum::Seq(seq) => {
                        for item in &seq.segments {
                            let i = item.ident();
                            if !rules.contains_key(i) {
                                return err!(i.span(), format!("cannot find identifier {}", i))
                            }
                        }
                    }
                    SeqOrEnum::Enum(e) => {
                        for i in &e.segments {
                            if !rules.contains_key(i) {
                                return err!(i.span(), format!("cannot find identifier {}", i))
                            }
                        }
                    }
                }
            }
        }

        let mut lookaheads = HashMap::new();

        for (ident, val) in &rules {
            match val {
                Value::SeqOrEnum(SeqOrEnum::Enum(e)) => {
                    let lookahead = match Enum::build_lookahead(0, &e.segments.iter().cloned().collect(), &rules) {
                        Ok(res) => res,
                        Err(err) => return err
                    };
                    lookaheads.insert(ident.clone(), lookahead);
                }
                _ => {}
            }
        }

        dbg!(lookaheads);

        quote! {

        }
    };
    TokenStream::from(stream)
}

mod parse {
    use syn::ext::IdentExt;
    use syn::{bracketed, Error, LitStr, token, Token};
    use syn::parse::ParseStream;
    use syn::punctuated::Punctuated;
    use syn::spanned::Spanned;
    use crate::{Enum, Grammer, Item, Primitive, Regex, Rule, Seq, SeqOrEnum, Value};

    impl syn::parse::Parse for Grammer {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            Ok(Grammer {
                rules: input.parse_terminated(Rule::parse)?,
            })
        }
    }

    impl syn::parse::Parse for Rule {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            Ok(Rule {
                ident: syn::Ident::parse_any(input)?,
                eq: input.parse()?,
                value: input.parse()?,
            })
        }
    }

    impl syn::parse::Parse for Value {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let lookahead = input.lookahead1();
            if lookahead.peek(syn::Ident::peek_any) || lookahead.peek(token::Bracket) {
                input.parse().map(Value::SeqOrEnum)
            } else {
                input.parse().map(Value::Primitive)
            }
        }
    }

    impl syn::parse::Parse for SeqOrEnum {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            if input.peek2(Token![|]) {
                input.parse().map(SeqOrEnum::Enum)
            } else {
                input.parse().map(SeqOrEnum::Seq)
            }
        }
    }

    impl syn::parse::Parse for Enum {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let mut segments = Punctuated::new();
            let first = syn::Ident::parse_any(input)?;
            segments.push(first);

            while input.peek(Token![|]) {
                segments.push_punct(input.parse()?);
                segments.push(syn::Ident::parse_any(input)?);
            }

            Ok(Enum {
                segments,
            })
        }
    }

    impl syn::parse::Parse for Seq {
        fn parse(input: ParseStream) -> syn::Result<Self> {

            let mut segments = vec![];
            let first = input.parse()?;
            match first {
                Item::One(_) | Item::OneOrMore(_, _) => {}
                Item::ZeroOrMore(ident, star) => {
                    let error_span = ident.span().join(star.span()).unwrap();
                    return Err(Error::new(error_span, "Sequence can't start with an zero-or-more rule"))
                }
                Item::Optional(bracket, _) => {
                    return Err(Error::new(bracket.span, "Sequence can't start with an optional rule"))
                }
            }
            segments.push(first);

            while input.peek(syn::Ident::peek_any) || input.peek(token::Bracket) {
                segments.push(input.parse()?);
            }

            Ok(Seq {
                segments,
            })
        }
    }

    impl syn::parse::Parse for Item {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            if input.peek(token::Bracket) {
                let content;
                Ok(Item::Optional(bracketed!(content in input), content.parse()?))
            } else {
                let first = input.parse()?;
                if input.peek(Token![+]) {
                    Ok(Item::OneOrMore(first, input.parse()?))
                } else if input.peek(Token![*]) {
                    Ok(Item::ZeroOrMore(first, input.parse()?))
                } else {
                    Ok(Item::One(first))
                }
            }
        }
    }

    impl syn::parse::Parse for Primitive {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let lit: LitStr = input.parse()?;
            if lit.suffix() == "r" {
                Ok(Primitive::Regex(Regex {
                    lit,
                }))
            } else if lit.suffix().is_empty() {
                Ok(Primitive::String(lit))
            } else {
                Err(Error::new(lit.span(), format!("unknown suffix {}", lit.suffix())))
            }
        }
    }

}
 
mod implementation {
    use proc_macro::TokenStream;
    use std::collections::HashMap;
    use std::fmt::{Debug, Formatter};
    use std::hash::{Hash, Hasher};
    use syn::Ident;
    use crate::{Enum, Grammer, Item, Lookahead, Primitive, Seq, SeqOrEnum, Value};

    impl Grammer {

        pub fn seqs(&self) -> Vec<&Seq> {
            let mut seqs = vec![];
            for rule in &self.rules {
                match &rule.value {
                    Value::SeqOrEnum(SeqOrEnum::Seq(seq)) => seqs.push(seq),
                    _ => {}
                }
            }
            seqs
        }

        pub fn enums(&self) -> Vec<&Enum> {
            let mut enums = vec![];
            for rule in &self.rules {
                match &rule.value {
                    Value::SeqOrEnum(SeqOrEnum::Enum(e)) => enums.push(e),
                    _ => {}
                }
            }
            enums
        }

        pub fn primitives(&self) -> Vec<&Primitive> {
            let mut primitives = vec![];
            for rule in &self.rules {
                match &rule.value {
                    Value::Primitive(token) => primitives.push(token),
                    _ => {}
                }
            }
            primitives
        }

    }

    impl Value {

        /**
         * Peeks the nth unique primitive
         */
        pub fn peek_nth_unique<'a>(&'a self, n: usize, rules: &'a HashMap<Ident, Value>) -> Option<&'a Primitive> {
            match self {
                Value::Primitive(p) => if n == 0 {
                    Some(p)
                } else {
                    None
                }
                Value::SeqOrEnum(soe) => match soe {
                    SeqOrEnum::Seq(seq) => seq.peek_nth_unique(n, rules),
                    SeqOrEnum::Enum(e) => e.peek_nth_unique(n, rules)
                }
            }
        }

        pub fn fixed_len(&self, rules: &HashMap<Ident, Value>) -> Option<usize> {
            match self {
                Value::Primitive(p) => Some(1),
                Value::SeqOrEnum(soe) => match soe {
                    SeqOrEnum::Seq(seq) => seq.fixed_len(rules),
                    SeqOrEnum::Enum(e) => e.fixed_len(rules)
                }
            }
        }
    }

    impl Item {

        pub fn ident(&self) -> &syn::Ident {
            match self {
                Item::One(i) => i,
                Item::OneOrMore(i, _) => i,
                Item::ZeroOrMore(i, _) => i,
                Item::Optional(_, i) => i
            }
        }

        pub fn fixed_len(&self, rules: &HashMap<Ident, Value>) -> Option<usize> {
            match self {
                Item::One(i) => {
                    rules[i].fixed_len(rules)
                },
                Item::OneOrMore(_, _) => None,
                Item::ZeroOrMore(_, _) => None,
                Item::Optional(_, _) => None
            }
        }

        /**
         * Peeks the nth unique primitive
         */
        pub fn peek_nth_unique<'a>(&'a self, mut n: usize, rules: &'a HashMap<Ident, Value>) -> Option<&'a Primitive> {
            match self {
                Item::One(i) => rules.get(i).unwrap().peek_nth_unique(n, rules),
                Item::OneOrMore(i, _) => rules.get(i).unwrap().peek_nth_unique(n, rules),
                Item::ZeroOrMore(_, _) => None,
                Item::Optional(_, _) => None
            }
        }
    }

    impl Seq {

        pub fn fixed_len(&self, rules: &HashMap<Ident, Value>) -> Option<usize> {
            let mut total = 0;
            for seg in &self.segments {
                match seg.fixed_len(rules) {
                    Some(n) => total += n,
                    None => return None
                }
            }
            Some(total)
        }

        /**
         * Peeks the nth unique primitive
         */
        pub fn peek_nth_unique<'a>(&'a self, mut n: usize, rules: &'a HashMap<Ident, Value>) -> Option<&'a Primitive> {
            for seg in &self.segments {
                if let Some(len) = seg.fixed_len(rules) {
                    if n < len {
                        return seg.peek_nth_unique(n, rules);
                    }
                    n -= len;
                } else {
                    return seg.peek_nth_unique(n, rules);
                }
            }
            None
        }
    }

    impl Enum {

        // Lift out recursive left side rule to top level
        pub fn normalize() {}

        /**
         * Builds a lookahead tree unique tokens
         */
        pub fn build_lookahead(depth: usize, segments: &Vec<syn::Ident>, rules: &HashMap<Ident, Value>) -> Result<HashMap<Primitive, Lookahead>, TokenStream> {
            let mut layer = HashMap::new();
            for seg in segments {
                let val = &rules[seg];
                if let Some(prim) = val.peek_nth_unique(depth, rules) {
                    if !layer.contains_key(prim) {
                        layer.insert(prim, vec![]);
                    }
                    layer.get_mut(prim).unwrap().push(seg.clone());
                } else {
                    return Err(err!(seg.span(), format!("Can't lookahead variant. This rule doesn't offer more than {} unique starting tokens!", depth + 1)));
                }
            }
            let mut merged = HashMap::new();
            for (key, mut value) in layer {
                if value.len() == 1 {
                    merged.insert(key.clone(), Lookahead::Leaf(value.pop().unwrap()));
                } else {
                    let child = Self::build_lookahead(depth + 1, &value, rules)?;
                    merged.insert(key.clone(), Lookahead::Node(child));
                }
            }
            Ok(merged)
        }

        /**
         * Peeks the nth unique primitive
         */
        pub fn peek_nth_unique<'a>(&'a self, n: usize, rules: &'a HashMap<Ident, Value>) -> Option<&'a Primitive> {
            let mut tok = None;
            for seg in &self.segments {
                let val = &rules[seg];
                if let Some(t) = val.peek_nth_unique(n, rules) {
                    if let Some(old) = tok {
                        if old != t {
                            return None;
                        }
                    } else {
                        tok = Some(t);
                    }
                } else {
                    return None;
                }
            }
            None
        }

        pub fn fixed_len(&self, rules: &HashMap<Ident, Value>) -> Option<usize> {
            let mut n = None;
            for seg in &self.segments {
                let val = &rules[seg];
                if let Some(len) = val.fixed_len(rules) {
                    if let Some(nn) = n {
                        if nn != len {
                            return None;
                        }
                    } else {
                        n = Some(len);
                    }

                } else {
                    return None;
                }
            }
            None
        }
    }

    impl Hash for Primitive {
        fn hash<H: Hasher>(&self, state: &mut H) {
            match self {
                Primitive::String(lit) => {
                    let mut hstr = lit.value();
                    hstr.push_str("0");
                    hstr.hash(state)
                },
                Primitive::Regex(reg) => {
                    let mut hstr = reg.lit.value();
                    hstr.push_str("1");
                    hstr.hash(state)
                }
            }
        }
    }

    impl Debug for Primitive {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", match self {
                Primitive::String(str) => str.value(),
                Primitive::Regex(reg) => reg.lit.value()
            })
        }
    }

    impl Eq for Primitive {}

    impl PartialEq for Primitive {
        fn eq(&self, other: &Self) -> bool {
            match self {
                Primitive::String(lit) => match other {
                    Primitive::String(lit1) => lit.value() == lit1.value(),
                    Primitive::Regex(_) => false
                }
                Primitive::Regex(reg) => match other {
                    Primitive::String(_) => false,
                    Primitive::Regex(reg1) => reg.lit.value() == reg1.lit.value()
                }
            }
        }
    }
}
