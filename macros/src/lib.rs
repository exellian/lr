mod codegen;
mod state;

extern crate proc_macro;
use crate::codegen::Generator;
use crate::implementation::{calculate_need_box, get_left_name, get_right_name};
use crate::state::State;
use proc_macro::TokenStream;
use quote::quote;
use std::collections::{HashMap, HashSet};
use syn::__private::{Span, TokenStream2};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Token};

#[derive(Clone)]
struct Regex {
    lit: syn::LitStr,
}

#[derive(Clone)]
enum Token {
    String(syn::LitStr),
    Regex(Regex),
}

#[derive(Clone)]
enum RuleRef {
    One(syn::Ident),
    OneOrMore(syn::Ident),
    ZeroOrMore(syn::Ident),
    Optional(syn::Ident),
}

#[derive(Clone)]
struct Enum {
    segments: Punctuated<syn::Ident, Token![|]>,
}

/**
 * Can start arbitrary
 */
#[derive(Clone)]
struct Seq {
    segments: Vec<RuleRef>,
}

#[derive(Clone)]
enum SeqOrEnum {
    Seq(Seq),
    Enum(Enum),
}

#[derive(Clone)]
enum RuleValue {
    Token(Token),
    SeqOrEnum(SeqOrEnum),
}

struct Rule {
    name: syn::Ident,
    eq: Token![=],
    value: RuleValue,
}

struct Grammer {
    rules: Punctuated<Rule, Token![;]>,
}

#[derive(Clone)]
enum RightTree {
    Leaf(syn::Ident),
    Node(HashMap<syn::Ident, RightTree>),
}

#[derive(Clone, Debug)]
enum LookaheadTree {
    Leaf(syn::Ident),
    Node(HashMap<syn::Ident, LookaheadTree>),
}

#[derive(Debug)]
struct Lookahead {
    lefts: HashMap<syn::Ident, LookaheadTree>,
    rights: HashMap<syn::Ident, LookaheadTree>,
}

enum TokenResult {
    Some(syn::Ident),
    // End of rule
    Eor,
    // None
    None,
}

enum TokenResult1<'a> {
    Some(&'a syn::Ident),
    // End of rule
    Eor,
    // None
    None,
}

enum RuleResult<'a> {
    Some(&'a RuleRef),
    Eor,
    None,
}

#[derive(Clone, Debug)]
struct EnumPath {
    constructors: Vec<syn::Ident>,
    seq_or_token: syn::Ident,
}

enum Split {
    None,
    Right(SeqOrEnum),
    Both(SeqOrEnum, SeqOrEnum),
}

macro_rules! err {
    // `()` indicates that the macro takes no argument.
    ($span: expr, $message: expr) => {
        syn::Error::new($span, $message).to_compile_error().into()
    };
}

fn errors(errs: Vec<(Span, String)>) -> TokenStream {
    assert!(errs.len() > 0);
    let mut err: Option<syn::Error> = None;
    for (s, m) in errs {
        let new_err = syn::Error::new(s, m);
        let res_err = match err.take() {
            None => new_err,
            Some(mut e) => {
                e.combine(new_err);
                e
            }
        };
        err = Some(res_err);
    }
    err.take().unwrap().to_compile_error().into()
}

fn parser_fn(rule: &syn::Ident, rules: &HashMap<syn::Ident, RuleValue>) -> TokenStream2 {
    match rules.get(rule).unwrap() {
        RuleValue::Token(_) => quote! { parse_token },
        RuleValue::SeqOrEnum(_) => quote! { parse },
    }
}

#[proc_macro]
pub fn parser1(input: TokenStream) -> TokenStream {
    let stream = {
        let grammer = parse_macro_input!(input as Grammer);
        let state = match state::Builder::new(grammer).build() {
            Ok(s) => s,
            Err(err) => return err,
        };
        Generator::new(state).generate()
    };
    TokenStream::from(stream)
}

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let stream = {
        let grammer = parse_macro_input!(input as Grammer);
        let mut rules = HashMap::new();

        for rule in grammer.rules {
            if rules.contains_key(&rule.name) {
                return err!(rule.name.span(), format!("duplicate rule {}", rule.name));
            }
            rules.insert(rule.name, rule.value);
        }

        for (rule, val) in &rules {
            match val {
                RuleValue::Token(_) => {}
                RuleValue::SeqOrEnum(soe) => match soe {
                    SeqOrEnum::Seq(seq) => {
                        if seq.segments.len() < 1 {
                            return err!(
                                rule.span(),
                                format!("Sequence must be at least length 1!")
                            );
                        }
                        match &seq.segments[0] {
                            RuleRef::One(_) => {}
                            _ => {
                                return err!(
                                    rule.span(),
                                    format!("Sequence must start with a non modified value!")
                                )
                            }
                        }
                        for r_ref in &seq.segments {
                            let i = r_ref.rule_name();
                            if !rules.contains_key(i) {
                                return err!(i.span(), format!("cannot find identifier {}", i));
                            }
                        }
                    }
                    SeqOrEnum::Enum(e) => {
                        for i in &e.segments {
                            if !rules.contains_key(i) {
                                return err!(i.span(), format!("cannot find identifier {}", i));
                            }
                        }
                    }
                },
            }
        }

        let mut size_errs = vec![];
        for (rule, val) in &rules {
            match val {
                RuleValue::Token(_) => {}
                RuleValue::SeqOrEnum(soe) => {
                    let size = match soe {
                        SeqOrEnum::Seq(seq) => seq.calculate_min_size(&rules, &mut HashSet::new()),
                        SeqOrEnum::Enum(e) => e.calculate_min_size(&rules, &mut HashSet::new()),
                    };
                    if let None = size {
                        size_errs.push((rule.span(), format!("Rule has infinite size!")));
                    }
                }
            }
        }
        if !size_errs.is_empty() {
            return errors(size_errs);
        }

        for (rule, val) in rules.clone() {
            match val {
                RuleValue::Token(_) => {}
                RuleValue::SeqOrEnum(soe) => {
                    match &soe {
                        SeqOrEnum::Seq(seq) => {
                            if !seq.leftest_can_be(&rule, &rules, &mut HashSet::new()) {
                                continue;
                            }
                        }
                        SeqOrEnum::Enum(e) => {
                            if !e.leftest_can_be(&rule, &rules, &mut HashSet::new()) {
                                continue;
                            }
                        }
                    };

                    let split = match soe {
                        SeqOrEnum::Seq(seq) => {
                            // If the rule is left recursive the we split
                            match seq.calculate_split(&rule, &mut rules, &mut HashSet::new()) {
                                Ok(s) => s,
                                Err(err) => return err,
                            }
                        }
                        SeqOrEnum::Enum(e) => {
                            match e.calculate_split(&rule, &mut rules, &mut HashSet::new()) {
                                Ok(s) => s,
                                Err(err) => return err,
                            }
                        }
                    };
                    match split {
                        Split::None => {
                            // Its not possible for a left recursive rule
                            // to be not split
                            unreachable!()
                        }
                        Split::Right(_) => {
                            // Its not possible for a left recursive rule
                            // to have no start case.
                            unreachable!()
                        }
                        Split::Both(left, right) => {
                            let new_left_name = if !left.is_empty() {
                                let name = get_left_name(&rule, None, &rules);
                                rules.insert(name.clone(), RuleValue::SeqOrEnum(left));
                                Some(name)
                            } else {
                                None
                            };

                            let new_right_name = if !right.is_empty() {
                                let name = get_right_name(&rule, None, &rules);
                                rules.insert(name.clone(), RuleValue::SeqOrEnum(right));
                                Some(name)
                            } else {
                                None
                            };

                            debug_assert!(new_left_name.is_some() || new_right_name.is_some());

                            if new_left_name.is_some() && new_right_name.is_some() {
                                // If we have left and right sides non empty
                                // then we rewrite the rule as follows:
                                // Rule = RuleLeft RuleRight+
                                rules.insert(
                                    rule,
                                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(Seq {
                                        segments: vec![
                                            RuleRef::One(new_left_name.unwrap()),
                                            RuleRef::OneOrMore(new_right_name.unwrap()),
                                        ],
                                    })),
                                );
                            } else if new_left_name.is_some() {
                                rules.insert(
                                    rule,
                                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(Seq {
                                        segments: vec![RuleRef::OneOrMore(new_left_name.unwrap())],
                                    })),
                                );
                            } else {
                                debug_assert!(new_right_name.is_some());
                                rules.insert(
                                    rule,
                                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(Seq {
                                        segments: vec![RuleRef::OneOrMore(new_right_name.unwrap())],
                                    })),
                                );
                            }
                        }
                    }
                }
            }
        }

        //let mut lookaheads = HashMap::new();
        for (rule, val) in &rules {
            match val {
                RuleValue::Token(_) => {}
                RuleValue::SeqOrEnum(soe) => match soe {
                    SeqOrEnum::Seq(_) => {}
                    SeqOrEnum::Enum(e) => {}
                },
            }
        }

        /*
         * Each key stores a rule
         * For each rule we store a set of rules that contain that rule (key)
         */
        let mut recursives = HashMap::new();
        for (rule, val) in &rules {
            match val {
                RuleValue::Token(_) => {}
                RuleValue::SeqOrEnum(soe) => match soe {
                    SeqOrEnum::Seq(seq) => {
                        let mut rec = HashSet::new();
                        seq.calculate_self_recursive_rules(
                            rule,
                            &rules,
                            &mut HashSet::new(),
                            &mut rec,
                        );
                        recursives.insert(rule.clone(), rec);
                    }
                    SeqOrEnum::Enum(e) => {
                        let mut rec = HashSet::new();
                        e.calculate_self_recursive_rules(
                            rule,
                            &rules,
                            &mut HashSet::new(),
                            &mut rec,
                        );
                        recursives.insert(rule.clone(), rec);
                    }
                },
            }
        }

        /* Now we transform the upper map so that we get for each rule all inner rules that need to be boxed.
         * This map stores for every rule the rules that need to be boxed.
         */
        let need_box = calculate_need_box(&recursives, &rules);

        let mut items = vec![];
        items.push(quote! {
            enum LexError {
                UnexspectedCharacter
            }
            enum Error {
                UnexpectedToken
            }
        });

        let mut tokens = vec![];

        for (rule, val) in &rules {
            match val {
                RuleValue::Token(token) => {
                    tokens.push(quote! {
                        #rule(#rule<'a>)
                    });
                    items.push(quote! {

                        struct #rule<'a> {
                            span: Span<'a>
                        }

                        impl<'a> Parse for #rule<'a> {
                            type Error = Error;
                            type Parser = Lexer<'a>;

                            fn parse(mut cursor: Self::Parser) -> Result<(Self::Parser, Self), Self::Error> {
                                if let Some(Token::#rule(token)) = cursor.next() {
                                    Ok((cursor, token))
                                } else {
                                    Err(Error::UnexpectedToken)
                                }
                            }
                        }
                    })
                }
                RuleValue::SeqOrEnum(soe) => match soe {
                    SeqOrEnum::Seq(seq) => {
                        let mut fields = vec![];
                        let mut constructor_fields = vec![];
                        let mut statements = vec![];
                        let mut i = 0;
                        for s in &seq.segments {
                            let name = "arg_".to_string() + &i.to_string();
                            let s_rule_name = s.rule_name();
                            let field_name = syn::Ident::new(&name, s_rule_name.span());
                            let need_box = need_box.get(rule).unwrap().contains(s_rule_name);
                            let pfn = parser_fn(s_rule_name, &rules);
                            match s {
                                RuleRef::One(ident) => {
                                    statements.push(quote! {
                                        let (parser, #field_name) = parser.#pfn::<#ident>()?;
                                    });
                                    if need_box {
                                        fields.push(quote! {
                                            #field_name: Box<#ident<'a>>
                                        });
                                        constructor_fields.push(quote! {
                                            #field_name: Box::new(#field_name)
                                        });
                                    } else {
                                        fields.push(quote! {
                                            #field_name: #ident<'a>
                                        });
                                        constructor_fields.push(quote! {
                                            #field_name
                                        });
                                    }
                                }
                                RuleRef::OneOrMore(ident) => {
                                    let first_field_name = syn::Ident::new(
                                        &("arg_first_".to_string() + &i.to_string()),
                                        s_rule_name.span(),
                                    );
                                    if need_box {
                                        fields.push(quote! {
                                            #field_name: (Box<#ident<'a>>, Vec<#ident<'a>>)
                                        });
                                        constructor_fields.push(quote! {
                                            #field_name: (Box::new(#first_field_name), #field_name)
                                        });
                                    } else {
                                        fields.push(quote! {
                                            #field_name: (#ident<'a>, Vec<#ident<'a>>)
                                        });
                                        constructor_fields.push(quote! {
                                            #field_name: (#first_field_name, #field_name)
                                        });
                                    }
                                }
                                RuleRef::ZeroOrMore(ident) => {
                                    fields.push(quote! {
                                        #field_name: Vec<#ident<'a>>
                                    });
                                    constructor_fields.push(quote! {
                                        #field_name
                                    });
                                }
                                RuleRef::Optional(ident) => {
                                    if need_box {
                                        fields.push(quote! {
                                            #field_name: Option<Box<#ident<'a>>>
                                        });
                                        constructor_fields.push(quote! {
                                            #field_name: #field_name .map(Box::new)
                                        });
                                    } else {
                                        fields.push(quote! {
                                            #field_name: Option<#ident<'a>>
                                        });
                                        constructor_fields.push(quote! {
                                            #field_name
                                        });
                                    }
                                }
                            }
                            i += 1;
                        }
                        items.push(quote! {
                            struct #rule<'a> {
                                #(#fields),*
                            }
                            impl<'a> Parse for #rule<'a> {
                                type Error = Error;
                                type Parser = Parser<Lexer<'a>>;

                                fn parse(parser: Self::Parser) -> Result<(Self::Parser, Self), Self::Error> {
                                    #(#statements)*
                                    Ok((parser, #rule {
                                       #(#constructor_fields),*
                                    }))
                                }
                            }
                        })
                    }
                    SeqOrEnum::Enum(e) => {
                        let mut fields = vec![];
                        for s in &e.segments {
                            fields.push(quote! {
                                #s(#s<'a>)
                            });
                        }
                        items.push(quote! {
                            enum #rule<'a> {
                                #(#fields),*
                            }
                            impl<'a> Parse for #rule<'a> {
                                type Error = Error;
                                type Parser = Parser<Lexer<'a>>;

                                fn parse(cursor: Self::Parser) -> Result<(Self::Parser, Self), Self::Error> {
                                    todo!()
                                }
                            }
                        })
                    }
                },
            }
        }

        quote! {
            use lr::{Cursor, Parser, Parse, Peek, Span};
            use std::marker::PhantomData;

            enum Token<'a> {
                #(#tokens),*
            }
            #[derive(Clone)]
            struct Lexer<'a> {
                _a: PhantomData<&'a ()>
            }
            impl<'a> Cursor for Lexer<'a> {
                type Item = Token<'a>;

                fn next(&mut self) -> Option<Self::Item> {
                    todo!()
                }
                fn peek(&mut self) -> Option<&Self::Item> {
                    todo!()
                }
                fn position(&self) -> usize {
                    todo!()
                }
            }
            #(#items)*
        }
    };
    TokenStream::from(stream)
}

mod parse {
    use crate::{Enum, Grammer, Regex, Rule, RuleRef, RuleValue, Seq, SeqOrEnum, Token};
    use std::collections::HashMap;
    use syn::ext::IdentExt;
    use syn::parse::ParseStream;
    use syn::punctuated::Punctuated;
    use syn::spanned::Spanned;
    use syn::{bracketed, token, Error, LitStr};

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
                name: syn::Ident::parse_any(input)?,
                eq: input.parse()?,
                value: input.parse()?,
            })
        }
    }

    impl syn::parse::Parse for RuleValue {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let lookahead = input.lookahead1();
            if lookahead.peek(syn::Ident::peek_any) || lookahead.peek(token::Bracket) {
                input.parse().map(RuleValue::SeqOrEnum)
            } else {
                input.parse().map(RuleValue::Token)
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

            Ok(Enum { segments })
        }
    }

    impl syn::parse::Parse for Seq {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let mut segments = vec![];
            let first = input.parse()?;
            match first {
                RuleRef::One(_) | RuleRef::OneOrMore(_) => {}
                RuleRef::ZeroOrMore(ident) => {
                    return Err(Error::new(
                        ident.span(),
                        "Sequence can't start with an zero-or-more rule",
                    ))
                }
                RuleRef::Optional(ident) => {
                    return Err(Error::new(
                        ident.span(),
                        "Sequence can't start with an optional rule",
                    ))
                }
            }
            segments.push(first);

            while input.peek(syn::Ident::peek_any) || input.peek(token::Bracket) {
                segments.push(input.parse()?);
            }

            Ok(Seq { segments })
        }
    }

    impl syn::parse::Parse for RuleRef {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            if input.peek(token::Bracket) {
                let content;
                bracketed!(content in input);
                Ok(RuleRef::Optional(content.parse()?))
            } else {
                let first = input.parse()?;
                if input.peek(Token![+]) {
                    let _ = input.parse::<Token![+]>()?;
                    Ok(RuleRef::OneOrMore(first))
                } else if input.peek(Token![*]) {
                    let _ = input.parse::<Token![*]>()?;
                    Ok(RuleRef::ZeroOrMore(first))
                } else {
                    Ok(RuleRef::One(first))
                }
            }
        }
    }

    impl syn::parse::Parse for Token {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let lit: LitStr = input.parse()?;
            if lit.suffix() == "r" {
                Ok(Token::Regex(Regex { lit }))
            } else if lit.suffix().is_empty() {
                Ok(Token::String(lit))
            } else {
                Err(Error::new(
                    lit.span(),
                    format!("unknown suffix {}", lit.suffix()),
                ))
            }
        }
    }
}

mod implementation {
    use crate::{
        Enum, EnumPath, Grammer, Lookahead, LookaheadTree, RightTree, RuleRef,
        RuleValue, Seq, SeqOrEnum, Split, Token, TokenResult, TokenResult1,
    };
    use proc_macro::{Span, TokenStream};
    use quote::__private::Ident;
    use std::collections::{HashMap, HashSet};
    use std::fmt::{Debug, Formatter};
    use std::hash::{Hash, Hasher};
    use syn::punctuated::Punctuated;

    pub(super) fn calculate_need_box(
        recursives: &HashMap<syn::Ident, HashSet<syn::Ident>>,
        rules: &HashMap<syn::Ident, RuleValue>,
    ) -> HashMap<syn::Ident, HashSet<syn::Ident>> {
        let mut res = HashMap::new();
        for (rule, val) in rules {
            if let RuleValue::SeqOrEnum(_) = val {
                let mut contained_in = HashSet::new();
                for (other, recursive_in) in recursives {
                    if recursive_in.contains(rule) {
                        contained_in.insert(other.clone());
                    }
                }
                res.insert(rule.clone(), contained_in);
            } else {
                continue;
            }
        }
        res
    }

    pub(super) fn empty() -> syn::Ident {
        syn::Ident::new("", Span::call_site().into())
    }

    pub(super) fn get_right_name(
        ident: &syn::Ident,
        recursive_to: Option<&syn::Ident>,
        rules: &HashMap<syn::Ident, RuleValue>,
    ) -> syn::Ident {
        // TODO ensure there are no duplicates
        let postfix = if let Some(rt) = recursive_to {
            rt.to_string()
        } else {
            "".to_string()
        };
        let new_name = ident.to_string() + "Right" + &postfix;
        syn::Ident::new(&new_name, ident.span())
    }

    pub(super) fn get_left_name(
        ident: &syn::Ident,
        recursive_to: Option<&syn::Ident>,
        rules: &HashMap<syn::Ident, RuleValue>,
    ) -> syn::Ident {
        // TODO ensure there are no duplicates
        let postfix = if let Some(rt) = recursive_to {
            rt.to_string()
        } else {
            "".to_string()
        };
        let new_name = ident.to_string() + "Left" + &postfix;
        syn::Ident::new(&new_name, ident.span())
    }

    impl SeqOrEnum {
        pub(crate) fn is_empty(&self) -> bool {
            match self {
                SeqOrEnum::Seq(seq) => seq.segments.is_empty(),
                SeqOrEnum::Enum(e) => e.segments.is_empty(),
            }
        }
    }

    fn rule_first_definite_token<'a>(
        name: &'a syn::Ident,
        rules: &'a HashMap<syn::Ident, RuleValue>,
        visited: &mut HashSet<syn::Ident>,
    ) -> Option<&'a syn::Ident> {
        if !visited.insert(name.clone()) {
            return None;
        }

        let result = match rules.get(name)? {
            RuleValue::Token(_) => Some(name),
            RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => seq.first_definite_token(rules, visited),
            RuleValue::SeqOrEnum(SeqOrEnum::Enum(e)) => e.first_definite_token(rules, visited),
        };

        visited.remove(name);
        result
    }

    impl Grammer {
        pub fn seqs(&self) -> Vec<&Seq> {
            let mut seqs = vec![];
            for rule in &self.rules {
                match &rule.value {
                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => seqs.push(seq),
                    _ => {}
                }
            }
            seqs
        }

        pub fn enums(&self) -> Vec<&Enum> {
            let mut enums = vec![];
            for rule in &self.rules {
                match &rule.value {
                    RuleValue::SeqOrEnum(SeqOrEnum::Enum(e)) => enums.push(e),
                    _ => {}
                }
            }
            enums
        }

        pub fn tokens(&self) -> Vec<&Token> {
            let mut primitives = vec![];
            for rule in &self.rules {
                match &rule.value {
                    RuleValue::Token(token) => primitives.push(token),
                    _ => {}
                }
            }
            primitives
        }
    }

    impl RuleRef {
        pub fn is_one(&self) -> bool {
            match self {
                RuleRef::One(_) => true,
                _ => false,
            }
        }

        pub fn rule_name(&self) -> &syn::Ident {
            match self {
                RuleRef::One(i) => i,
                RuleRef::OneOrMore(i) => i,
                RuleRef::ZeroOrMore(i) => i,
                RuleRef::Optional(i) => i,
            }
        }
    }

    impl Seq {
        fn is_empty(&self) -> bool {
            self.segments.is_empty()
        }

        pub fn first_definite_token<'a>(
            &'a self,
            rules: &'a HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
        ) -> Option<&'a syn::Ident> {
            for segment in &self.segments {
                match segment {
                    RuleRef::One(ident) | RuleRef::OneOrMore(ident) => {
                        return rule_first_definite_token(ident, rules, visited);
                    }
                    RuleRef::ZeroOrMore(_) | RuleRef::Optional(_) => {
                        return None;
                    }
                }
            }
            None
        }

        pub fn calculate_need_box(
            &self,
            this: &syn::Ident,
            parent: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
            need_box: &mut HashMap<syn::Ident, HashSet<syn::Ident>>,
            visited: &mut HashSet<syn::Ident>,
        ) {
            for segment in &self.segments {
                let name = segment.rule_name();
                if name == parent {
                    match need_box.get_mut(this) {
                        None => {
                            need_box.insert(this.clone(), HashSet::from([parent.clone()]));
                        }
                        Some(s) => {
                            s.insert(parent.clone());
                        }
                    }
                }
                match rules.get(name).unwrap() {
                    RuleValue::Token(_) => {}
                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                        if visited.contains(name) {
                            continue;
                        }
                        visited.insert(name.clone());
                        /*if match need_box.get(this) {
                            None => true,
                            Some(set) => !set.contains(name),
                        } {*/
                        seq.calculate_need_box(name, parent, rules, need_box, visited);
                        //}
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Enum(e)) => {
                        if visited.contains(name) {
                            continue;
                        }
                        visited.insert(name.clone());
                        e.calculate_need_box(parent, rules, need_box, visited);
                    }
                }
            }
        }

        pub fn nth_can_be(
            &self,
            parent: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
            n: usize,
        ) -> Option<RuleRef> {
            match self.nth_can_be_helper(
                parent,
                rules,
                n,
                0,
                HashSet::from([0]),
                &mut HashSet::new(),
            ) {
                Some(Ok(res)) => Some(res),
                None | Some(Err(_)) => None,
            }
        }

        fn nth_can_be_helper(
            &self,
            parent: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
            n: usize,
            segment_index: usize,
            passed_variants: HashSet<usize>,
            parent_visited: &mut HashSet<syn::Ident>,
        ) -> Option<Result<RuleRef, Vec<usize>>> {
            if segment_index == self.segments.len() {
                return None;
            }
            let mut passed_for_next_segment = HashSet::new();
            for mut passed in passed_variants {
                let mut visited = parent_visited.clone();
                let segment = &self.segments[segment_index];
                let segment_name = segment.rule_name();
                if segment_name == parent && passed == n {
                    return Some(Ok(segment.clone()));
                }
                if visited.contains(segment_name) {
                    passed += 1;
                    if passed < n {
                        passed_for_next_segment.insert(passed);
                    }
                    continue;
                }
                visited.insert(segment_name.clone());
                match rules.get(segment_name).unwrap() {
                    RuleValue::Token(_) => {
                        passed += 1;
                        if passed < n {
                            passed_for_next_segment.insert(passed);
                        }
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                        match seq.nth_can_be_helper(
                            parent,
                            rules,
                            n,
                            0,
                            HashSet::from([passed]),
                            &mut visited,
                        ) {
                            None => {}
                            Some(Ok(res)) => return Some(Ok(res)),
                            Some(Err(other_variants)) => {
                                passed_for_next_segment.extend(other_variants);
                            }
                        }
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Enum(e)) => {
                        match e.nth_can_be_helper(
                            parent,
                            rules,
                            n,
                            HashSet::from([passed]),
                            &mut visited,
                        ) {
                            None => {}
                            Some(Ok(res)) => return Some(Ok(res)),
                            Some(Err(other_variants)) => {
                                passed_for_next_segment.extend(other_variants);
                            }
                        }
                    }
                }
            }
            if passed_for_next_segment.is_empty() {
                return None;
            } else {
                self.nth_can_be_helper(
                    parent,
                    rules,
                    n,
                    segment_index + 1,
                    passed_for_next_segment,
                    parent_visited,
                )
            }
        }

        pub fn leftest<'a>(&'a self, rules: &'a HashMap<syn::Ident, RuleValue>) -> &RuleRef {
            self.leftest_helper(rules, &mut HashSet::new())
        }

        fn leftest_helper<'a>(
            &'a self,
            rules: &'a HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
        ) -> &'a RuleRef {
            let first = &self.segments[0];
            let first_name = first.rule_name();
            if visited.contains(first_name) {
                return first;
            }
            visited.insert(first_name.clone());
            match rules.get(first_name).unwrap() {
                RuleValue::Token(_) => first,
                RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => seq.leftest_helper(rules, visited),
                // If an enum occurs we stop and return this enum
                RuleValue::SeqOrEnum(SeqOrEnum::Enum(_)) => first,
            }
        }

        pub fn calculate_nth_definite_token1<'a>(
            &'a self,
            rules: &'a HashMap<syn::Ident, RuleValue>,
            n: usize,
        ) -> Option<&'a syn::Ident> {
            match self.calculate_nth_definite_token1_helper(rules, n, &mut 0, &mut HashSet::new()) {
                TokenResult1::Some(tok) => Some(tok),
                TokenResult1::Eor | TokenResult1::None => None,
            }
        }
        pub fn calculate_nth_definite_token1_helper<'a>(
            &'a self,
            rules: &'a HashMap<syn::Ident, RuleValue>,
            n: usize,
            passed: &mut usize,
            parent_visited: &mut HashSet<syn::Ident>,
        ) -> TokenResult1<'a> {
            for segment in &self.segments {
                let mut visited = parent_visited.clone();
                let name = segment.rule_name();
                match rules.get(name).unwrap() {
                    RuleValue::Token(_) => {
                        if *passed == n {
                            return TokenResult1::Some(name);
                        }
                        // If this token has dynamic then we can't
                        // get a next definite token
                        match segment {
                            RuleRef::One(_) => {}
                            RuleRef::OneOrMore(_)
                            | RuleRef::ZeroOrMore(_)
                            | RuleRef::Optional(_) => return TokenResult1::None,
                        }
                        *passed += 1;
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                        let mut is_one_or_more = false;
                        match segment {
                            RuleRef::One(_) => {}
                            RuleRef::OneOrMore(_) => is_one_or_more = true,
                            RuleRef::ZeroOrMore(_) | RuleRef::Optional(_) => {
                                return TokenResult1::None
                            }
                        }
                        if visited.contains(name) {
                            return TokenResult1::None;
                        }
                        visited.insert(name.clone());
                        match seq.calculate_nth_definite_token1_helper(
                            rules,
                            n,
                            passed,
                            &mut visited,
                        ) {
                            TokenResult1::Some(tok) => return TokenResult1::Some(tok),
                            TokenResult1::None => return TokenResult1::None,
                            TokenResult1::Eor => {}
                        }
                        // If this sequence has dynamic length
                        // then we can't determine the next token
                        if is_one_or_more {
                            return TokenResult1::None;
                        }
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Enum(_)) => {
                        let token = match segment {
                            RuleRef::One(_) => {
                                rule_first_definite_token(name, rules, &mut visited)
                            }
                            RuleRef::OneOrMore(_) => {
                                if let Some(tok) =
                                    rule_first_definite_token(name, rules, &mut visited)
                                {
                                    if *passed == n {
                                        return TokenResult1::Some(tok);
                                    }
                                    *passed += 1;
                                }
                                return TokenResult1::None;
                            }
                            RuleRef::ZeroOrMore(_) | RuleRef::Optional(_) => None,
                        };
                        match token {
                            Some(tok) => {
                                if *passed == n {
                                    return TokenResult1::Some(tok);
                                }
                                *passed += 1;
                            }
                            None => return TokenResult1::None,
                        }
                    }
                }
            }
            TokenResult1::Eor
        }

        // Asserts that the leftest rule can only consist out of tokens and sequences
        pub fn lift_out_left_recursive(
            &self,
            parent: &syn::Ident,
            rules: &mut HashMap<syn::Ident, RuleValue>,
        ) -> Result<(RuleRef, Seq), TokenStream> {
            let leftest = self.leftest(rules).rule_name();
            debug_assert!(leftest == parent);
            // If the second rule can also be parent
            // we would have second order left recursiveness,
            // which is not allowed
            if let Some(s) = self.nth_can_be(parent, rules, 1) {
                return Err(err!(
                    s.rule_name().span(),
                    format!("Left recursiveness of second order is not supported!")
                ));
            }
            self.lift_out_left_recursive_helper(parent, rules, &mut HashSet::new())
        }
        fn lift_out_left_recursive_helper(
            &self,
            parent: &syn::Ident,
            rules: &mut HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
        ) -> Result<(RuleRef, Seq), TokenStream> {
            let first = &self.segments[0];
            let first_name = first.rule_name();
            if first_name == parent {
                Ok((
                    first.clone(),
                    Seq {
                        segments: (&self.segments[1..]).to_vec(),
                    },
                ))
            } else {
                if visited.contains(first_name) {
                    panic!("Found different cycle then the one to {}", parent);
                }
                visited.insert(first_name.clone());
                match rules.get(first_name).unwrap().clone() {
                    RuleValue::Token(_) => {
                        //unreachable!();
                        panic!("lel");
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Enum(_)) => {
                        return Err(err!(
                            first_name.span(),
                            format!("Sequences can't referenciate enums!")
                        ))
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                        let (parent_ref, child_right) =
                            seq.lift_out_left_recursive_helper(parent, rules, visited)?;
                        let new_segments = if !child_right.is_empty() {
                            let child_right_name =
                                get_right_name(first_name, Some(first_name), rules);
                            rules.insert(
                                child_right_name.clone(),
                                RuleValue::SeqOrEnum(SeqOrEnum::Seq(child_right)),
                            );
                            let mut segments = self.segments.clone();
                            segments[0] = match first {
                                RuleRef::One(_) => RuleRef::One(child_right_name),
                                RuleRef::OneOrMore(_) => RuleRef::OneOrMore(child_right_name),
                                RuleRef::ZeroOrMore(_) => RuleRef::ZeroOrMore(child_right_name),
                                RuleRef::Optional(_) => RuleRef::Optional(child_right_name),
                            };
                            segments
                        } else {
                            (&self.segments[1..]).to_vec()
                        };
                        Ok((
                            parent_ref,
                            Seq {
                                segments: new_segments,
                            },
                        ))
                    }
                }
            }
        }

        pub fn calculate_min_size(
            &self,
            rules: &HashMap<syn::Ident, RuleValue>,
            parent_visited: &mut HashSet<syn::Ident>,
        ) -> Option<usize> {
            let mut size = None;
            for rr in &self.segments {
                let mut visited = parent_visited.clone();
                let ident = rr.rule_name();
                match rr {
                    RuleRef::OneOrMore(ident) | RuleRef::One(ident) => {
                        let child_size = match rules.get(ident).unwrap() {
                            RuleValue::Token(_) => Some(1),
                            RuleValue::SeqOrEnum(soe) => {
                                if visited.contains(ident) {
                                    return None;
                                }
                                visited.insert(ident.clone());
                                match soe {
                                    SeqOrEnum::Seq(seq) => {
                                        seq.calculate_min_size(rules, &mut visited)
                                    }
                                    SeqOrEnum::Enum(e) => e.calculate_min_size(rules, &mut visited),
                                }
                            }
                        };
                        match child_size {
                            None => return None,
                            Some(c) => match &size {
                                None => size = Some(c),
                                Some(s) => {
                                    size = Some(s + c);
                                }
                            },
                        }
                    }
                    RuleRef::Optional(ident) | RuleRef::ZeroOrMore(ident) => match &size {
                        None => size = Some(0),
                        _ => {}
                    },
                }
            }
            size
        }

        pub fn calculate_nth_definite_token(
            &self,
            rules: &HashMap<syn::Ident, RuleValue>,
            n: usize,
        ) -> Option<syn::Ident> {
            match self.calculate_nth_definite_token_helper(rules, &mut HashSet::new(), n, &mut 0) {
                TokenResult::Some(ident) => Some(ident),
                _ => None,
            }
        }

        pub fn calculate_nth_definite_token_helper(
            &self,
            rules: &HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
            n: usize,
            passed: &mut usize,
        ) -> TokenResult {
            let mut iter = self.segments.iter();
            loop {
                match iter.next() {
                    None => return TokenResult::Eor,
                    Some(rr) => {
                        // If we have a cyclic structure than we can't calculate the next token
                        if visited.contains(rr.rule_name()) {
                            return TokenResult::None;
                        }
                        visited.insert(rr.rule_name().clone());
                        match rr {
                            RuleRef::One(ident) => match rules.get(ident).unwrap() {
                                RuleValue::Token(tok) => {
                                    if n == *passed {
                                        return TokenResult::Some(ident.clone());
                                    } else {
                                        *passed += 1;
                                    }
                                }
                                RuleValue::SeqOrEnum(soe) => {
                                    let child_res = match soe {
                                        SeqOrEnum::Seq(seq) => seq
                                            .calculate_nth_definite_token_helper(
                                                rules, visited, n, passed,
                                            ),
                                        SeqOrEnum::Enum(e) => e
                                            .calculate_nth_definite_token_helper(
                                                rules, visited, n, passed,
                                            ),
                                    };
                                    match child_res {
                                        TokenResult::Some(ident) => {
                                            return TokenResult::Some(ident)
                                        }
                                        TokenResult::Eor => {}
                                        TokenResult::None => return TokenResult::None,
                                    }
                                }
                            },
                            RuleRef::OneOrMore(ident) => match rules.get(ident).unwrap() {
                                RuleValue::Token(tok) => {
                                    if n == *passed {
                                        return TokenResult::Some(ident.clone());
                                    } else {
                                        // Because this ident has a modifier on it
                                        // we can't calculate tokens further this rule
                                        return TokenResult::None;
                                    }
                                }
                                RuleValue::SeqOrEnum(soe) => {
                                    let child_res = match soe {
                                        SeqOrEnum::Seq(seq) => seq
                                            .calculate_nth_definite_token_helper(
                                                rules, visited, n, passed,
                                            ),
                                        SeqOrEnum::Enum(e) => e
                                            .calculate_nth_definite_token_helper(
                                                rules, visited, n, passed,
                                            ),
                                    };
                                    match child_res {
                                        TokenResult::Some(ident) => {
                                            return TokenResult::Some(ident)
                                        }
                                        // Because this ident has a modifier on it
                                        // we can't calculate tokens further this rule
                                        _ => return TokenResult::None,
                                    }
                                }
                            },
                            _ => return TokenResult::None,
                        }
                    }
                }
            }
        }

        pub fn calculate_right_rule(
            &self,
            parent: &syn::Ident,
            rules: &mut HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
        ) -> Result<Option<Seq>, TokenStream> {
            // We can assert that the first rule is always non modified
            // assert let RuleRef::One(_) = &self.segments[0]
            if !self.leftest_can_be(parent, rules, &mut HashSet::new()) {
                return Ok(None);
            }
            let first = self.segments[0].rule_name();
            if first == parent {
                // Notice: new segments can be empty
                Ok(Some(Seq {
                    segments: (&self.segments[1..]).to_vec(),
                }))
            } else {
                // In this case we are in a child rule that itself is left recursive
                // So we return None because this rule will later be transformed
                if visited.contains(first) {
                    return Ok(None);
                }
                visited.insert(first.clone());
                match rules.get(first).unwrap().clone() {
                    RuleValue::Token(_) => panic!("In a left recursive rule, a token can't appear!"),
                    RuleValue::SeqOrEnum(soe) => match soe {
                        SeqOrEnum::Seq(child_seq) => {
                            if let Some(right_seq) = child_seq.calculate_right_rule(parent, rules, visited)? {

                                // Segments of this sequence right version
                                let mut new_segments = self.segments.clone();
                                if right_seq.segments.len() == 0 {
                                    // In this case the child consisted only of parent
                                    // Therefore we can simply remove the value in our right version
                                    new_segments.remove(0);
                                } else {
                                    let child_right_name = get_right_name(first, Some(parent), rules);
                                    // Update the existing rule to the normalized form
                                    // e.g: for child BinAp and parent Expr
                                    // BinAp = Expr Ident Expr
                                    // ->
                                    // BinApRight = Ident Expr // 1.
                                    // BinAp = Expr BinApRight // 2.

                                    // 1.
                                    rules.insert(child_right_name.clone(), RuleValue::SeqOrEnum(SeqOrEnum::Seq(right_seq)));
                                    // 2.
                                    rules.insert(first.clone(), RuleValue::SeqOrEnum(SeqOrEnum::Seq(Seq {
                                        segments: vec![RuleRef::One(parent.clone()), RuleRef::One(child_right_name.clone())]
                                    })));

                                    // We return our right version, so our old segments with the first token replaced by our childs right
                                    new_segments[0] = RuleRef::One(child_right_name);
                                }
                                Ok(Some(Seq {
                                    segments: new_segments
                                }))
                            } else {
                                panic!("This branch can never happen because we already asserted that the leftest value is the parent one!");
                            }
                        }
                        SeqOrEnum::Enum(_) =>
                            Err(err!(
                                first.span(),
                                format!("In left recursive rule graph, there cannot be a sequence followed by an enum!")
                            ))
                    }
                }
            }
        }

        pub fn leftest_can_be(
            &self,
            target: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
        ) -> bool {
            match &self.segments[0] {
                RuleRef::One(rule)
                | RuleRef::OneOrMore(rule)
                | RuleRef::Optional(rule)
                | RuleRef::ZeroOrMore(rule) => {
                    if visited.contains(rule) {
                        return false;
                    }
                    if rule == target {
                        true
                    } else {
                        visited.insert(rule.clone());
                        match rules.get(rule).unwrap() {
                            RuleValue::Token(_) => false,
                            RuleValue::SeqOrEnum(soe) => match soe {
                                SeqOrEnum::Seq(seq) => seq.leftest_can_be(target, rules, visited),
                                SeqOrEnum::Enum(e) => e.leftest_can_be(target, rules, visited),
                            },
                        }
                    }
                }
            }
        }

        pub fn contains(&self, parent: &syn::Ident) -> bool {
            self.segments.iter().any(|x| x.rule_name() == parent)
        }

        pub fn calculate_self_recursive_rules(
            &self,
            parent: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
            recursive_in: &mut HashSet<syn::Ident>,
        ) {
            for rr in &self.segments {
                let rule_name = rr.rule_name();
                if visited.contains(rule_name) {
                    continue;
                }
                match rules.get(rule_name).unwrap() {
                    RuleValue::Token(_) => {}
                    RuleValue::SeqOrEnum(soe) => match soe {
                        SeqOrEnum::Seq(seq) => {
                            if seq.contains(parent) {
                                recursive_in.insert(rule_name.clone());
                            }
                            visited.insert(rule_name.clone());
                            seq.calculate_self_recursive_rules(
                                parent,
                                rules,
                                visited,
                                recursive_in,
                            );
                        }
                        SeqOrEnum::Enum(e) => {
                            if e.contains(parent) {
                                recursive_in.insert(rule_name.clone());
                            }
                            visited.insert(rule_name.clone());
                            e.calculate_self_recursive_rules(parent, rules, visited, recursive_in);
                        }
                    },
                }
            }
        }

        /**
         * This function computes a version without the parent element on the left. It removes left recursiveness
         */
        pub fn calculate_right(
            &self,
            parent: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
            parent_rules: &mut HashMap<syn::Ident, SeqOrEnum>,
            mapping: &mut HashMap<syn::Ident, syn::Ident>,
        ) -> Result<Option<SeqOrEnum>, TokenStream> {
            match &self.segments[0] {
                RuleRef::One(rule) => {
                    if rule == parent {
                        Ok(Some(SeqOrEnum::Seq(Seq {
                            segments: (&self.segments[1..]).to_vec(),
                        })))
                    } else {
                        if mapping.contains_key(rule) {
                            let new_ident = mapping.get(rule).unwrap();
                            let mut new_segments = self.segments.clone();
                            new_segments[0] = RuleRef::One(new_ident.clone());
                            return Ok(Some(SeqOrEnum::Seq(Seq {
                                segments: new_segments,
                            })));
                        }
                        if let Some(res) = match rules.get(rule).unwrap() {
                            RuleValue::Token(_) => None,
                            RuleValue::SeqOrEnum(soe) => match soe {
                                SeqOrEnum::Seq(seq) => {
                                    seq.calculate_right(parent, rules, parent_rules, mapping)?
                                }
                                SeqOrEnum::Enum(e) => {
                                    e.calculate_right(parent, rules, parent_rules, mapping)?
                                }
                            },
                        } {
                            let new_ident = get_right_name(rule, Some(parent), rules);
                            let mut new_segments = self.segments.clone();
                            new_segments[0] = RuleRef::One(new_ident.clone());
                            parent_rules.insert(new_ident, res);
                            Ok(Some(SeqOrEnum::Seq(Seq {
                                segments: new_segments,
                            })))
                        } else {
                            Ok(None)
                        }
                    }
                }
                RuleRef::OneOrMore(rule) | RuleRef::Optional(rule) | RuleRef::ZeroOrMore(rule) => {
                    // Check if the leftest is the parent rule.
                    // So if we have left recursiveness with a frequency modifier applied
                    // Then we should throw an error
                    if match rules.get(rule).unwrap() {
                        RuleValue::Token(_) => false,
                        RuleValue::SeqOrEnum(soe) => match soe {
                            SeqOrEnum::Seq(seq) => {
                                seq.leftest_can_be(parent, rules, &mut HashSet::new())
                            }
                            SeqOrEnum::Enum(e) => {
                                e.leftest_can_be(parent, rules, &mut HashSet::new())
                            }
                        },
                    } {
                        Err(err!(rule.span(), format!("Rule {} cannot be left recursive because child has frequency modifier applied!", parent)))
                    } else {
                        Ok(None)
                    }
                }
            }
        }

        /**
         * This function calculates a split of the sequence in right and left on the given parent element
         * for left recursiveness. This is used to parse left recursive rules as follows:
         * Expr = ...;
         * ->
         * Expr = ExprLeft ExprRight+;
         */
        pub fn calculate_split(
            &self,
            parent: &syn::Ident,
            rules: &mut HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
        ) -> Result<Split, TokenStream> {
            let first = &self.segments[0];
            let first_name = first.rule_name();
            if first_name == parent {
                // Check if the next leftest can be also parent
                // Then we return an error because left recursiveness of
                // second order is not supported yet
                if self.segments.len() > 1 {
                    let second = self.segments[1].rule_name();
                    if match rules.get(second).unwrap() {
                        RuleValue::Token(_) => false,
                        RuleValue::SeqOrEnum(soe) => match soe {
                            SeqOrEnum::Seq(seq) => {
                                seq.leftest_can_be(parent, rules, &mut HashSet::new())
                            }
                            SeqOrEnum::Enum(e) => {
                                e.leftest_can_be(parent, rules, &mut HashSet::new())
                            }
                        },
                    } {
                        return Err(err!(
                            second.span(),
                            format!("Left recursiveness of second order not supported!")
                        ));
                    }
                }

                // Calculate the right side that doesn't contain parent
                let right = SeqOrEnum::Seq(Seq {
                    segments: (&self.segments[1..]).to_vec(),
                });

                // Calculate a potential left rule if the
                // parent rule is optional
                let left = match first {
                    RuleRef::One(_) => None,
                    RuleRef::Optional(_) => Some(right.clone()),
                    RuleRef::ZeroOrMore(ident) | RuleRef::OneOrMore(ident) =>
                        return Err(err!(
                            ident.span(),
                            format!("Left recursive rule can't be modified in quantity, because it is ambitous and can be removed!")
                        ))
                };
                // Notice: new segments can be empty
                Ok(match left {
                    None => Split::Right(right),
                    Some(left) => Split::Both(left, right),
                })
            } else {
                if visited.contains(first_name) {
                    // In this case we are at a branch that is not left recursive to
                    // the parent element but left recursive to first_name.
                    // In this case we return this rule as a potential left rule.
                    return Ok(Split::None);
                }
                visited.insert(first_name.clone());

                // Next we get the rule of the first because it is not the parent
                match rules.get(first_name).unwrap().clone() {
                    RuleValue::Token(_) => {
                        // In this case we simply return the sequence as it is
                        // as a left rule
                        return Ok(Split::None);
                    }
                    RuleValue::SeqOrEnum(soe) => {
                        // If the first rule is a sequence or enum
                        // then we currently prohibit quantity modifiers
                        match first {
                            RuleRef::One(_) => {},
                            RuleRef::Optional(ident) => return Err(err!(
                                ident.span(),
                                format!("Can't use optional here because {} can contain {} on the left side!", ident, parent)
                            )),
                            RuleRef::OneOrMore(ident) | RuleRef::ZeroOrMore(ident) =>
                                return Err(err!(
                                    ident.span(),
                                    format!("This rule can contains {} on the left side. \
                                    Therefore it is left recursive and can't modified by quantity! \
                                    Hint: Remove * or +", parent)
                            ))
                        }

                        // Calculate the split of the first token recursively
                        let split = match soe {
                            SeqOrEnum::Seq(seq) => seq.calculate_split(parent, rules, visited)?,
                            SeqOrEnum::Enum(e) => e.calculate_split(parent, rules, visited)?,
                        };

                        // Depending on the split we build the new left and right side
                        // For this sequence
                        match split {
                            Split::None => {
                                debug_assert!(first.is_one());
                                // If the first rule is just a left rule then
                                // we have no recursiveness and therefore we can return ourselves
                                Ok(Split::None)
                            }
                            Split::Right(right) => {
                                debug_assert!(first.is_one());
                                if !right.is_empty() {
                                    // First create a new name for the new right rule
                                    // and insert the new right rule
                                    let new_right_name =
                                        get_right_name(first_name, Some(parent), rules);
                                    rules.insert(
                                        new_right_name.clone(),
                                        RuleValue::SeqOrEnum(right),
                                    );

                                    // Now create the new right side of this sequence
                                    let mut new_right_segments = self.segments.clone();
                                    new_right_segments[0] = RuleRef::One(new_right_name);
                                    let new_right = SeqOrEnum::Seq(Seq {
                                        segments: new_right_segments,
                                    });

                                    // Modifiers can currently not applied on rules that contain
                                    // the parent rule on the left side
                                    // In the future we could introduce this by returning multiple left sides
                                    // and introduce intermediate enum creation
                                    // here we would differentiate between the modfiers and return appropriately

                                    // This is just the case for RuleRef::One
                                    Ok(Split::Right(new_right))
                                } else {
                                    // If the childs right is empty then we just omit the child
                                    Ok(Split::Right(SeqOrEnum::Seq(Seq {
                                        segments: (&self.segments[1..]).to_vec(),
                                    })))
                                }
                            }
                            Split::Both(left, right) => {
                                debug_assert!(first.is_one());

                                // We calculate the new left side of this sequence
                                let new_left = if !left.is_empty() {
                                    // Get a new name for the first child and insert into rules
                                    let new_left_name =
                                        get_left_name(first_name, Some(parent), rules);
                                    rules.insert(new_left_name.clone(), RuleValue::SeqOrEnum(left));

                                    // Build the new left side of this sequence by changing the first rule
                                    // with the new left rule of the child
                                    let mut new_left_segments = self.segments.clone();
                                    new_left_segments[0] = RuleRef::One(new_left_name);

                                    Seq {
                                        segments: new_left_segments,
                                    }
                                } else {
                                    // If the new left side is empty just omit the rule
                                    Seq {
                                        segments: (&self.segments[1..]).to_vec(),
                                    }
                                };

                                // Do the same for the right side
                                let new_right = if !right.is_empty() {
                                    let new_right_name =
                                        get_right_name(first_name, Some(parent), rules);
                                    rules.insert(
                                        new_right_name.clone(),
                                        RuleValue::SeqOrEnum(right),
                                    );

                                    let mut new_right_segments = self.segments.clone();
                                    new_right_segments[0] = RuleRef::One(new_right_name);

                                    Seq {
                                        segments: new_right_segments,
                                    }
                                } else {
                                    // If the new right side is empty just omit the rule
                                    Seq {
                                        segments: (&self.segments[1..]).to_vec(),
                                    }
                                };

                                Ok(Split::Both(
                                    SeqOrEnum::Seq(new_left),
                                    SeqOrEnum::Seq(new_right),
                                ))
                            }
                        }
                    }
                }
            }
        }
    }

    impl Enum {
        pub fn first_definite_token<'a>(
            &'a self,
            rules: &'a HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
        ) -> Option<&'a syn::Ident> {
            let mut common: Option<&'a syn::Ident> = None;
            for variant in &self.segments {
                let mut local = visited.clone();
                if let Some(tok) = rule_first_definite_token(variant, rules, &mut local) {
                    match common {
                        None => common = Some(tok),
                        Some(existing) if existing == tok => {}
                        _ => return None,
                    }
                }
            }
            common
        }

        pub fn calculate_need_box(
            &self,
            parent: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
            need_box: &mut HashMap<syn::Ident, HashSet<syn::Ident>>,
            visited: &mut HashSet<syn::Ident>,
        ) {
            for variant in &self.segments {
                if visited.contains(variant) {
                    continue;
                }
                visited.insert(variant.clone());
                match rules.get(variant).unwrap() {
                    RuleValue::Token(_) => {}
                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                        seq.calculate_need_box(variant, parent, rules, need_box, visited);
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Enum(e)) => {
                        e.calculate_need_box(parent, rules, need_box, visited);
                    }
                }
            }
        }

        pub fn calculate_lefts_and_rights(
            &self,
            this: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
        ) -> Result<(HashMap<Ident, EnumPath>, HashMap<Ident, EnumPath>), TokenStream> {
            self.calculate_lefts_and_rights_helper(this, rules, &mut vec![this.clone()])
        }
        pub fn calculate_lefts_and_rights_helper(
            &self,
            parent: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
            visited: &mut Vec<syn::Ident>,
        ) -> Result<(HashMap<Ident, EnumPath>, HashMap<Ident, EnumPath>), TokenStream> {
            let mut lefts = HashMap::new();
            let mut rights = HashMap::new();
            for variant in &self.segments {
                match rules.get(variant).unwrap() {
                    RuleValue::Token(_) => {
                        lefts.insert(variant.clone(), EnumPath {
                            constructors: visited.clone(),
                            seq_or_token: variant.clone(),
                        });
                    },
                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                        let leftest = seq.leftest(rules);
                        let path = EnumPath {
                            constructors: visited.clone(),
                            seq_or_token: variant.clone(),
                        };
                        // If the first rule of this sequence is the parent
                        // then we have to add this rule to the rights
                        // of this enum
                        if leftest.rule_name() == parent {
                            match leftest {
                                RuleRef::ZeroOrMore(_) | RuleRef::Optional(_) => {
                                    lefts.insert(variant.clone(), path.clone());
                                }
                                _ => {}
                            }
                            rights.insert(variant.clone(), path);
                        } else {
                            lefts.insert(variant.clone(), path);
                        }
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Enum(e)) => {
                        if visited.contains(variant) {
                            return Err(err!(
                                variant.span(),
                                format!("Recursive enums are not allowed!")
                            ));
                        }
                        visited.push(variant.clone());
                        let (mut child_lefts, mut child_rights) =
                            e.calculate_lefts_and_rights_helper(parent, rules, visited)?;
                        visited.pop();
                        lefts.extend(child_lefts.into_iter());
                        rights.extend(child_rights.into_iter());
                    }
                }
            }
            Ok((lefts, rights))
        }

        pub fn calculate_lookahead1(
            lefts: &HashMap<syn::Ident, EnumPath>,
            rights: &HashMap<syn::Ident, EnumPath>,
            rules: &HashMap<syn::Ident, RuleValue>,
        ) -> Result<Lookahead, TokenStream> {
            let left_names = lefts.iter().map(|(_, x)| x.seq_or_token.clone()).collect();
            let lefts_lookahead = Self::calculate_lookahead1_helper(left_names, &HashMap::new(), rules, 0)?;
            let mut right_names = vec![];
            let mut full_names = HashMap::new();
            for (_, right) in rights {
                match rules.get(&right.seq_or_token).unwrap() {
                    RuleValue::Token(_) | RuleValue::SeqOrEnum(SeqOrEnum::Enum(_)) => {
                        unreachable!()
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                        // We can assert that the length of a right sequence is 2
                        // because the left recursive element was lifted out
                        debug_assert!(seq.segments.len() == 2);
                        debug_assert!(seq.segments[1].is_one());
                        let right_name = seq.segments[1].rule_name();
                        right_names.push(right_name.clone());
                        full_names.insert(right_name.clone(), right.seq_or_token.clone());
                    }
                }
            }
            let right_lookahead = Self::calculate_lookahead1_helper(right_names, &full_names, rules, 0)?;
            Ok(Lookahead {
                lefts: lefts_lookahead,
                rights: right_lookahead,
            })
        }

        fn calculate_lookahead1_helper(
            group: Vec<syn::Ident>,
            full_names: &HashMap<syn::Ident, syn::Ident>,
            rules: &HashMap<syn::Ident, RuleValue>,
            depth: usize,
        ) -> Result<HashMap<syn::Ident, LookaheadTree>, TokenStream> {
            let mut grouped = HashMap::new();
            for name in group {
                match rules.get(&name).unwrap() {
                    RuleValue::SeqOrEnum(SeqOrEnum::Enum(_)) => {
                        panic!("Can't calculate lookahead on embedded enums!")
                    }
                    RuleValue::Token(_) => match grouped.get_mut(&name) {
                        None => {
                            grouped.insert(name.clone(), vec![name]);
                        }
                        Some(group) => {
                            group.push(name);
                        }
                    },
                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                        match seq.calculate_nth_definite_token1(rules, depth) {
                            None => {
                                return Err(err!(
                                    name.span(),
                                    format!("This rule doesn't offer {} unique tokens!", depth + 1)
                                ))
                            }
                            Some(next) => match grouped.get_mut(next) {
                                None => {
                                    grouped.insert(next.clone(), vec![name.clone()]);
                                }
                                Some(group) => {
                                    group.push(name.clone());
                                }
                            },
                        }
                    }
                }
            }
            let mut res = HashMap::new();
            for (name, group) in grouped {
                if group.len() == 1 {
                    let mut leaf_name = group[0].clone();
                    if let Some(name) = full_names.get(&leaf_name) {
                        leaf_name = name.clone();
                    }
                    res.insert(name, LookaheadTree::Leaf(leaf_name));
                } else {
                    res.insert(
                        name,
                        LookaheadTree::Node(Self::calculate_lookahead1_helper(
                            group,
                            full_names,
                            rules,
                            depth + 1,
                        )?),
                    );
                }
            }
            Ok(res)
        }

        fn nth_can_be_helper(
            &self,
            parent: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
            n: usize,
            parent_passed: HashSet<usize>,
            parent_visited: &mut HashSet<syn::Ident>,
        ) -> Option<Result<RuleRef, HashSet<usize>>> {
            let mut passed_variants = HashSet::new();
            for mut passed in parent_passed {
                for variant in &self.segments {
                    if variant == parent && n == passed {
                        return Some(Ok(RuleRef::One(variant.clone())));
                    }
                    let mut visited = parent_visited.clone();
                    if visited.contains(variant) {
                        passed += 1;
                        if passed < n {
                            passed_variants.insert(passed);
                        }
                        continue;
                    }
                    visited.insert(variant.clone());
                    match rules.get(variant).unwrap() {
                        RuleValue::Token(_) => {
                            passed += 1;
                            if passed < n {
                                passed_variants.insert(passed);
                            }
                        }
                        RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                            match seq.nth_can_be_helper(
                                parent,
                                rules,
                                n,
                                0,
                                HashSet::from([passed]),
                                &mut visited,
                            ) {
                                None => {}
                                Some(Ok(res)) => return Some(Ok(res)),
                                Some(Err(child_variants)) => {
                                    passed_variants.extend(child_variants);
                                }
                            }
                        }
                        RuleValue::SeqOrEnum(SeqOrEnum::Enum(e)) => {
                            match e.nth_can_be_helper(
                                parent,
                                rules,
                                n,
                                HashSet::from([passed]),
                                &mut visited,
                            ) {
                                None => {}
                                Some(Ok(res)) => return Some(Ok(res)),
                                Some(Err(child_variants)) => {
                                    passed_variants.extend(child_variants);
                                }
                            }
                        }
                    }
                }
            }
            if passed_variants.is_empty() {
                None
            } else {
                Some(Err(passed_variants))
            }
        }

        pub fn calculate_min_size(
            &self,
            rules: &HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
        ) -> Option<usize> {
            let mut min = None;
            for ident in &self.segments {
                match rules.get(ident).unwrap() {
                    RuleValue::Token(_) => {
                        // The minimum size can not be less 1
                        min = Some(1);
                    }
                    RuleValue::SeqOrEnum(soe) => {
                        if visited.contains(ident) {
                            continue;
                        }
                        visited.insert(ident.clone());
                        let child_size = match soe {
                            SeqOrEnum::Seq(seq) => seq.calculate_min_size(rules, visited),
                            SeqOrEnum::Enum(e) => e.calculate_min_size(rules, visited),
                        };
                        if let Some(size) = child_size {
                            match &min {
                                None => min = Some(size),
                                Some(m) => {
                                    if size < *m {
                                        min = Some(size);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            min
        }

        pub fn calculate_nth_definite_token(
            &self,
            rules: &HashMap<syn::Ident, RuleValue>,
            n: usize,
        ) -> Option<syn::Ident> {
            match self.calculate_nth_definite_token_helper(rules, &mut HashSet::new(), n, &mut 0) {
                TokenResult::Some(ident) => Some(ident),
                _ => None,
            }
        }

        // returns the ident of the nth definite token of a rule.
        // The term definite is best explained by counter example.
        // The following sequence doesn't have a definite second token:
        // Test = Number Ident* Number
        // because Ident has a modifier on it the second token can be a Number or an Ident.
        // Therefore it is not definite and we would return None in this case.
        // On the other side the first token is always parsed and therefore definite.
        // This function is required to calculate the lookahead of a rule. The lookahead
        // is a sequence of token-types that identifies a specific rule clearly.
        // In other words, if you look up this token sequence in the upcoming parse stream, you can
        // say for sure which rule you have to parse. This is especially importanted for enums.
        // Enums define a clear set of rules that can follow. To determine the one rule out of this set
        // which needs to be parsed, we have to lookahead in the parse stream.
        pub fn calculate_nth_definite_token_helper(
            &self,
            rules: &HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
            n: usize,
            passed: &mut usize,
        ) -> TokenResult {
            let mut tok = None;
            for ident in &self.segments {
                // If we have a cyclic structure than we can't calculate the next token
                if visited.contains(ident) {
                    return TokenResult::None;
                }
                visited.insert(ident.clone());
                match rules.get(ident).unwrap() {
                    RuleValue::Token(tok) => {
                        if n == *passed {
                            return TokenResult::Some(ident.clone());
                        } else {
                            // After a token there can't be anything
                            return TokenResult::None;
                        }
                    }
                    RuleValue::SeqOrEnum(soe) => {
                        let child_res = match soe {
                            SeqOrEnum::Seq(seq) => {
                                seq.calculate_nth_definite_token_helper(rules, visited, n, passed)
                            }
                            SeqOrEnum::Enum(e) => {
                                e.calculate_nth_definite_token_helper(rules, visited, n, passed)
                            }
                        };
                        match child_res {
                            TokenResult::Some(ident) => {
                                if let Some(i) = &tok {
                                    if i != &ident {
                                        return TokenResult::None;
                                    }
                                } else {
                                    tok = Some(ident);
                                }
                            }
                            _ => return TokenResult::None,
                        }
                    }
                }
            }
            // we found on all childs the same next token
            // therefore this next token is definite
            if let Some(ident) = tok {
                TokenResult::Some(ident)
            } else {
                panic!("Empty enums can't exist!")
            }
        }

        pub fn leftest_can_be(
            &self,
            target: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
        ) -> bool {
            for s in &self.segments {
                if visited.contains(s) {
                    continue;
                }
                if match rules.get(s).unwrap() {
                    RuleValue::Token(_) => false,
                    RuleValue::SeqOrEnum(soe) => match soe {
                        SeqOrEnum::Seq(seq) => seq.leftest_can_be(target, rules, visited),
                        SeqOrEnum::Enum(e) => e.leftest_can_be(target, rules, visited),
                    },
                } {
                    return true;
                }
            }
            false
        }

        pub fn contains(&self, parent: &syn::Ident) -> bool {
            self.segments.iter().any(|x| x == parent)
        }

        pub fn calculate_lookahead(
            &self,
            rules: &HashMap<syn::Ident, RuleValue>,
        ) -> Result<HashMap<syn::Ident, LookaheadTree>, TokenStream> {
            Self::calculate_lookahead_helper(rules, &self.segments.clone().into_iter().collect(), 0)
        }

        // Returns for every unique token a rule that can be constructed
        fn calculate_lookahead_helper(
            rules: &HashMap<syn::Ident, RuleValue>,
            to_split: &Vec<syn::Ident>,
            depth: usize,
        ) -> Result<HashMap<syn::Ident, LookaheadTree>, TokenStream> {
            // Group the split by the next definite token
            let mut grouped = HashMap::new();
            for ident in to_split {
                match rules.get(ident).unwrap() {
                    RuleValue::Token(tok) => {
                        if grouped.contains_key(ident) {
                            return Err(err!(
                                ident.span(),
                                format!(
                                    "When having a token in an enum it must differ from all other rules! \n\
                                    Does therefore a different rule start with the same token?"
                                )
                            ));
                        } else {
                            grouped.insert(ident.clone(), vec![ident.clone()]);
                        }
                    }
                    RuleValue::SeqOrEnum(soe) => {
                        let next = match soe {
                            SeqOrEnum::Seq(seq) => {
                                match seq.calculate_nth_definite_token(rules, depth) {
                                    None => return Err(err!(
                                        ident.span(),
                                        format!("Can't calculate unique lookahead for this token! Does a different rule start with the same token?")
                                    )),
                                    Some(next) => next,
                                }
                            }
                            SeqOrEnum::Enum(e) => {
                                match e.calculate_nth_definite_token(rules, depth) {
                                    None => return Err(err!(
                                        ident.span(),
                                        format!("Can't calculate unique lookahead for this token! Does a different rule start with the same token?")
                                    )),
                                    Some(next) => next,
                                }
                            }
                        };
                        match grouped.get_mut(&next) {
                            None => {
                                grouped.insert(next, vec![ident.clone()]);
                            }
                            Some(group) => {
                                group.push(ident.clone());
                            }
                        }
                    }
                }
            }

            let mut res = HashMap::new();
            for (tok, group) in grouped {
                let tree = if group.len() == 1 {
                    LookaheadTree::Leaf(group.first().unwrap().clone())
                } else {
                    let sub_tree = Self::calculate_lookahead_helper(rules, &group, depth + 1)?;
                    LookaheadTree::Node(sub_tree)
                };
                res.insert(tok, tree);
            }
            Ok(res)
        }

        // Expr = BinAp | Ap;
        // BinAp = Test Expr;
        // Test = Expr Ident;
        // Ap = Ident LeftBracket args RightBracket;
        //
        // calculate_right_side_split(Expr):
        // Test -> Ident;
        //
        // When generating a parser for Expr then we have to
        // consider the right side split
        //
        // Additionally we have to check if a rule contains second order left recursiveness
        // e.g: Expr = Expr Expr Ident;
        // We can't allow that
        //
        // The new right sides could addtionally be left recursive
        // E.g:
        // Expr = BinAp;
        // BinAp = Expr BinAp Ident;
        // After split:
        //
        // BinAp -> BinAp Ident;
        //
        /*
         * This function should add and transform the rules in a way so that for every left recursive rule a new rule is inserted that forms the right split:
         * E.g:
         * Expr = BinAp | Ap;
         * BinAp = Test Expr;
         * Test = Expr Ident;
         * Ap = Ident LeftBracket args RightBracket;
         * -> Transformed to:
         * Expr = BinAp | Ap; + map(BinAp -> BinApRight, Test -> TestRight)
         * BinAp = Expr BinApRight;
         * BinApRight = TestRight Expr;
         * Test = Expr TestRight;
         * TestRight = Ident;
         * Ap = Ident LeftBracket args RightBracket;
         *
         * Expr = Ap;
         * Ap = BinAp | UnAp;
         * UnAp = Ident Expr;
         * BinAp = Expr Ident Expr;
         * ->
         * Expr = Ap; + map(Ap -> BinAp -> BinApRight)
         * Ap = BinAp | UnAp;
         * UnAp = Ident Expr;
         * BinAp = Expr BinApRight;
         * BinApRight = Ident Expr;
         *
         * This function should do this process for the given parent rule
         */
        pub fn calculate_right_rule_tree(
            &self,
            parent: &syn::Ident,
            rules: &mut HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
        ) -> Result<Option<RightTree>, TokenStream> {
            let mut right = HashMap::new();
            for child in &self.segments {
                if visited.contains(child) {
                    continue;
                }
                visited.insert(child.clone());
                if let Some(r) = match rules.get(child).unwrap().clone() {
                    RuleValue::Token(_) => None,
                    RuleValue::SeqOrEnum(soe) => match soe {
                        SeqOrEnum::Seq(seq) => {
                            if let Some(new_right_seq) =
                                seq.calculate_right_rule(parent, rules, &mut HashSet::new())?
                            {
                                let child_right_name = get_right_name(child, Some(parent), rules);

                                // Update this child rule with the new right rule
                                // See also calculate_right_rule
                                rules.insert(
                                    child.clone(),
                                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(Seq {
                                        segments: vec![
                                            RuleRef::One(parent.clone()),
                                            RuleRef::One(child_right_name.clone()),
                                        ],
                                    })),
                                );

                                // Insert the new right rule into rules
                                rules.insert(
                                    child_right_name.clone(),
                                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(new_right_seq)),
                                );

                                Some(RightTree::Leaf(child_right_name))
                            } else {
                                None
                            }
                        }
                        SeqOrEnum::Enum(e) => {
                            // In this case we have an embedded enum in this enum
                            e.calculate_right_rule_tree(parent, rules, visited)?
                        }
                    },
                } {
                    right.insert(child.clone(), r);
                }
            }
            // If there are no entries, in other words we didn't found any rule that needs a transform,
            // we can simply return None
            if right.is_empty() {
                Ok(None)
            } else {
                Ok(Some(RightTree::Node(right)))
            }
        }

        /**
         * Calculates a set of rule names that contain the given rule: @param parent
         * If this function is called on a rule itself then this function returns the set of rules
         * that the rule is recursive in.
         */
        pub fn calculate_self_recursive_rules(
            &self,
            parent: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
            recursive_in: &mut HashSet<syn::Ident>,
        ) {
            for rule_name in &self.segments {
                if visited.contains(rule_name) {
                    continue;
                }
                match rules.get(rule_name).unwrap() {
                    RuleValue::Token(_) => {}
                    RuleValue::SeqOrEnum(soe) => match soe {
                        SeqOrEnum::Seq(seq) => {
                            if seq.contains(parent) {
                                recursive_in.insert(rule_name.clone());
                            }
                            visited.insert(rule_name.clone());
                            seq.calculate_self_recursive_rules(
                                parent,
                                rules,
                                visited,
                                recursive_in,
                            );
                        }
                        SeqOrEnum::Enum(e) => {
                            if e.contains(parent) {
                                recursive_in.insert(rule_name.clone());
                            }
                            visited.insert(rule_name.clone());
                            e.calculate_self_recursive_rules(parent, rules, visited, recursive_in);
                        }
                    },
                }
            }
        }

        pub fn calculate_right(
            &self,
            parent: &syn::Ident,
            rules: &HashMap<syn::Ident, RuleValue>,
            parent_rules: &mut HashMap<syn::Ident, SeqOrEnum>,
            mapping: &mut HashMap<syn::Ident, syn::Ident>,
        ) -> Result<Option<SeqOrEnum>, TokenStream> {
            let mut new_segments = vec![];
            let mut left_recursive = false;
            for rule in &self.segments {
                if rule == parent {
                    left_recursive = true;
                    continue;
                }
                if mapping.contains_key(rule) {
                    let new_ident = mapping.get(rule).unwrap();
                    new_segments.push(new_ident.clone());
                    left_recursive = true;
                    continue;
                }

                if let Some(res) = match rules.get(rule).unwrap() {
                    RuleValue::Token(_) => None,
                    RuleValue::SeqOrEnum(soe) => match soe {
                        SeqOrEnum::Seq(seq) => {
                            seq.calculate_right(parent, rules, parent_rules, mapping)?
                        }
                        SeqOrEnum::Enum(e) => {
                            e.calculate_right(parent, rules, parent_rules, mapping)?
                        }
                    },
                } {
                    let new_ident = get_right_name(rule, Some(parent), rules);
                    mapping.insert(rule.clone(), new_ident.clone());
                    parent_rules.insert(new_ident.clone(), res);
                    new_segments.push(new_ident);
                    left_recursive = true;
                }
            }
            if !left_recursive {
                Ok(None)
            } else {
                Ok(Some(SeqOrEnum::Enum(Enum {
                    segments: Punctuated::from_iter(new_segments.into_iter()),
                })))
            }
        }

        pub fn calculate_split(
            &self,
            parent: &syn::Ident,
            rules: &mut HashMap<syn::Ident, RuleValue>,
            visited: &mut HashSet<syn::Ident>,
        ) -> Result<Split, TokenStream> {
            let mut lefts = HashSet::new();
            let mut rights = HashSet::new();

            for child in &self.segments {
                if child == parent {
                    // In this case we have to add a special empty
                    // rule to the right
                    let empty = empty();
                    rules.insert(
                        empty.clone(),
                        RuleValue::SeqOrEnum(SeqOrEnum::Seq(Seq { segments: vec![] })),
                    );
                    rights.insert(empty);
                } else {
                    if visited.contains(child) {
                        // In this case we are at a branch that is not left recursive to
                        // the parent element but left recursive to first_name.
                        // In this case we return this rule as a potential left rule.
                        lefts.insert(child.clone());
                        continue;
                    }
                    visited.insert(child.clone());

                    match rules.get(child).unwrap().clone() {
                        RuleValue::Token(_) => {
                            // If the variant is just a token
                            // Then it is a left side
                            lefts.insert(child.clone());
                        }
                        RuleValue::SeqOrEnum(soe) => {
                            // If the variant is a sequence or an enum we
                            // have to perform a split
                            let split = match soe {
                                SeqOrEnum::Seq(seq) => {
                                    seq.calculate_split(parent, rules, visited)?
                                }
                                SeqOrEnum::Enum(e) => e.calculate_split(parent, rules, visited)?,
                            };

                            match split {
                                Split::None => {
                                    // If the variant is just a left rule
                                    // we don't have to rename it nor save the returned variant
                                    // because it wasn't transformed and is already in the rules map.
                                    lefts.insert(child.clone());
                                }
                                Split::Right(right) => {
                                    if !right.is_empty() {
                                        // If the variant is a right rule
                                        // we have to save the new right rule in the rules.
                                        let new_right_name =
                                            get_right_name(child, Some(parent), rules);
                                        rules.insert(
                                            new_right_name.clone(),
                                            RuleValue::SeqOrEnum(right),
                                        );

                                        rights.insert(new_right_name);
                                    } else {
                                        // If the rule is empty then we have to insert
                                        // the empty rule to the right side
                                        let empty = empty();
                                        rules.insert(
                                            empty.clone(),
                                            RuleValue::SeqOrEnum(SeqOrEnum::Seq(Seq {
                                                segments: vec![],
                                            })),
                                        );
                                        rights.insert(empty);
                                    }
                                }
                                Split::Both(left, right) => {
                                    // If we have a split then we have to
                                    // add the new left rule and the new right rule

                                    if !left.is_empty() {
                                        let new_left_name =
                                            get_left_name(child, Some(parent), rules);
                                        rules.insert(
                                            new_left_name.clone(),
                                            RuleValue::SeqOrEnum(left),
                                        );

                                        lefts.insert(new_left_name);
                                    } else {
                                        let empty = empty();
                                        rules.insert(
                                            empty.clone(),
                                            RuleValue::SeqOrEnum(SeqOrEnum::Seq(Seq {
                                                segments: vec![],
                                            })),
                                        );
                                        lefts.insert(empty);
                                    }

                                    if !right.is_empty() {
                                        let new_right_name =
                                            get_right_name(child, Some(parent), rules);
                                        rules.insert(
                                            new_right_name.clone(),
                                            RuleValue::SeqOrEnum(right),
                                        );

                                        rights.insert(new_right_name);
                                    } else {
                                        let empty = empty();
                                        rules.insert(
                                            empty.clone(),
                                            RuleValue::SeqOrEnum(SeqOrEnum::Seq(Seq {
                                                segments: vec![],
                                            })),
                                        );
                                        rights.insert(empty);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if !lefts.is_empty() && !rights.is_empty() {
                Ok(Split::Both(
                    SeqOrEnum::Enum(Enum {
                        segments: lefts.into_iter().collect(),
                    }),
                    SeqOrEnum::Enum(Enum {
                        segments: rights.into_iter().collect(),
                    }),
                ))
            } else if !lefts.is_empty() {
                Ok(Split::None)
            } else {
                debug_assert!(!rights.is_empty());
                Ok(Split::Right(SeqOrEnum::Enum(Enum {
                    segments: rights.into_iter().collect(),
                })))
            }
        }
    }

    impl Hash for Token {
        fn hash<H: Hasher>(&self, state: &mut H) {
            match self {
                Token::String(lit) => {
                    let mut hstr = lit.value();
                    hstr.push_str("0");
                    hstr.hash(state)
                }
                Token::Regex(reg) => {
                    let mut hstr = reg.lit.value();
                    hstr.push_str("1");
                    hstr.hash(state)
                }
            }
        }
    }

    impl Debug for Token {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Token::String(str) => str.value(),
                    Token::Regex(reg) => reg.lit.value(),
                }
            )
        }
    }

    impl Eq for Token {}

    impl PartialEq for Token {
        fn eq(&self, other: &Self) -> bool {
            match self {
                Token::String(lit) => match other {
                    Token::String(lit1) => lit.value() == lit1.value(),
                    Token::Regex(_) => false,
                },
                Token::Regex(reg) => match other {
                    Token::String(_) => false,
                    Token::Regex(reg1) => reg.lit.value() == reg1.lit.value(),
                },
            }
        }
    }
}
