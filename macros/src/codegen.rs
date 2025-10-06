use crate::state::State;
use crate::{Enum, EnumPath, LookaheadTree, RuleRef, RuleValue, Seq, SeqOrEnum, Token};
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use std::collections::HashMap;

pub struct Generator {
    state: State,
}
impl Generator {
    pub fn new(state: State) -> Self {
        Generator { state }
    }

    fn gen_type(rule: &RuleRef, need_box: bool) -> TokenStream {
        let name = rule.rule_name();
        let inner = if need_box {
            quote!(Box<#name<'a>>)
        } else {
            quote!(#name<'a>)
        };
        match rule {
            RuleRef::One(_) => quote!(#inner),
            RuleRef::OneOrMore(_) | RuleRef::ZeroOrMore(_) => quote!(Vec<#inner>),
            RuleRef::Optional(_) => quote!(Option<#inner>),
        }
    }

    fn needs_box(state: &State, parent: Option<&Ident>, rule: &RuleRef) -> bool {
        parent
            .and_then(|p| state.need_box.get(p))
            .map(|set| set.contains(rule.rule_name()))
            .unwrap_or(false)
    }

    fn gen_parse_statement(
        arg_name: &Ident,
        to_parse: &RuleRef,
        state: &State,
        opt_res_parser_name: Option<Ident>,
        need_box: bool,
    ) -> TokenStream {
        let parse_fn_name = match state
            .rules
            .get(to_parse.rule_name())
            .expect("Failed to get rule!")
        {
            RuleValue::Token(_) => "parse_token",
            RuleValue::SeqOrEnum(_) => "parse",
        };
        let res_parser_name = match opt_res_parser_name {
            None => Ident::new("parser", Span::call_site()),
            Some(name) => name,
        };
        match to_parse {
            RuleRef::One(ident) => {
                let parse_fn = Ident::new(parse_fn_name, Span::call_site());
                quote! {
                    let (#res_parser_name, #arg_name) = parser.#parse_fn::<#ident>()?;
                }
            }
            RuleRef::OneOrMore(ident) => {
                let parse_fn = Ident::new(parse_fn_name, Span::call_site());
                let opt_parse_fn =
                    Ident::new(&("opt_".to_string() + parse_fn_name), Span::call_site());
                let first_init = if need_box {
                    quote!(vec![Box::new(first)])
                } else {
                    quote!(vec![first])
                };
                let push_stmt = if need_box {
                    quote!(#arg_name.push(Box::new(next));)
                } else {
                    quote!(#arg_name.push(next);)
                };
                quote! {
                    let (mut #res_parser_name, mut #arg_name) = {
                        let (p, first) = parser.#parse_fn::<#ident>()?;
                        (p, #first_init)
                    };
                    loop {
                        let (p, opt_next) = #res_parser_name.#opt_parse_fn::<#ident>();
                        match opt_next {
                            Some(next) => {
                                #push_stmt
                                #res_parser_name = p;
                            }
                            None => {
                                #res_parser_name = p;
                                break;
                            }
                        }
                    }
                }
            }
            RuleRef::ZeroOrMore(ident) => {
                let opt_parse_fn =
                    Ident::new(&("opt_".to_string() + parse_fn_name), Span::call_site());
                let push_stmt = if need_box {
                    quote!(#arg_name.push(Box::new(next));)
                } else {
                    quote!(#arg_name.push(next);)
                };
                quote! {
                    let mut #arg_name = vec![];
                    let mut #res_parser_name = parser;
                    loop {
                        let (p, opt_next) = #res_parser_name.#opt_parse_fn::<#ident>();
                        match opt_next {
                            Some(next) => {
                                #push_stmt
                                #res_parser_name = p;
                            }
                            None => {
                                #res_parser_name = p;
                                break;
                            }
                        }
                    }
                }
            }
            RuleRef::Optional(ident) => {
                let opt_parse_fn =
                    Ident::new(&("opt_".to_string() + parse_fn_name), Span::call_site());
                quote! {
                    let (#res_parser_name, #arg_name) = parser.#opt_parse_fn::<#ident>();
                }
            }
        }
    }

    fn gen_enum_cons(path: &EnumPath, mut val: TokenStream) -> TokenStream {
        let mut variant = path.seq_or_token.clone();
        for e in path.constructors.iter().rev() {
            val = quote!(#e::#variant(#val));
            variant = e.clone();
        }
        val
    }

    fn gen_enum_parse_statement(
        parent: &syn::Ident,
        state: &State,
        tree: &LookaheadTree,
        enum_paths: &HashMap<syn::Ident, EnumPath>,
        depth: usize,
        opt_left: Option<TokenStream>,
    ) -> TokenStream {
        match tree {
            LookaheadTree::Leaf(name) => {
                let enum_path = enum_paths.get(name).expect("Failed to get enum path!");
                let (statements, value) = match state
                    .rules
                    .get(name)
                    .expect("Failed to get rule to parse rule!")
                {
                    RuleValue::Token(_) => {
                        let arg_name = Ident::new("res", Span::call_site());
                        let to_parse = if opt_left.is_some() {
                            RuleRef::Optional(name.clone())
                        } else {
                            RuleRef::One(name.clone())
                        };
                        let parse_statement = Self::gen_parse_statement(
                            &arg_name,
                            &to_parse,
                            state,
                            None,
                            Self::needs_box(state, Some(parent), &to_parse),
                        );
                        let ret = if let Some(this) = &opt_left {
                            let wrapped = Self::gen_enum_cons(enum_path, quote!(r));
                            quote! {
                                match #arg_name {
                                    Some(r) => Ok(#wrapped),
                                    None => Err(#this)
                                }
                            }
                        } else {
                            let wrapped = Self::gen_enum_cons(enum_path, quote!(#arg_name));
                            quote!(#wrapped)
                        };
                        (parse_statement, ret)
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                        let first = &seq.segments[0];
                        let first_name = first.rule_name();
                        if first_name == parent {
                            debug_assert!(seq.segments.len() == 2);
                            let right_name = seq.segments[1].rule_name();
                            let to_parse = if opt_left.is_some() {
                                RuleRef::Optional(right_name.clone())
                            } else {
                                RuleRef::One(right_name.clone())
                            };
                            let right_arg_name = Ident::new("right", Span::call_site());
                            let right_parse_statement = Self::gen_parse_statement(
                                &right_arg_name,
                                &to_parse,
                                state,
                                None,
                                Self::needs_box(state, Some(parent), &to_parse),
                            );
                            let left = match &opt_left {
                                None => {
                                    debug_assert!(match first {
                                        RuleRef::One(_) | RuleRef::OneOrMore(_) => false,
                                        RuleRef::ZeroOrMore(_) | RuleRef::Optional(_) => true,
                                    });
                                    // In this case the left field has type Option<...>
                                    quote!(None)
                                }
                                Some(l) => match first {
                                    RuleRef::One(_) => {
                                        quote!(Box::new(#l))
                                    }
                                    RuleRef::OneOrMore(_) | RuleRef::ZeroOrMore(_) => {
                                        panic!("Can't quantify left recursive element!");
                                    }
                                    RuleRef::Optional(_) => {
                                        quote!(Some(Box::new(#l)))
                                    }
                                },
                            };
                            if let Some(this) = &opt_left {
                                let wrapped = Self::gen_enum_cons(
                                    enum_path,
                                    quote! {
                                        #name {
                                            left: #left,
                                            right
                                        }
                                    },
                                );
                                (
                                    right_parse_statement,
                                    quote! {
                                        match #right_arg_name {
                                            None => Err(#this),
                                            Some(right) => {
                                                Ok(#wrapped)
                                            }
                                        }
                                    },
                                )
                            } else {
                                let wrapped =
                                    Self::gen_enum_cons(enum_path, quote!(#right_arg_name));
                                (
                                    right_parse_statement,
                                    quote! {
                                        #name {
                                            left: #left,
                                            #wrapped
                                        }
                                    },
                                )
                            }
                        } else {
                            let arg_name = Ident::new("res", Span::call_site());
                            let to_parse = if opt_left.is_some() {
                                RuleRef::Optional(name.clone())
                            } else {
                                RuleRef::One(name.clone())
                            };
                            let parse_statement = Self::gen_parse_statement(
                                &arg_name,
                                &to_parse,
                                state,
                                None,
                                Self::needs_box(state, Some(parent), &to_parse),
                            );
                            let ret = if let Some(this) = &opt_left {
                                let wrapped = Self::gen_enum_cons(enum_path, quote!(r));
                                quote! {
                                    match #arg_name {
                                        Some(r) => Ok(#wrapped),
                                        None => Err(#this)
                                    }
                                }
                            } else {
                                let wrapped = Self::gen_enum_cons(enum_path, quote!(#arg_name));
                                quote!(#wrapped)
                            };
                            (parse_statement, ret)
                        }
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Enum(_)) => {
                        panic!("Enums can't contain enums as leafs!");
                    }
                };

                quote! {
                    #statements
                    (parser, #value)
                }
            }
            LookaheadTree::Node(cases) => {
                if cases.len() == 1 {
                    let first = cases.values().next().expect("expected lookahead case");
                    Self::gen_enum_parse_statement(
                        parent,
                        state,
                        first,
                        enum_paths,
                        depth + 1,
                        opt_left.clone(),
                    )
                } else {
                    let mut entries: Vec<_> = cases.iter().collect();
                    entries
                        .sort_by(|(left, _), (right, _)| left.to_string().cmp(&right.to_string()));

                    let n = entries.len();
                    let mut cases_ts = Vec::with_capacity(n);
                    for (i, (token, tree)) in entries.into_iter().enumerate() {
                        let inner = Self::gen_enum_parse_statement(
                            parent,
                            state,
                            tree,
                            enum_paths,
                            depth + 1,
                            opt_left.clone(),
                        );
                        let condition = quote!(parser.peek(token::#token, #depth));
                        let case = if i == 0 {
                            quote!(if #condition)
                        } else if i == n - 1 {
                            quote!(else)
                        } else {
                            quote!(else if #condition)
                        };
                        cases_ts.push(quote! {
                            #case {
                                #inner
                            }
                        });
                    }
                    quote! {
                        #(#cases_ts)*
                    }
                }
            }
        }
    }

    fn gen_enum_type(name: &Ident, e: &Enum, state: &State) -> TokenStream {
        let mut enum_variants = vec![];
        for variant in &e.segments {
            enum_variants.push(Self::gen_enum_variant(variant));
        }
        let (lefts, rights) = state
            .enum_lr
            .get(name)
            .expect("Failed to get lefts and rights of enum!");
        debug_assert!(!lefts.is_empty());

        let lookahead = state
            .lookaheads
            .get(name)
            .expect("Failed to get lookahead!");
        let (left_lookahead, right_lookahead) = (&lookahead.lefts, &lookahead.rights);
        let mut statements = vec![];

        let left_name = Ident::new("left", Span::call_site());
        let left_expr = Self::gen_enum_parse_statement(
            name,
            state,
            &LookaheadTree::Node(left_lookahead.clone()),
            lefts,
            0,
            None,
        );

        let right_expr = if !rights.is_empty() {
            Some(Self::gen_enum_parse_statement(
                name,
                state,
                &LookaheadTree::Node(right_lookahead.clone()),
                rights,
                0,
                Some(quote!(this)),
            ))
        } else {
            None
        };

        match right_expr {
            Some(right) => {
                statements.push(quote! {
                    let (mut parser, mut this) = #left_expr;
                    let (parser, this) = loop {
                        let (parser1, res) = #right;
                        match res {
                            Ok(new) => {
                                this = new;
                                parser = parser1;
                            }
                            Err(old) => {
                                break (parser1, old)
                            }
                        }
                    };
                    Ok((parser, this))
                });
            }
            None => {
                statements.push(quote! {
                    let (parser, this) = #left_expr;
                    Ok((parser, this))
                });
            }
        }

        quote! {
            pub enum #name<'a> {
                #(#enum_variants),*
            }
            impl<'a> Parse for #name<'a> {
                type Error = Error;
                type Parser = Parser<Lexer<'a>>;

                fn parse(mut parser: Self::Parser) -> Result<(Self::Parser, Self), Self::Error> {
                    #(#statements)*
                }
            }
        }
    }

    fn gen_seq_type(name: &Ident, seq: &Seq, state: &State) -> TokenStream {
        let left_recursive = state.left_recursive.contains(name);
        /*
        let mut field_names = vec![];
        let mut type_fields_def = vec![];
        let mut i = 0;
        for segment in &seq.segments {
            // If the rule is left recursive we need to box the first value
            let arg_name = Ident::new(&format!("arg_{}", i), Span::call_site());
            let typ = Self::gen_type(segment, i == 0 && left_recursive);
            type_fields_def.push(quote!(#arg_name: #typ));
            field_names.push(arg_name);
            i += 1;
        }*/

        // Generate parse statements
        let first = &seq.segments[0];
        let first_name = first.rule_name();

        let (struct_def, statements) = if left_recursive {
            let mut statements = vec![];
            // This is the case when self is left-recursive
            debug_assert!(left_recursive);
            /*
            debug_assert!(match first {
                RuleRef::One(_) | RuleRef::OneOrMore(_) => false,
                RuleRef::ZeroOrMore(_) | RuleRef::Optional(_) => true,
            });*/
            debug_assert!(seq.segments.len() == 2);
            let left_side_arg_name = Ident::new("left", Span::call_site());
            let left_side_type = Self::gen_type(first, true);
            let right_side_arg_name = Ident::new("right", Span::call_site());
            let right_side = &seq.segments[1];
            let right_side_typ = Self::gen_type(right_side, false);

            let struct_def = quote! {
                pub struct #name<'a> {
                    #left_side_arg_name: #left_side_type,
                    #right_side_arg_name: #right_side_typ
                }
            };

            let right_parse_statement = Self::gen_parse_statement(
                &right_side_arg_name,
                right_side,
                state,
                None,
                Self::needs_box(state, Some(name), right_side),
            );

            if first_name == name {
                let op_parse_statement = Self::gen_parse_statement(
                    &right_side_arg_name,
                    &RuleRef::Optional(right_side.rule_name().clone()),
                    state,
                    Some(Ident::new("p", Span::call_site())),
                    Self::needs_box(
                        state,
                        Some(name),
                        &RuleRef::Optional(right_side.rule_name().clone()),
                    ),
                );
                statements.push(right_parse_statement);
                statements.push(quote! {
                    let mut this = #name {
                        #left_side_arg_name: None,
                        #right_side_arg_name
                    };
                    let this = loop {
                        #op_parse_statement
                        match #right_side_arg_name {
                            Some(right) => {
                                this = #name {
                                    #left_side_arg_name: Some(Box::new(this)),
                                    #right_side_arg_name: right
                                };
                                parser = p;
                            },
                            None => break this
                        }
                    };
                    Ok((parser, this))
                });
            } else {
                statements.push(Self::gen_parse_statement(
                    &left_side_arg_name,
                    first,
                    state,
                    None,
                    Self::needs_box(state, Some(name), first),
                ));
                statements.push(right_parse_statement);
                // TODO make a general version
                let left_init = match first {
                    RuleRef::One(_) => {
                        quote!(#left_side_arg_name: Box::new(#left_side_arg_name))
                    }
                    RuleRef::OneOrMore(_) | RuleRef::ZeroOrMore(_) => {
                        quote!(#left_side_arg_name)
                    }
                    RuleRef::Optional(_) => {
                        quote!(#left_side_arg_name: #left_side_arg_name.map(Box::new))
                    }
                };
                statements.push(quote! {
                    Ok((parser, #name {
                        #left_init,
                        #right_side_arg_name
                    }))
                });
            }
            (struct_def, statements)
        } else {
            let mut statements = vec![];
            let mut field_defs = vec![];
            let mut field_cons = vec![];
            let mut i = 0;
            for segment in &seq.segments {
                let field_name = Ident::new(&format!("arg_{}", i), Span::call_site());
                let need_box = match state.need_box.get(name) {
                    None => false,
                    Some(set) => set.contains(segment.rule_name()),
                };

                let field_type = Self::gen_type(segment, need_box);
                field_defs.push(quote!(#field_name: #field_type));
                let cons = if need_box {
                    match segment {
                        RuleRef::One(_) => {
                            quote!(#field_name: Box::new(#field_name))
                        }
                        RuleRef::OneOrMore(_) | RuleRef::ZeroOrMore(_) => {
                            quote!(#field_name)
                        }
                        RuleRef::Optional(_) => {
                            quote!(#field_name: #field_name.map(Box::new))
                        }
                    }
                } else {
                    quote!(#field_name)
                };
                field_cons.push(quote!(#cons));
                statements.push(Self::gen_parse_statement(
                    &field_name,
                    segment,
                    state,
                    None,
                    need_box,
                ));
                i += 1;
            }
            let struct_def = quote! {
                pub struct #name<'a> {
                    #(#field_defs),*
                }
            };
            statements.push(quote! {
                Ok((parser, #name {
                    #(#field_cons),*
                }))
            });

            (struct_def, statements)
        };
        quote! {
            #struct_def
            impl<'a> Parse for #name<'a> {
                type Error = Error;
                type Parser = Parser<Lexer<'a>>;

                fn parse(mut parser: Self::Parser) -> Result<(Self::Parser, Self), Self::Error> {
                    #(#statements)*
                }
            }
        }
    }

    fn gen_token_type(name: &syn::Ident, token: &Token) -> TokenStream {
        quote! {
            pub struct #name<'a> {
                span: Span<'a>
            }
            impl<'a> Clone for #name<'a> {
                fn clone(&self) -> Self {
                    #name {
                        span: Span::new(self.span.code, self.span.range.clone())
                    }
                }
            }
            impl<'a> Parse for #name<'a> {
                type Error = Error;
                type Parser = Lexer<'a>;

                fn parse(mut cursor: Self::Parser) -> Result<(Self::Parser, Self), Self::Error> {
                    if let Some(Token::#name(token)) = cursor.next() {
                        Ok((cursor, token))
                    } else {
                        Err(Error::UnexpectedToken)
                    }
                }
            }
        }
    }

    fn gen_lexer(token_defs: &[(syn::Ident, Token)]) -> TokenStream {
        let has_regex = token_defs
            .iter()
            .any(|(_, token)| matches!(token, Token::Regex(_)));

        let regex_use = if has_regex {
            Some(quote!(
                use regex::Regex;
            ))
        } else {
            None
        };

        let mut regex_decls = Vec::new();
        let mut matcher_blocks = Vec::new();
        let mut constructors = Vec::new();

        for (idx, (name, token)) in token_defs.iter().enumerate() {
            let idx_lit = syn::LitInt::new(&idx.to_string(), Span::call_site());
            constructors.push(quote! {
                #idx_lit => Token::#name(#name { span }),
            });

            match token {
                Token::String(lit) => {
                    matcher_blocks.push(quote! {
                        if code[position..].starts_with(#lit) {
                            let len = #lit.len();
                            match best_match {
                                Some((best_len, _)) if best_len >= len => {}
                                _ => best_match = Some((len, #idx_lit)),
                            }
                        }
                    });
                }
                Token::Regex(regex) => {
                    let var_name = Ident::new(
                        &format!("regex_{}", name.to_string().to_lowercase()),
                        Span::call_site(),
                    );
                    let lit = &regex.lit;
                    regex_decls.push(quote! {
                        let #var_name = Regex::new(#lit)
                            .expect(concat!("invalid regex for token ", stringify!(#name)));
                    });
                    matcher_blocks.push(quote! {
                        if let Some(mat) = #var_name.find(&code[position..]) {
                            if mat.start() == 0 {
                                let len = mat.end();
                                match best_match {
                                    Some((best_len, _)) if best_len >= len => {}
                                    _ => best_match = Some((len, #idx_lit)),
                                }
                            }
                        }
                    });
                }
            }
        }

        quote! {
            #regex_use

            #[derive(Clone)]
            pub struct Lexer<'a> {
                tokens: Vec<Token<'a>>,
                position: usize,
            }

            impl<'a> Lexer<'a> {
                pub fn new(code: &'a str) -> Result<Self, LexError> {
                    let mut tokens = Vec::new();
                    let mut position = 0usize;
                    #(#regex_decls)*
                    while position < code.len() {
                        let mut best_match: Option<(usize, usize)> = None;
                        #(#matcher_blocks)*
                        match best_match {
                            Some((len, idx)) => {
                                if len == 0 {
                                    return Err(LexError::UnexspectedCharacter { index: position });
                                }
                                let end = position + len;
                                let span = Span::new(code, position..end);
                                let token = match idx {
                                    #(#constructors)*
                                    _ => unreachable!(),
                                };
                                tokens.push(token);
                                position = end;
                            }
                            None => {
                                return Err(LexError::UnexspectedCharacter { index: position });
                            }
                        }
                    }
                    Ok(Self { tokens, position: 0 })
                }
            }

            impl<'a> Cursor for Lexer<'a> {
                type Item = Token<'a>;

                fn next(&mut self) -> Option<Self::Item> {
                    if self.position < self.tokens.len() {
                        let token = self.tokens[self.position].clone();
                        self.position += 1;
                        Some(token)
                    } else {
                        None
                    }
                }

                fn peek(&mut self, k: usize) -> Option<&Self::Item> {
                    self.tokens.get(self.position + k)
                }

                fn position(&self) -> usize {
                    self.position
                }
            }
        }
    }

    fn gen_enum_variant(name: &syn::Ident) -> TokenStream {
        quote! {
            #name(#name<'a>)
        }
    }

    fn gen_document(
        token_defs: Vec<(syn::Ident, Token)>,
        token_types: Vec<TokenStream>,
        token_peeks: Vec<TokenStream>,
        token_ev: Vec<TokenStream>,
        seq_impls: Vec<TokenStream>,
        enum_impls: Vec<TokenStream>,
    ) -> TokenStream {
        let lexer = Self::gen_lexer(&token_defs);
        quote! {
            mod parser {
                use lr::{Cursor, Parser, Parse, Span};

                pub enum LexError {
                    UnexspectedCharacter { index: usize }
                }
                pub enum Error {
                    UnexpectedToken
                }
                #[derive(Clone)]
                pub enum Token<'a> {
                    #(#token_ev),*
                }
                mod token {
                    use lr::Peek;

                    #(#token_peeks)*
                }
                #lexer
                #(#token_types)*
                #(#seq_impls)*
                #(#enum_impls)*
            }
        }
    }

    pub fn generate(&self) -> TokenStream {
        let mut token_types = vec![];
        let mut token_defs = vec![];
        let mut token_peeks = vec![];
        let mut token_ev = vec![];
        let mut seq_impls = vec![];
        let mut enum_impls = vec![];

        //dbg!(&self.state.lookaheads);

        let mut rule_names: Vec<_> = self.state.rules.keys().collect();
        rule_names.sort_by(|a, b| a.to_string().cmp(&b.to_string()));

        for name in rule_names {
            let value = self.state.rules.get(name).expect("missing rule for name");
            match value {
                RuleValue::Token(token) => {
                    token_defs.push((name.clone(), token.clone()));
                    token_types.push(Self::gen_token_type(name, &token));
                    token_peeks.push(quote! {
                        pub(super) struct #name;
                        impl<'a> Peek<super::Lexer<'a>> for #name {
                            fn peek(token: &super::Token<'a>) -> bool {
                                matches!(token, super::Token::#name(_))
                            }
                        }
                    });
                    token_ev.push(Self::gen_enum_variant(name));
                }
                RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                    seq_impls.push(Self::gen_seq_type(name, &seq, &self.state))
                }
                RuleValue::SeqOrEnum(SeqOrEnum::Enum(e)) => {
                    enum_impls.push(Self::gen_enum_type(name, &e, &self.state))
                }
            }
        }
        //dbg!(&self.state.need_box);
        //dbg!(Self::gen_document(token_types, token_peeks, token_ev, seq_impls, enum_impls).to_string());
        //quote!()
        Self::gen_document(
            token_defs,
            token_types,
            token_peeks,
            token_ev,
            seq_impls,
            enum_impls,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    fn generate_source(input: &str) -> String {
        let grammar = syn::parse_str::<crate::Grammer>(input).expect("failed to parse grammar");
        let state = crate::state::Builder::new(grammar)
            .build()
            .expect("failed to build generator state");
        Generator::new(state).generate().to_string()
    }

    #[test]
    fn generates_token_only_grammar() {
        let actual = generate_source(
            r#"
            TokenA = "a";
        "#,
        );

        let expected = quote! {
            mod parser {
                use lr::{Cursor, Parser, Parse, Span};

                pub enum LexError {
                    UnexspectedCharacter { index: usize }
                }
                pub enum Error {
                    UnexpectedToken
                }
                #[derive(Clone)]
                pub enum Token<'a> {
                    TokenA(TokenA<'a>)
                }
                mod token {
                    use lr::Peek;

                    pub(super) struct TokenA;
                    impl<'a> Peek<super::Lexer<'a>> for TokenA {
                        fn peek(token: &super::Token<'a>) -> bool {
                            matches!(token, super::Token::TokenA(_))
                        }
                    }
                }
                #[derive(Clone)]
                pub struct Lexer<'a> {
                    tokens: Vec<Token<'a>>,
                    position: usize,
                }
                impl<'a> Lexer<'a> {
                    pub fn new(code: &'a str) -> Result<Self, LexError> {
                        let mut tokens = Vec::new();
                        let mut position = 0usize;
                        while position < code.len() {
                            let mut best_match: Option<(usize, usize)> = None;
                            if code[position..].starts_with("a") {
                                let len = "a".len();
                                match best_match {
                                    Some((best_len, _)) if best_len >= len => {}
                                    _ => best_match = Some((len, 0)),
                                }
                            }
                            match best_match {
                                Some((len, idx)) => {
                                    if len == 0 {
                                        return Err(LexError::UnexspectedCharacter { index: position });
                                    }
                                    let end = position + len;
                                    let span = Span::new(code, position..end);
                                    let token = match idx {
                                        0 => Token::TokenA(TokenA { span }),
                                        _ => unreachable!(),
                                    };
                                    tokens.push(token);
                                    position = end;
                                }
                                None => {
                                    return Err(LexError::UnexspectedCharacter { index: position });
                                }
                            }
                        }
                        Ok(Self { tokens, position: 0 })
                    }
                }
                impl<'a> Cursor for Lexer<'a> {
                    type Item = Token<'a>;

                    fn next(&mut self) -> Option<Self::Item> {
                        if self.position < self.tokens.len() {
                            let token = self.tokens[self.position].clone();
                            self.position += 1;
                            Some(token)
                        } else {
                            None
                        }
                    }
                    fn peek(&mut self, k: usize) -> Option<&Self::Item> {
                        self.tokens.get(self.position + k)
                    }
                    fn position(&self) -> usize {
                        self.position
                    }
                }
                pub struct TokenA<'a> {
                    span: Span<'a>
                }
                impl<'a> Clone for TokenA<'a> {
                    fn clone(&self) -> Self {
                        TokenA {
                            span: Span::new(self.span.code, self.span.range.clone())
                        }
                    }
                }
                impl<'a> Parse for TokenA<'a> {
                    type Error = Error;
                    type Parser = Lexer<'a>;

                    fn parse(mut cursor: Self::Parser) -> Result<(Self::Parser, Self), Self::Error> {
                        if let Some(Token::TokenA(token)) = cursor.next() {
                            Ok((cursor, token))
                        } else {
                            Err(Error::UnexpectedToken)
                        }
                    }
                }
            }
        }
        .to_string();

        assert_eq!(actual, expected);
    }

    #[test]
    fn generates_sequence_grammar() {
        let actual = generate_source(
            r#"
            TokenA = "a";
            Start = TokenA;
        "#,
        );

        let expected = quote! {
            mod parser {
                use lr::{Cursor, Parser, Parse, Span};

                pub enum LexError {
                    UnexspectedCharacter { index: usize }
                }
                pub enum Error {
                    UnexpectedToken
                }
                #[derive(Clone)]
                pub enum Token<'a> {
                    TokenA(TokenA<'a>)
                }
                mod token {
                    use lr::Peek;

                    pub(super) struct TokenA;
                    impl<'a> Peek<super::Lexer<'a>> for TokenA {
                        fn peek(token: &super::Token<'a>) -> bool {
                            matches!(token, super::Token::TokenA(_))
                        }
                    }
                }
                #[derive(Clone)]
                pub struct Lexer<'a> {
                    tokens: Vec<Token<'a>>,
                    position: usize,
                }
                impl<'a> Lexer<'a> {
                    pub fn new(code: &'a str) -> Result<Self, LexError> {
                        let mut tokens = Vec::new();
                        let mut position = 0usize;
                        while position < code.len() {
                            let mut best_match: Option<(usize, usize)> = None;
                            if code[position..].starts_with("a") {
                                let len = "a".len();
                                match best_match {
                                    Some((best_len, _)) if best_len >= len => {}
                                    _ => best_match = Some((len, 0)),
                                }
                            }
                            match best_match {
                                Some((len, idx)) => {
                                    if len == 0 {
                                        return Err(LexError::UnexspectedCharacter { index: position });
                                    }
                                    let end = position + len;
                                    let span = Span::new(code, position..end);
                                    let token = match idx {
                                        0 => Token::TokenA(TokenA { span }),
                                        _ => unreachable!(),
                                    };
                                    tokens.push(token);
                                    position = end;
                                }
                                None => {
                                    return Err(LexError::UnexspectedCharacter { index: position });
                                }
                            }
                        }
                        Ok(Self { tokens, position: 0 })
                    }
                }
                impl<'a> Cursor for Lexer<'a> {
                    type Item = Token<'a>;

                    fn next(&mut self) -> Option<Self::Item> {
                        if self.position < self.tokens.len() {
                            let token = self.tokens[self.position].clone();
                            self.position += 1;
                            Some(token)
                        } else {
                            None
                        }
                    }
                    fn peek(&mut self, k: usize) -> Option<&Self::Item> {
                        self.tokens.get(self.position + k)
                    }
                    fn position(&self) -> usize {
                        self.position
                    }
                }
                pub struct TokenA<'a> {
                    span: Span<'a>
                }
                impl<'a> Clone for TokenA<'a> {
                    fn clone(&self) -> Self {
                        TokenA {
                            span: Span::new(self.span.code, self.span.range.clone())
                        }
                    }
                }
                impl<'a> Parse for TokenA<'a> {
                    type Error = Error;
                    type Parser = Lexer<'a>;

                    fn parse(mut cursor: Self::Parser) -> Result<(Self::Parser, Self), Self::Error> {
                        if let Some(Token::TokenA(token)) = cursor.next() {
                            Ok((cursor, token))
                        } else {
                            Err(Error::UnexpectedToken)
                        }
                    }
                }
                pub struct Start<'a> {
                    arg_0: TokenA<'a>
                }
                impl<'a> Parse for Start<'a> {
                    type Error = Error;
                    type Parser = Parser<Lexer<'a>>;

                    fn parse(mut parser: Self::Parser) -> Result<(Self::Parser, Self), Self::Error> {
                        let (parser, arg_0) = parser.parse_token::<TokenA>()?;
                        Ok((parser, Start { arg_0 }))
                    }
                }
            }
        }
        .to_string();

        assert_eq!(actual, expected);
    }

    #[test]
    fn generates_left_recursive_grammar() {
        let actual = generate_source(
            r#"
            TokenA = "a";
            TokenB = "b";
            Start = StartTail | TokenB;
            StartTail = Start TokenA;
        "#,
        );

        if std::env::var_os("UPDATE_MACRO_FIXTURES").is_some() {
            let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests/fixtures/left_recursive.txt");
            std::fs::write(path, &actual).unwrap();
            return;
        }

        let expected = include_str!("../tests/fixtures/left_recursive.txt");
        assert_eq!(actual, expected);
    }

    #[test]
    fn generates_nested_left_recursive_grammar() {
        let actual = generate_source(
            r#"
            TokenA = "a";
            TokenB = "b";
            Start = StartTail | TokenB;
            StartTail = Start Child;
            Child = ChildTail | TokenA;
            ChildTail = Child TokenA;
        "#,
        );

        if std::env::var_os("UPDATE_MACRO_FIXTURES").is_some() {
            let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests/fixtures/nested_left_recursive.txt");
            std::fs::write(path, &actual).unwrap();
            return;
        }

        let expected = include_str!("../tests/fixtures/nested_left_recursive.txt");
        assert_eq!(actual, expected);
    }

    #[test]
    fn generates_deeply_nested_left_recursive_grammar() {
        let actual = generate_source(
            r#"
            TokenA = "a";
            TokenB = "b";
            TokenC = "c";
            TokenD = "d";
            TokenE = "e";
            TokenZ = "z";
            Start = StartTail | TokenZ;
            StartTail = SeqA;
            SeqA = EnumA TokenA;
            EnumA = SeqB | TokenB;
            SeqB = EnumB TokenC;
            EnumB = SeqC | TokenE;
            SeqC = Start TokenD;
        "#,
        );

        if std::env::var_os("UPDATE_MACRO_FIXTURES").is_some() {
            let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests/fixtures/deep_nested_left_recursive.txt");
            std::fs::write(path, &actual).unwrap();
            return;
        }

        let expected = include_str!("../tests/fixtures/deep_nested_left_recursive.txt");
        assert_eq!(actual, expected);
    }
}
