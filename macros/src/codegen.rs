use crate::state::State;
use crate::{Enum, LookaheadTree, RuleRef, RuleValue, Seq, SeqOrEnum, Token};
use quote::__private::{Ident, TokenStream};
use quote::quote;
use std::collections::HashSet;
use syn::__private::Span;

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

    fn gen_parse_statement(
        arg_name: &Ident,
        to_parse: &RuleRef,
        state: &State,
        opt_res_parser_name: Option<Ident>,
    ) -> TokenStream {
        let parse_fn_name = match state.rules.get(to_parse.rule_name()).unwrap() {
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
                quote! {
                    let (mut parser, mut #arg_name) = {
                        let (p, first) = parser.#parse_fn::<#ident>()?;
                        (p, vec![first])
                    };
                    let (#res_parser_name, #arg_name) = loop {
                        let (p, opt_next) = parser.#opt_parse_fn::<#ident>()?;
                        match opt_next {
                            Some(next) => {
                                #arg_name.push(next);
                                parser = p;
                            }
                            None => break (parser, #arg_name)
                        }
                    };
                }
            }
            RuleRef::ZeroOrMore(ident) => {
                let opt_parse_fn =
                    Ident::new(&("opt_".to_string() + parse_fn_name), Span::call_site());
                quote! {
                    let mut #arg_name = vec![];
                    let (#res_parser_name, #arg_name) = loop {
                        let (p, opt_next) = parser.#opt_parse_fn::<#ident>()?;
                        match opt_next {
                            Some(next) => {
                                #arg_name.push(next);
                                parser = p;
                            }
                            None => break (parser, #arg_name)
                        }
                    };
                }
            }
            RuleRef::Optional(ident) => {
                let opt_parse_fn =
                    Ident::new(&("opt_".to_string() + parse_fn_name), Span::call_site());
                quote! {
                    let (#res_parser_name, #arg_name) = parser.#opt_parse_fn::<#ident>()?;
                }
            }
        }
    }

    fn gen_enum_parse_statement(parent: &syn::Ident, state: &State, tree: &LookaheadTree, depth: usize, opt_left: Option<TokenStream>) -> TokenStream {
        match tree {
            LookaheadTree::Leaf(name) => {
                match state.rules.get(name).unwrap() {
                    RuleValue::Token(_) => {
                        let arg_name = Ident::new("res", Span::call_site());
                        let parse_statement = Self::gen_parse_statement(
                            &arg_name,
                            &RuleRef::One(name.clone()),
                            state,
                            None
                        );
                        quote!{
                            #parse_statement
                            (parser, #arg_name)
                        }
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                        let first = &seq.segments[0];
                        let first_name = first.rule_name();
                        if first_name == parent {
                            debug_assert!(seq.segments.len() == 2);
                            let right_name = &seq.segments[1];
                            let right_arg_name = Ident::new("right", Span::call_site());
                            let right_parse_statement = Self::gen_parse_statement(
                                &right_arg_name,
                                right_name,
                                state,
                                None
                            );
                            let left = match opt_left {
                                None => {
                                    debug_assert!(match first {
                                        RuleRef::One(_) | RuleRef::OneOrMore(_) => false,
                                        RuleRef::ZeroOrMore(_) | RuleRef::Optional(_) => true
                                    });
                                    quote!(None)
                                },
                                Some(l) => l
                            };
                            quote! {
                                #right_parse_statement
                                (parser, #name {
                                    left: #left,
                                    #right_arg_name
                                })
                            }
                        } else {
                            let arg_name = Ident::new("res", Span::call_site());
                            let parse_statement = Self::gen_parse_statement(
                                &arg_name,
                                &RuleRef::One(name.clone()),
                                state,
                                None
                            );
                            quote! {
                                #parse_statement
                                (parser, #arg_name)
                            }
                        }
                    }
                    RuleValue::SeqOrEnum(SeqOrEnum::Enum(_)) => {
                        panic!("Enums can't contain enums as leafs!");
                    }
                }
            }
            LookaheadTree::Node(cases) => {
                let mut i = 0;
                let n = cases.len();
                for (token, tree) in cases {
                    let ifelse = match i {
                        0 => quote!(if),
                        n- 1 => quote!(else),
                        _ => quote!(else if)
                    };
                }
            }
        }
    }

    fn gen_enum_type(name: &Ident, e: &Enum, state: &State) -> TokenStream {
        let mut enum_variants = vec![];
        for variant in &e.segments {
            enum_variants.push(Self::gen_enum_variant(variant));
        }
        let (lefts, rights) = state.enum_lr.get(name).unwrap();
        let lookahead = state.lookaheads.get(name).unwrap();
        let (left_lookahead, right_lookahead) = (&lookahead.lefts, &lookahead.rights);
        let mut statements = vec![];

        let left_name = Ident::new("left", Span::call_site());



        statements.push(quote! {
            let (parser, left) = #(#left_cases);
        });

        statements.

        quote! {
            enum #name<'a> {
                #(#enum_variants),*
            }
            impl<'a> Parse for #name<'a> {
                type Error = Error;
                type Parser = Parser<Lexer<'a>>;

                fn parse(mut parser: Self::Parser) -> Result<(Self::Parser, Self), Self::Error> {
                    todo!()
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
                struct #name<'a> {
                    #left_side_arg_name: #left_side_type,
                    #right_side_arg_name: #right_side_typ
                }
            };

            let right_parse_statement =
                Self::gen_parse_statement(&right_side_arg_name, right_side, state, None);

            if first_name == name {
                let op_parse_statement = Self::gen_parse_statement(
                    &right_side_arg_name,
                    &RuleRef::Optional(right_side.rule_name().clone()),
                    state,
                    Some(Ident::new("p", Span::call_site())),
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
                statements.push(Self::gen_parse_statement(&field_name, segment, state, None));
                i += 1;
            }
            let struct_def = quote! {
                struct #name<'a> {
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
            struct #name<'a> {
                span: Span<'a>
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

    fn gen_enum_variant(name: &syn::Ident) -> TokenStream {
        quote! {
            #name(#name<'a>)
        }
    }

    fn gen_document(
        token_types: Vec<TokenStream>,
        token_peeks: Vec<TokenStream>,
        token_ev: Vec<TokenStream>,
        seq_impls: Vec<TokenStream>,
        enum_impls: Vec<TokenStream>,
    ) -> TokenStream {
        quote! {
            use lr::{Cursor, Parser, Parse, Peek, Span};
            use std::marker::PhantomData;

            enum LexError {
                UnexspectedCharacter
            }
            enum Error {
                UnexpectedToken
            }
            enum Token<'a> {
                #(#token_ev),*
            }
            mod token {
                #(#token_peeks)*
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
                fn peek(&mut self, k: usize) -> Option<&Self::Item> {
                    todo!()
                }
                fn position(&self) -> usize {
                    todo!()
                }
            }
            #(#token_types)*
            #(#seq_impls)*
            #(#enum_impls)*
        }
    }

    pub fn generate(&self) -> TokenStream {
        let mut token_types = vec![];
        let mut token_peeks = vec![];
        let mut token_ev = vec![];
        let mut seq_impls = vec![];
        let mut enum_impls = vec![];
        for (name, value) in &self.state.rules {
            match value {
                RuleValue::Token(token) => {
                    token_types.push(Self::gen_token_type(name, token));
                    token_peeks.push(quote! {
                        struct #name;
                        impl Peek for #name {}
                    });
                    token_ev.push(Self::gen_enum_variant(name));
                }
                RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                    seq_impls.push(Self::gen_seq_type(name, seq, &self.state))
                }
                RuleValue::SeqOrEnum(SeqOrEnum::Enum(e)) => {
                    enum_impls.push(Self::gen_enum_type(name, e, &self.state))
                }
            }
        }
        dbg!(&self.state.need_box);
        //dbg!(Self::gen_document(token_types, token_ev, seq_impls, enum_impls).to_string());
        //quote!()
        Self::gen_document(token_types, token_peeks, token_ev, seq_impls, enum_impls)
    }
}
