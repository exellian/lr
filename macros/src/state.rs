use crate::implementation::get_right_name;
use crate::{
    errors, Enum, EnumPath, Grammer, Lookahead, LookaheadTree, RuleRef, RuleValue, Seq, SeqOrEnum,
};
use proc_macro::TokenStream;
use quote::__private::Ident;
use std::collections::{HashMap, HashSet};

macro_rules! err {
    // `()` indicates that the macro takes no argument.
    ($span: expr, $message: expr) => {
        syn::Error::new($span, $message).to_compile_error().into()
    };
}

pub struct Builder {
    grammer: Grammer,
}

pub struct State {
    pub(crate) rules: HashMap<Ident, RuleValue>,
    pub(crate) enum_lr: HashMap<Ident, (HashMap<Ident, EnumPath>, HashMap<Ident, EnumPath>)>,
    pub(crate) left_recursive: HashSet<Ident>,
    pub(crate) lookaheads: HashMap<Ident, Lookahead>,
    pub(crate) need_box: HashMap<Ident, HashSet<Ident>>,
}
impl State {
    pub(crate) fn new(
        rules: HashMap<Ident, RuleValue>,
        enum_lr: HashMap<Ident, (HashMap<Ident, EnumPath>, HashMap<Ident, EnumPath>)>,
        left_recursive: HashSet<Ident>,
        lookaheads: HashMap<Ident, Lookahead>,
        need_box: HashMap<Ident, HashSet<Ident>>,
    ) -> Self {
        State {
            rules,
            enum_lr,
            left_recursive,
            lookaheads,
            need_box,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Grammer;
    use syn::parse_str;

    #[test]
    fn allows_sequences_with_modifiers() {
        let grammar: Grammer = parse_str(
            r#"
            TokenA = "a";
            Item = TokenA*;
            OptionalItem = [Item];
            PlusTail = TokenA+ TokenA;
        "#,
        )
        .expect("failed to parse grammar");

        assert!(Builder::new(grammar).build().is_ok());
    }

    #[test]
    fn rejects_left_recursion_after_optional_prefix() {
        let grammar: Grammer = parse_str(
            r#"
            TokenA = "a";
            Expr = [TokenA] Expr;
        "#,
        )
        .expect("failed to parse grammar");

        assert!(std::panic::catch_unwind(|| Builder::new(grammar).build()).is_err());
    }
}

impl Builder {
    pub(crate) fn new(grammer: Grammer) -> Self {
        Builder { grammer }
    }

    fn lift_out_left_recursive(
        rule: &Ident,
        opt_parent: Option<&Ident>,
        rules: &mut HashMap<Ident, RuleValue>,
    ) -> Result<(), TokenStream> {
        let seq = match rules.get(rule).unwrap().clone() {
            RuleValue::Token(_) | RuleValue::SeqOrEnum(SeqOrEnum::Enum(_)) => {
                panic!("Can't lift out left recursive element on enums or tokens!")
            }
            RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => seq,
        };
        let parent = match opt_parent {
            None => rule,
            Some(p) => p,
        };
        let (recursive, right) = match seq.lift_out_left_recursive(parent, rules) {
            Ok(res) => res,
            Err(err) => return Err(err),
        };
        let right_name = get_right_name(rule, None, &rules);
        // Insert new right rule
        rules.insert(
            right_name.clone(),
            RuleValue::SeqOrEnum(SeqOrEnum::Seq(right)),
        );
        // Override old rule with the recursive element lifted to the top
        rules.insert(
            rule.clone(),
            RuleValue::SeqOrEnum(SeqOrEnum::Seq(Seq {
                segments: vec![recursive, RuleRef::One(right_name)],
            })),
        );
        Ok(())
    }

    pub fn build(self) -> Result<State, TokenStream> {
        // Extract the rules from the grammer and check for duplicates
        let mut rules = HashMap::new();
        for rule in self.grammer.rules {
            if rules.contains_key(&rule.name) {
                return Err(err!(
                    rule.name.span(),
                    format!("Duplicate rule {}", rule.name)
                ));
            }
            rules.insert(rule.name, rule.value);
        }

        // Ensure:
        // 1. Every referenced rule exists
        // 2. Sequences have at least length 1
        // 3. Sequences start with no quantity modifier
        for (name, val) in &rules {
            match val {
                RuleValue::Token(_) => {}
                RuleValue::SeqOrEnum(soe) => match soe {
                    SeqOrEnum::Seq(seq) => {
                        if seq.segments.len() < 1 {
                            return Err(err!(
                                name.span(),
                                format!("Sequence must be at least length 1!")
                            ));
                        }
                        for r_ref in &seq.segments {
                            let i = r_ref.rule_name();
                            if !rules.contains_key(i) {
                                return Err(err!(
                                    i.span(),
                                    format!("Cannot find identifier {}", i)
                                ));
                            }
                        }
                    }
                    SeqOrEnum::Enum(e) => {
                        for i in &e.segments {
                            if !rules.contains_key(i) {
                                return Err(err!(
                                    i.span(),
                                    format!("cannot find identifier {}", i)
                                ));
                            }
                        }
                    }
                },
            }
        }

        // Ensure that every rule can be terminated
        // or. every rule is not infinite size
        let mut size_errs = vec![];
        for (name, val) in &rules {
            match val {
                RuleValue::Token(_) => {}
                RuleValue::SeqOrEnum(soe) => {
                    let size = match soe {
                        SeqOrEnum::Seq(seq) => seq.calculate_min_size(&rules, &mut HashSet::new()),
                        SeqOrEnum::Enum(e) => e.calculate_min_size(&rules, &mut HashSet::new()),
                    };
                    if let None = size {
                        size_errs.push((name.span(), format!("Rule has infinite size!")));
                    }
                }
            }
        }
        if !size_errs.is_empty() {
            return Err(errors(size_errs));
        }

        let mut enum_lr = HashMap::new();
        let mut left_recursive = HashSet::new();
        // Lift out every left recursive rule
        for (name, val) in rules.clone() {
            match val {
                RuleValue::Token(_) => {}
                RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                    if let Some(index) = seq.leftest_index_of(&name, &rules) {
                        if seq.leftest(&rules).rule_name() != &name {
                            continue;
                        }
                        if index > 0 {
                            let offending = &seq.segments[index];
                            return Err(err!(
                                offending.rule_name().span(),
                                format!(
                                    "Left recursive rule {} cannot have optional or repeatable elements before the recursive reference", name
                                )
                            ));
                        }
                        Self::lift_out_left_recursive(&name, None, &mut rules)?;
                        left_recursive.insert(name);
                    }
                }
                RuleValue::SeqOrEnum(SeqOrEnum::Enum(e)) => {
                    let (lefts, rights) = match e.calculate_lefts_and_rights(&name, &rules) {
                        Ok(res) => res,
                        Err(err) => return Err(err),
                    };
                    for (_, left) in &lefts {
                        match rules.get(&left.seq_or_token).unwrap() {
                            RuleValue::Token(_) => {}
                            RuleValue::SeqOrEnum(SeqOrEnum::Enum(_)) => {
                                panic!("Enum can't contain enums as leafs!")
                            }
                            RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                                if let Some(index) = seq.leftest_index_of(&name, &rules) {
                                    if seq.leftest(&rules).rule_name() != &name {
                                        continue;
                                    }
                                    if index > 0 {
                                        let offending = &seq.segments[index];
                                        return Err(err!(
                                            offending.rule_name().span(),
                                            format!(
                                                "Left recursive rule {} cannot have optional or repeatable elements before the recursive reference", name
                                            )
                                        ));
                                    }
                                    Self::lift_out_left_recursive(
                                        &left.seq_or_token,
                                        Some(&name),
                                        &mut rules,
                                    )?;
                                    left_recursive.insert(left.seq_or_token.clone());
                                }
                            }
                        }
                    }
                    for (_, right) in &rights {
                        match rules.get(&right.seq_or_token).unwrap() {
                            RuleValue::Token(_) => {
                                panic!("Enum can't contain tokens as right sides!")
                            }
                            RuleValue::SeqOrEnum(SeqOrEnum::Enum(_)) => {
                                panic!("Enum can't contain enums as leafs!")
                            }
                            RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                                if let Some(index) = seq.leftest_index_of(&name, &rules) {
                                    if seq.leftest(&rules).rule_name() != &name {
                                        continue;
                                    }
                                    if index > 0 {
                                        let offending = &seq.segments[index];
                                        return Err(err!(
                                            offending.rule_name().span(),
                                            format!(
                                                "Left recursive rule {} cannot have optional or repeatable elements before the recursive reference", name
                                            )
                                        ));
                                    }
                                }
                                Self::lift_out_left_recursive(
                                    &right.seq_or_token,
                                    Some(&name),
                                    &mut rules,
                                )?;
                                left_recursive.insert(right.seq_or_token.clone());
                            }
                        }
                    }
                    enum_lr.insert(name, (lefts, rights));
                }
            }
        }

        // Calculate the lookahead for every enum
        let mut lookaheads = HashMap::new();
        for (name, val) in &rules {
            match val {
                RuleValue::Token(_) | RuleValue::SeqOrEnum(SeqOrEnum::Seq(_)) => {}
                RuleValue::SeqOrEnum(SeqOrEnum::Enum(_)) => {
                    let (lefts, rights) = enum_lr.get(name).unwrap();
                    let lookahead = Enum::calculate_lookahead1(lefts, rights, &rules)?;
                    lookaheads.insert(name.clone(), lookahead);
                }
            }
        }

        let mut need_box = HashMap::new();
        for (name, val) in &rules {
            match val {
                RuleValue::Token(_) => {}
                RuleValue::SeqOrEnum(SeqOrEnum::Seq(seq)) => {
                    seq.calculate_need_box(name, name, &rules, &mut need_box, &mut HashSet::new())
                }
                RuleValue::SeqOrEnum(SeqOrEnum::Enum(e)) => {
                    e.calculate_need_box(name, &rules, &mut need_box, &mut HashSet::new())
                }
            }
        }

        Ok(State::new(
            rules,
            enum_lr,
            left_recursive,
            lookaheads,
            need_box,
        ))
    }
}
