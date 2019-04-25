#![recursion_limit = "1024"]
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::io::Write;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{braced, bracketed, parse_macro_input, token, Ident, Pat, PatTupleStruct, Result, Token};

struct GrammerTokens {
    start: Ident,
    _brace_token: token::Brace,
    rules: Punctuated<RuleTokens, Token![,]>,
}

impl Parse for GrammerTokens {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(GrammerTokens {
            start: input.parse()?,
            _brace_token: braced!(content in input),
            rules: content.parse_terminated(RuleTokens::parse)?,
        })
    }
}

fn format<T: quote::ToTokens>(i: T) -> String {
    let stream = quote! {#i};
    format!("{}", stream)
}

fn get_or_insert<T>(ele: T, map: &mut HashMap<T, usize>, vec: &mut Vec<T>) -> usize
where
    T: Eq + std::hash::Hash + Clone,
{
    match map.get(&ele) {
        Some(&i) => i,
        None => {
            let i = vec.len();
            map.insert(ele.clone(), i);
            vec.push(ele);
            i
        }
    }
}

impl GrammerTokens {
    pub fn to_grammer(self) -> Grammer {
        let mut nt_map = HashMap::new();
        let mut nt = Vec::new();
        let mut t_map = HashMap::new();
        let mut t = Vec::new();
        nt.push("_START".to_owned());
        nt_map.insert("_START".to_owned(), 0);
        let mut rules: Vec<_> = self
            .rules
            .into_iter()
            .map(|r| {
                let lhs = get_or_insert(format(r.lhs), &mut nt_map, &mut nt);
                let rhs = r
                    .rhs
                    .into_iter()
                    .map(|i| match i {
                        Pat::Ident(syn::PatIdent { ident, .. }) => {
                            Sym::NT(get_or_insert(format(ident), &mut nt_map, &mut nt))
                        }
                        Pat::TupleStruct(pat) => Sym::T(get_or_insert(pat, &mut t_map, &mut t)),
                        _ => panic!("Unexpect Type,{:#?}", i),
                    })
                    .collect();
                Rule { lhs, rhs }
            })
            .collect();
        let rule = Rule {
            lhs: 0,
            rhs: {
                let i = nt_map.get(&format(self.start)).unwrap();
                vec![Sym::NT(*i)]
            },
        };
        rules.insert(0, rule);
        Grammer { rules, nt, t }
    }
}

struct RuleTokens {
    lhs: Ident,
    _bracket_token: token::Bracket,
    rhs: Punctuated<Pat, Token![,]>,
}

impl Parse for RuleTokens {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(RuleTokens {
            lhs: input.parse()?,
            _bracket_token: bracketed!(content in input),
            rhs: content.parse_terminated(Pat::parse)?,
        })
    }
}

#[proc_macro]
pub fn parser(tokens: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(tokens as GrammerTokens);
    let grammer = ast.to_grammer();
    grammer.to_slr()
}

struct Grammer {
    // 第0条规则总为_START->起点
    rules: Vec<Rule>,
    nt: Vec<String>,
    t: Vec<PatTupleStruct>,
}

struct Rule {
    lhs: usize,
    rhs: Vec<Sym>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
enum Sym {
    NT(usize),
    T(usize),
}

fn format_sym(sym: Sym, grammer: &Grammer) -> String {
    match sym {
        Sym::NT(k) => format!("{}", grammer.nt[k]),
        Sym::T(k) => format(grammer.t[k].clone()),
    }
}

/// 项
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
struct Item {
    /// 产生式在Grammar的位置
    pub rule: usize,
    /// 点的位置
    pub pos: usize,
}
/// 项集
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
struct ItemSet(Vec<Item>);

struct LR0FSM(Vec<(ItemSet, BTreeMap<Sym, usize>)>);

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
enum Action {
    // 移进(状态)
    Shift(usize),
    // 归约(产生式)
    Reduce(usize),
    Accept,
    Error,
}

impl std::fmt::Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Action::Shift(ref s) => write!(f, "s{} ", s),
            Action::Reduce(ref r) => write!(f, "r{} ", r),
            Action::Accept => write!(f, "acc"),
            Action::Error => write!(f, "   "),
        }
    }
}

impl Grammer {
    fn to_lr0fsm(&self) -> LR0FSM {
        struct Tmp {
            states: Vec<(ItemSet, BTreeMap<Sym, usize>)>,
            states_map: BTreeMap<ItemSet, usize>,
            raw_items: BTreeMap<ItemSet, usize>,
        }
        impl Tmp {
            fn closure(&mut self, grammer: &Grammer, items: ItemSet) -> usize {
                if let Some(&i) = self.raw_items.get(&items) {
                    return i;
                }
                let mut closure: BTreeSet<_> = items.0.iter().cloned().collect();
                let mut to_add: VecDeque<_> = items.0.iter().cloned().collect();
                while let Some(item) = to_add.pop_front() {
                    if let Some(&Sym::NT(ref nt)) = grammer.rules[item.rule].rhs.get(item.pos) {
                        for i in 0..grammer.rules.len() {
                            if &grammer.rules[i].lhs == nt {
                                let it = Item { rule: i, pos: 0 };
                                if !closure.contains(&it) {
                                    to_add.push_back(it);
                                    closure.insert(it);
                                }
                            }
                        }
                    }
                }
                let mut item_set: Vec<_> = closure.into_iter().collect();
                item_set.sort();
                let item_set = ItemSet(item_set);
                let i = match self.states_map.get(&item_set) {
                    Some(&i) => i,
                    None => {
                        let i = self.states.len();
                        self.states_map.insert(item_set.clone(), i);
                        self.states.push((item_set, BTreeMap::new()));
                        i
                    }
                };
                self.raw_items.insert(items, i);
                i
            }
        }
        let mut state = Tmp {
            states: Vec::new(),
            states_map: BTreeMap::new(),
            raw_items: BTreeMap::new(),
        };
        // 对增广文法的新产生式求闭包
        state.closure(self, ItemSet(vec![Item { rule: 0, pos: 0 }]));
        let mut finished = 0;
        while finished < state.states.len() {
            let mut next = BTreeMap::new();
            //上一个项目集中的项目
            for item in (state.states[finished].0).0.iter() {
                let rule = &self.rules[item.rule];
                if item.pos < rule.rhs.len() {
                    next.entry(rule.rhs[item.pos])
                        .or_insert(Vec::new())
                        .push(Item {
                            rule: item.rule,
                            pos: item.pos + 1,
                        });
                }
            }
            for (sym, items) in next.into_iter() {
                let index = state.closure(self, ItemSet(items));
                state.states[finished].1.insert(sym, index);
            }
            finished += 1;
        }
        LR0FSM(state.states)
    }
    /// 非终结符标号->FIRST集终结符标号,是包含空串
    fn first_set(&self) -> Vec<(BTreeSet<usize>, bool)> {
        let mut ans = Vec::new();
        for _ in 0..self.nt.len() {
            ans.push((BTreeSet::new(), false));
        }
        loop {
            let mut stop = true;
            'rule: for rule in self.rules.iter() {
                for sym in rule.rhs.iter() {
                    match sym {
                        Sym::T(i) => {
                            if ans[rule.lhs].0.insert(*i) {
                                stop = false;
                            }
                            continue 'rule;
                        }
                        Sym::NT(i) => {
                            if *i == rule.lhs {
                                // 不含空串
                                if !ans[rule.lhs].1 {
                                    continue 'rule;
                                }
                            } else {
                                for j in &ans[*i].0.clone() {
                                    if ans[rule.lhs].0.insert(*j) {
                                        stop = false;
                                    }
                                }
                                if !ans[*i].1 {
                                    continue 'rule;
                                }
                            }
                        }
                    }
                }
                // 没有遇到continue
                if !ans[rule.lhs].1 {
                    ans[rule.lhs].1 = true;
                    stop = false;
                }
            }
            if stop {
                break;
            }
        }
        ans
    }

    fn follow_set(&self, first_set: Vec<(BTreeSet<usize>, bool)>) -> Vec<(BTreeSet<usize>, bool)> {
        let mut ans: Vec<_> = (0..self.nt.len())
            .map(|_| (BTreeSet::new(), false))
            .collect();
        if let &Sym::NT(i) = &self.rules[0].rhs[0] {
            ans[i].1 = true;
        } else {
            panic!("")
        }
        loop {
            let mut stop = true;
            for rule in self.rules.iter() {
                let mut follow = ans[rule.lhs].clone();
                for sym in rule.rhs.iter().rev() {
                    match sym {
                        Sym::T(i) => {
                            follow.0.clear();
                            follow.0.insert(*i);
                            follow.1 = false;
                        }
                        Sym::NT(i) => {
                            let i = *i;
                            for t in follow.0.iter() {
                                if ans[i].0.insert(*t) {
                                    stop = false;
                                }
                            }
                            if !ans[i].1 && follow.1 {
                                ans[i].1 = true;
                                stop = false;
                            }
                            if !first_set[i].1 {
                                follow.0.clear();
                                follow.1 = false;
                            }
                            for j in &first_set[i].0 {
                                follow.0.insert(*j);
                            }
                        }
                    }
                }
            }
            if stop {
                break;
            }
        }
        ans
    }

    fn to_slr(&self) -> TokenStream {
        self.print().unwrap();
        let first = self.first_set();
        //println!("FIRST:");
        //print_set(&first, self);
        //println!("FOLLOW:");
        let follow = self.follow_set(first);
        //print_set(&follow, self);
        let lr0fsm = self.to_lr0fsm();
        lr0fsm.draw(self).unwrap();
        let mut actions: Vec<Vec<_>> = (0..lr0fsm.0.len())
            .map(|_| (0..self.t.len() + 1).map(|_| Action::Error).collect())
            .collect();
        let mut gotos: Vec<Vec<Option<usize>>> = (0..lr0fsm.0.len())
            .map(|_| (0..self.nt.len()).map(|_| None).collect())
            .collect();
        for (i, (items, trans)) in lr0fsm.0.into_iter().enumerate() {
            for (sym, target) in trans.into_iter() {
                match sym {
                    Sym::T(j) => actions[i][j] = Action::Shift(target),
                    Sym::NT(j) => gotos[i][j] = Some(target),
                }
            }
            for item in items.0.into_iter() {
                if item.pos == self.rules[item.rule].rhs.len() {
                    if item.rule == 0 {
                        actions[i][self.t.len()] = Action::Accept;
                        continue;
                    }
                    let follow = &follow[self.rules[item.rule].lhs];
                    for f in &follow.0 {
                        match actions[i][*f] {
                            Action::Error => actions[i][*f] = Action::Reduce(item.rule),
                            Action::Reduce(_) | Action::Accept => {
                                panic!("Reduce-Reduce Conflict at ACTION[{}][{}]", i, *f)
                            }
                            Action::Shift(_) => {
                                panic!("Shift-Reduce Conflict at ACTION[{}][{}]", i, *f)
                            }
                        }
                    }
                    if follow.1 {
                        actions[i][self.t.len()] = Action::Reduce(item.rule)
                    }
                }
            }
        }
        print_table(&actions, &gotos, self).unwrap();
        self.gencode(actions, gotos)
    }

    fn gencode(&self, actions: Vec<Vec<Action>>, gotos: Vec<Vec<Option<usize>>>) -> TokenStream {
        let nts: Vec<_> = self.nt.clone();
        let actions: Vec<_> = actions
            .into_iter()
            .map(|action| {
                let actions = action.into_iter().map(|a| match a {
                    Action::Shift(i) => quote! {Action::Shift(#i)},
                    Action::Reduce(i) => quote! {Action::Reduce(#i)},
                    Action::Accept => quote! {Action::Accept},
                    Action::Error => quote! {Action::Error},
                });
                quote! {&[#(#actions),*]}
            })
            .collect();
        let gotos: Vec<_> = gotos
            .into_iter()
            .map(|goto| {
                let gotos = goto.into_iter().map(|a| match a {
                    Some(a) => quote! {Some(#a)},
                    None => quote! {None},
                });
                quote! {&[#(#gotos),*]}
            })
            .collect();
        let lhs: Vec<_> = self.rules.iter().map(|r| r.lhs).collect();
        let lens: Vec<_> = self.rules.iter().map(|r| r.rhs.len()).collect();
        let tokens = self.t.clone();
        let values: Vec<_> = (0..self.t.len()).collect();
        let empty = self.t.len();
        let stream = quote! {
            const GOTOS:&[&[Option<usize>]] = &[#(#gotos),*];
            const ACTIONS:&[&[Action]] = &[#(#actions),*];
            const NTS:&[&str] = &[#(#nts),*];
            const LHS:&[usize] = &[#(#lhs),*];
            const LENS:&[usize] = &[#(#lens),*];
            const TERMINAL_NUM:usize = #empty;
            fn match_token(t:&Token)->Option<usize>{
                match t{
                    #(#tokens=>Some(#values),)*
                    _=>None
                }
            }
        };
        stream.into()
    }

    fn print(&self) -> std::io::Result<()> {
        let mut f = std::fs::File::create("grammer.txt")?;
        write!(f, "Nonterminal:\n")?;
        for i in &self.nt {
            write!(f, "{} ", i)?;
        }
        write!(f, "\nTerminal:\n")?;
        for i in &self.t {
            write!(f, "{}, ", format(i))?;
        }
        write!(f, "\nRules:\n")?;
        for i in &self.rules {
            write!(f, "{}->", self.nt[i.lhs])?;
            for &j in &i.rhs {
                write!(f, "{}, ", format_sym(j, self))?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

fn print_table(
    actions: &Vec<Vec<Action>>,
    gotos: &Vec<Vec<Option<usize>>>,
    grammer: &Grammer,
) -> std::io::Result<()> {
    let mut f = std::fs::File::create("table.csv")?;
    write!(f, "S,")?;
    for t in &grammer.t {
        write!(f, "{},", format(t))?;
    }
    for nt in &grammer.nt {
        write!(f, "{},", nt)?;
    }
    write!(f, "\n")?;
    for (i, (actions, gotos)) in actions.iter().zip(gotos.iter()).enumerate() {
        write!(f, "{},", i)?;
        for action in actions.iter() {
            write!(f, "{},", action)?;
        }
        for goto in gotos.iter() {
            match goto {
                Some(i) => write!(f, "{},", i)?,
                None => write!(f, " ,")?,
            }
        }
        write!(f, "\n")?;
    }
    Ok(())
}

fn print_set(set: &Vec<(BTreeSet<usize>, bool)>, grammer: &Grammer) {
    for (id, (set, empty)) in set.iter().enumerate() {
        print!("{}:{{ ", grammer.nt[id]);
        for i in set.iter() {
            print!("{}, ", format(&grammer.t[*i]));
        }
        if *empty {
            print!("ε ")
        }
        println!("}}");
    }
}

impl LR0FSM {
    /// Print the state machine in graphviz format.
    pub fn draw(&self, grammer: &Grammer) -> std::io::Result<()> {
        let mut f = std::fs::File::create("lr0fsm.dot")?;
        write!(
            f,
            "digraph G {{\nnode [shape=\"box\",style=\"rounded\",penwidth=1,width=2.0];"
        )?;
        for (i, &(ref item_set, ref trans)) in self.0.iter().enumerate() {
            write!(f, "s{}[label=\"s{}\\n", i, i)?;
            for item in item_set.0.iter() {
                let rule = &grammer.rules[item.rule];
                write!(f, "{}->", grammer.nt[rule.lhs])?;
                for j in 0..item.pos {
                    write!(f, "{} ", format_sym(rule.rhs[j], grammer))?;
                }
                write!(f, ".")?;
                for j in item.pos..rule.rhs.len() {
                    write!(f, "{} ", format_sym(rule.rhs[j], grammer))?;
                }
                write!(f, "\\n")?;
            }
            write!(f, "\"]\n")?;
            for (&sym, &target) in trans.iter() {
                write!(
                    f,
                    "s{} -> s{} [label=<{}>]\n",
                    i,
                    target,
                    format_sym(sym, grammer)
                )?;
            }
        }
        write!(f, "}}\n")?;
        Ok(())
    }
}
