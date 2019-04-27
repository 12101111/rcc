#![recursion_limit = "1024"]
extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::io::Write;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{braced, bracketed, parse_macro_input, token, Pat, PatTupleStruct, Result, Token};
use cfg_if::cfg_if;

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
        let start = Ident::new("_START", Span::call_site());
        nt.push(start.clone());
        nt_map.insert(start, 0);
        let mut rules: Vec<_> = self
            .rules
            .into_iter()
            .map(|r| {
                let lhs = get_or_insert(r.lhs, &mut nt_map, &mut nt);
                let rhs = r
                    .rhs
                    .into_iter()
                    .map(|i| match i {
                        Pat::Ident(syn::PatIdent { ident, .. }) => {
                            Sym::NT(get_or_insert(ident, &mut nt_map, &mut nt))
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
                let i = nt_map.get(&self.start).unwrap();
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

cfg_if! {
    if #[cfg(feature="lr1")]{    
        #[proc_macro]
        pub fn parser(tokens: TokenStream) -> TokenStream {
            parse_macro_input!(tokens as GrammerTokens).to_grammer().lr1()
        }
    } else if #[cfg(feature="slr")]{
        #[proc_macro]
        pub fn parser(tokens: TokenStream) -> TokenStream {
            parse_macro_input!(tokens as GrammerTokens).to_grammer().slr()
        }
    } else if #[cfg(feature="lalr")]{
        #[proc_macro]
        pub fn parser(tokens: TokenStream) -> TokenStream {
            parse_macro_input!(tokens as GrammerTokens).to_grammer().lalr();
            unimplemented!()
        }
    }
}


struct Grammer {
    // 第0条规则总为_START->起点
    rules: Vec<Rule>,
    nt: Vec<Ident>,
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
        Sym::NT(k) => format(grammer.nt[k].clone()),
        Sym::T(k) => {
            if k < grammer.t.len() {
                format(grammer.t[k].clone())
            } else {
                format!("$")
            }
        }
    }
}

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

#[cfg(any(feature="slr",feature="lalr"))]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
struct LR0Item {
    /// 产生式在Grammar的位置
    pub rule: usize,
    /// 点的位置
    pub pos: usize,
}

#[cfg(any(feature="slr",feature="lalr"))]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
struct LR0ItemSet(Vec<LR0Item>);

#[cfg(any(feature="slr",feature="lalr"))]
#[derive(Clone)]
struct LR0FSM(Vec<(LR0ItemSet, BTreeMap<Sym, usize>)>);

#[cfg(any(feature="lr1",feature="lalr"))]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
struct LR1Item {
    /// 产生式在Grammar的位置
    pub rule: usize,
    /// 点的位置
    pub pos: usize,
    /// 向前看符号在T的位置
    /// 只有当pos为最后一个时才有用
    /// 这时只有下一个输入符号为la才进行归约
    /// la是FOLLOW(LHS)的子集
    pub la: usize,
}

#[cfg(any(feature="lr1",feature="lalr"))]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
struct LR1ItemSet(Vec<LR1Item>);

#[cfg(any(feature="lr1",feature="lalr"))]
#[derive(Clone)]
struct LR1FSM(Vec<(LR1ItemSet, BTreeMap<Sym, usize>)>);

impl Grammer {
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

    fn gencode(&self, actions: Vec<Vec<Action>>, gotos: Vec<Vec<Option<usize>>>) -> TokenStream {
        let nts: Vec<_> = self.nt.clone();
        let nt: Vec<_> = self.nt.clone();
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
            const NT:&[Nonterminal] = &[#(Nonterminal::#nt),*];
            const LHS:&[usize] = &[#(#lhs),*];
            const LENS:&[usize] = &[#(#lens),*];
            const TERMINAL_NUM:usize = #empty;
            #[derive(Debug,Clone,Copy)]
            pub enum Nonterminal{
                #(#nts),*
            }
            fn match_token(t:&Token)->Option<usize>{
                match t{
                    #(#tokens=>Some(#values),)*
                    _=>None
                }
            }
        };
        stream.into()
    }
    /// 非终结符标号->FIRST集终结符标号,是否包含空串
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

    #[cfg(feature="slr")]
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

    #[cfg(any(feature="slr",feature="lalr"))]
    fn lr0_closure(&self, core: &LR0ItemSet) -> LR0ItemSet {
        let mut closure: BTreeSet<_> = core.0.iter().cloned().collect();
        let mut to_add: VecDeque<_> = core.0.iter().cloned().collect();
        while let Some(item) = to_add.pop_front() {
            if let Some(&Sym::NT(ref nt)) = self.rules[item.rule].rhs.get(item.pos) {
                for i in 0..self.rules.len() {
                    if &self.rules[i].lhs == nt {
                        let it = LR0Item { rule: i, pos: 0 };
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
        LR0ItemSet(item_set)
    }

    #[cfg(any(feature="slr",feature="lalr"))]
    fn lr0fsm(&self) -> LR0FSM {
        let mut states = Vec::new();
        let mut states_map = BTreeMap::new();
        let mut core_items = BTreeMap::new();
        // 初始项目0
        let cores0 = LR0ItemSet(vec![LR0Item { rule: 0, pos: 0 }]);
        let items0 = self.lr0_closure(&cores0);
        states_map.insert(items0.clone(), 0);
        states.push((items0, BTreeMap::new()));
        core_items.insert(cores0, 0);
        let mut finished = 0;
        while finished < states.len() {
            // 上一个项目集 符号->下一个项目集核心
            let mut next = BTreeMap::new();
            for item in (states[finished].0).0.iter() {
                let rule = &self.rules[item.rule];
                if item.pos < rule.rhs.len() {
                    next.entry(rule.rhs[item.pos])
                        .or_insert(Vec::new())
                        .push(LR0Item {
                            rule: item.rule,
                            pos: item.pos + 1,
                        });
                }
            }
            //根据核心项目集求完整项目集
            for (sym, items) in next.into_iter() {
                let core = LR0ItemSet(items);
                if let Some(&i) = core_items.get(&core) {
                    states[finished].1.insert(sym, i);
                } else {
                    let item_set = self.lr0_closure(&core);
                    let i = match states_map.get(&item_set) {
                        Some(&i) => i,
                        None => {
                            let i = states.len();
                            states_map.insert(item_set.clone(), i);
                            states.push((item_set, BTreeMap::new()));
                            i
                        }
                    };
                    core_items.insert(core, i);
                    states[finished].1.insert(sym, i);
                }
            }
            finished += 1;
        }
        LR0FSM(states)
    }

    #[cfg(feature="slr")]
    fn slr(&self) -> TokenStream {
        println!("Using SLR");
        self.print().unwrap();
        let first = self.first_set();
        println!("FIRST:");
        print_set(&first, self);
        println!("FOLLOW:");
        let follow = self.follow_set(first);
        print_set(&follow, self);
        let lr0fsm = self.lr0fsm();
        lr0fsm.draw(self, "lr0fsm.dot").unwrap();
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
        print_table(&actions, &gotos, self, "slr.csv").unwrap();
        self.gencode(actions, gotos)
    }

    #[cfg(any(feature="lr1",feature="lalr"))]
    fn lr1_closure(
        &self,
        first_set: &Vec<(BTreeSet<usize>, bool)>,
        core: &LR1ItemSet,
    ) -> LR1ItemSet {
        let mut closure: BTreeSet<_> = core.0.iter().cloned().collect();
        let mut to_add: VecDeque<_> = core.0.iter().cloned().collect();
        while let Some(item) = to_add.pop_front() {
            //点之后是非终结符
            if let Some(&Sym::NT(ref nt)) = self.rules[item.rule].rhs.get(item.pos) {
                for i in 0..self.rules.len() {
                    //遍历产生式,如果左边是这个非终结符
                    if &self.rules[i].lhs == nt {
                        let mut first = Vec::new();
                        match self.rules[item.rule].rhs.get(item.pos + 1) {
                            Some(&Sym::NT(nt)) => {
                                first.extend(&first_set[nt].0);
                                if first_set[nt].1 {
                                    first.push(item.la);
                                }
                            }
                            Some(&Sym::T(t)) => first.push(t),
                            None => first.push(item.la),
                        }
                        for b in first {
                            let it = LR1Item {
                                rule: i,
                                pos: 0,
                                la: b,
                            };
                            if !closure.contains(&it) {
                                to_add.push_back(it);
                                closure.insert(it);
                            }
                        }
                    }
                }
            }
        }
        let mut item_set: Vec<_> = closure.into_iter().collect();
        item_set.sort();
        LR1ItemSet(item_set)
    }

    #[cfg(any(feature="lr1",feature="lalr"))]
    fn lr1fsm(&self) -> LR1FSM {
        let mut states = Vec::new();
        let mut states_map = BTreeMap::new();
        let mut core_items = BTreeMap::new();
        let first_set = self.first_set();
        println!("FIRST:");
        print_set(&first_set, self);
        // 初始项目0
        let cores0 = LR1ItemSet(vec![LR1Item {
            rule: 0,
            pos: 0,
            la: self.t.len(),
        }]);
        let items0 = self.lr1_closure(&first_set, &cores0);
        states_map.insert(items0.clone(), 0);
        states.push((items0, BTreeMap::new()));
        core_items.insert(cores0, 0);
        let mut finished = 0;
        while finished < states.len() {
            // 上一个项目集 符号->下一个项目集核心
            let mut next = BTreeMap::new();
            for item in (states[finished].0).0.iter() {
                let rule = &self.rules[item.rule];
                if item.pos < rule.rhs.len() {
                    next.entry(rule.rhs[item.pos])
                        .or_insert(Vec::new())
                        .push(LR1Item {
                            rule: item.rule,
                            pos: item.pos + 1,
                            la: item.la,
                        });
                }
            }
            //根据项目集核心求完整项目集
            for (sym, items) in next.into_iter() {
                let core = LR1ItemSet(items);
                if let Some(&i) = core_items.get(&core) {
                    states[finished].1.insert(sym, i);
                } else {
                    let item_set = self.lr1_closure(&first_set, &core);
                    let i = match states_map.get(&item_set) {
                        Some(&i) => i,
                        None => {
                            let i = states.len();
                            states_map.insert(item_set.clone(), i);
                            states.push((item_set, BTreeMap::new()));
                            i
                        }
                    };
                    core_items.insert(core, i);
                    states[finished].1.insert(sym, i);
                }
            }
            finished += 1;
        }
        LR1FSM(states)
    }

    #[cfg(feature="lr1")]
    fn lr1(&self) -> TokenStream {
        println!("Using LR(1)");
        self.print().unwrap();
        let lr1fsm = self.lr1fsm();
        lr1fsm.draw(self, "lr1fsm.dot").unwrap();
        let mut actions: Vec<Vec<_>> = (0..lr1fsm.0.len())
            .map(|_| (0..self.t.len() + 1).map(|_| Action::Error).collect())
            .collect();
        let mut gotos: Vec<Vec<Option<usize>>> = (0..lr1fsm.0.len())
            .map(|_| (0..self.nt.len()).map(|_| None).collect())
            .collect();
        for (i, (items, trans)) in lr1fsm.0.into_iter().enumerate() {
            for (sym, target) in trans.into_iter() {
                match sym {
                    Sym::T(j) => actions[i][j] = Action::Shift(target),
                    Sym::NT(j) => gotos[i][j] = Some(target),
                }
            }
            for item in items.0.into_iter() {
                if item.pos == self.rules[item.rule].rhs.len() {
                    if item.rule == 0 {
                        assert_eq!(item.la, self.t.len());
                        actions[i][self.t.len()] = Action::Accept;
                        continue;
                    }
                    let f = item.la;
                    match actions[i][f] {
                        Action::Error => actions[i][f] = Action::Reduce(item.rule),
                        Action::Reduce(_) | Action::Accept => {
                            panic!("Reduce-Reduce Conflict at ACTION[{}][{}]", i, f)
                        }
                        Action::Shift(_) => panic!("Shift-Reduce Conflict at ACTION[{}][{}]", i, f),
                    }
                }
            }
        }
        print_table(&actions, &gotos, self, "lr1.csv").unwrap();
        self.gencode(actions, gotos)
    }

    #[cfg(feature="lalr")]
    fn lalr(&self) {
        println!("Using LALR(1)");
        let lr0fsm = self.lr0fsm();
        let core = lr0fsm.to_core_items().0;
        //core.draw(self,"core_lr0fsm").unwrap();
        let first_set = self.first_set();
        let table: Vec<Vec<BTreeSet<usize>>> = core
            .iter()
            .map(|(item_set, _maps)| {
                item_set
                    .0
                    .iter()
                    .map(|item| {
                        self.lr1_closure(
                            &first_set,
                            &LR1ItemSet(vec![LR1Item {
                                rule: item.rule,
                                pos: item.pos,
                                la: usize::max_value(),
                            }]),
                        )
                        .0
                        .into_iter()
                        .map(|b| b.la)
                        //.filter(|b| b != &usize::max_value())
                        .collect()
                    })
                    .collect()
            })
            .collect();
        println!("{:?}", table);
    }
}

fn print_table(
    actions: &Vec<Vec<Action>>,
    gotos: &Vec<Vec<Option<usize>>>,
    grammer: &Grammer,
    filename: &str,
) -> std::io::Result<()> {
    let mut f = std::fs::File::create(filename)?;
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

#[cfg(any(feature="slr",feature="lalr"))]
impl LR0FSM {
    /// Print the state machine in graphviz format.
    pub fn draw(&self, grammer: &Grammer, filename: &str) -> std::io::Result<()> {
        let mut f = std::fs::File::create(filename)?;
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

    #[cfg(feature="lalr")]
    pub fn to_core_items(self) -> LR0FSM {
        let states = self
            .0
            .into_iter()
            .map(|(items, map)| {
                let core_items = items.0.into_iter().filter(|item| item.pos != 0).collect();
                (LR0ItemSet(core_items), map)
            })
            .collect();
        LR0FSM(states)
    }
}

#[cfg(any(feature="lr1",feature="lalr"))]
impl LR1FSM {
    pub fn draw(&self, grammer: &Grammer, filename: &str) -> std::io::Result<()> {
        let mut f = std::fs::File::create(filename)?;
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
                write!(f, ",")?;
                write!(f, "{}", format_sym(Sym::T(item.la), grammer))?;
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
    #[cfg(feature="lalr")]
    pub fn to_lalr(self){
        //TODO:
        unimplemented!()
    }
}
