#![recursion_limit = "2048"]
extern crate proc_macro;

use cfg_if::cfg_if;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::io::Write;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{
    parenthesized, parse_macro_input, token, Block, Pat, PatTupleStruct, Result, Token, Type,
};

struct GrammarTokens {
    rules: Vec<RuleTokens>,
}

impl Parse for GrammarTokens {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(GrammarTokens {
            rules: {
                let mut rules = Vec::new();
                while !input.is_empty() {
                    rules.push(input.parse()?);
                }
                rules
            },
        })
    }
}

struct RuleTokens {
    _fn: Token![fn],
    lhs: Ident,
    _paren: token::Paren,
    rhs: Punctuated<SymToken, Token![,]>,
    _arrow: Token![->],
    ty: Type,
    func: Block,
}

impl Parse for RuleTokens {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(RuleTokens {
            _fn: input.parse()?,
            lhs: input.parse()?,
            _paren: parenthesized!(content in input),
            rhs: content.parse_terminated(SymToken::parse)?,
            _arrow: input.parse()?,
            ty: input.parse()?,
            func: input.parse()?,
        })
    }
}

struct SymToken {
    ident: Ident,
    _colon: Token![:],
    pat: Pat,
}

impl Parse for SymToken {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(SymToken {
            ident: input.parse()?,
            _colon: input.parse()?,
            pat: input.parse()?,
        })
    }
}

fn format<T: quote::ToTokens>(i: T) -> String {
    let stream = quote! {#i};
    format!("{}", stream)
}

impl GrammarTokens {
    pub fn into_grammar(self) -> Grammar {
        let mut rules = Vec::new();
        let mut funcs = Vec::new();
        let mut nt = Vec::new();
        let mut nt_types = Vec::new();
        let mut t = Vec::new();
        let mut nt_map = HashMap::new();
        let mut t_map = HashMap::new();
        let start = Ident::new("_START", Span::call_site());
        nt.push(start.clone());
        nt_types.push(Type::Tuple(syn::TypeTuple {
            paren_token: syn::token::Paren {
                span: Span::call_site(),
            },
            elems: Punctuated::new(),
        }));
        nt_map.insert(start, 0);
        funcs.push(Block {
            brace_token: syn::token::Brace {
                span: Span::call_site(),
            },
            stmts: vec![syn::Stmt::Expr(syn::Expr::Tuple(syn::ExprTuple {
                attrs: Vec::new(),
                paren_token: syn::token::Paren {
                    span: Span::call_site(),
                },
                elems: Punctuated::new(),
            }))],
        });
        for rule in self.rules.iter() {
            match nt_map.get(&rule.lhs) {
                Some(&i) => {
                    if nt_types[i] != rule.ty {
                        panic!(
                            "Nonterminal {} map to multiple parsed types: {} and {}",
                            format(rule.lhs.clone()),
                            format(nt_types[i].clone()),
                            format(rule.ty.clone())
                        );
                    }
                }
                None => {
                    let i = nt.len();
                    nt_map.insert(rule.lhs.clone(), i);
                    nt.push(rule.lhs.clone());
                    nt_types.push(rule.ty.clone());
                }
            }
        }
        rules.push(Rule {
            lhs: 0,
            rhs: vec![(Sym::NT(1), Ident::new("_start", Span::call_site()))],
        });
        for rule in self.rules.into_iter() {
            funcs.push(rule.func);
            rules.push(Rule {
                lhs: *nt_map
                    .get(&rule.lhs)
                    .expect(&format!("Can't find rule for {}", format(rule.lhs.clone()))),
                rhs: rule
                    .rhs
                    .into_iter()
                    .map(|sym| {
                        (
                            match sym.pat {
                                Pat::Ident(syn::PatIdent { ident, .. }) => {
                                    Sym::NT(*nt_map.get(&ident).expect(&format!(
                                        "Can't find rule for {}",
                                        format(ident.clone())
                                    )))
                                }
                                Pat::TupleStruct(pat) => Sym::T(match t_map.get(&pat) {
                                    Some(&i) => i,
                                    None => {
                                        let i = t.len();
                                        t_map.insert(pat.clone(), i);
                                        t.push(pat);
                                        i
                                    }
                                }),
                                _ => panic!("Unexpect Type,{:#?}", sym.pat),
                            },
                            sym.ident,
                        )
                    })
                    .collect(),
            });
        }
        Grammar {
            rules,
            funcs,
            nt,
            nt_types,
            t,
        }
    }
}

#[proc_macro]
pub fn parser(tokens: TokenStream) -> TokenStream {
    use std::io::Read;
    use std::str::FromStr;
    let mut file = format(proc_macro2::TokenStream::from(tokens));
    let _ = file.pop();
    let root = env!("CARGO_MANIFEST_DIR");
    let file = format!("{}/../src/{}", root, &file[1..]);
    println!("{}", file);
    let mut file = std::fs::File::open(&file).expect("Unable to open file");
    let mut src = String::new();
    file.read_to_string(&mut src).expect("Unable to read file");
    let syntax = TokenStream::from_str(&src).expect("Unable to parse file");
    parse(syntax)
}

cfg_if! {
    if #[cfg(feature="lr1")]{
        fn parse(tokens: TokenStream) -> TokenStream {
            parse_macro_input!(tokens as GrammarTokens).into_grammar().lr1()
        }
    } else if #[cfg(feature="slr")]{
        fn parse(tokens: TokenStream) -> TokenStream {
            parse_macro_input!(tokens as GrammarTokens).into_grammar().slr()
        }
    } else if #[cfg(feature="lalr")]{
        fn parse(tokens: TokenStream) -> TokenStream {
            parse_macro_input!(tokens as GrammarTokens).into_grammar().lalr();
            unimplemented!()
        }
    } else{
        fn parse(_tokens: TokenStream) -> TokenStream {
            panic!("Select a feature to use!")
        }
    }
}

struct Grammar {
    // 第0条规则总为_START->起点
    rules: Vec<Rule>,
    funcs: Vec<Block>,
    nt: Vec<Ident>,
    nt_types: Vec<Type>,
    t: Vec<PatTupleStruct>,
}

struct Rule {
    lhs: usize,
    rhs: Vec<(Sym, Ident)>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
enum Sym {
    NT(usize),
    T(usize),
}

fn format_sym(sym: Sym, grammar: &Grammar) -> String {
    match sym {
        Sym::NT(k) => format(grammar.nt[k].clone()),
        Sym::T(k) => {
            if k < grammar.t.len() {
                format(grammar.t[k].clone())
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

#[cfg(any(feature = "slr", feature = "lalr"))]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
struct LR0Item {
    /// 产生式在Grammar的位置
    pub rule: usize,
    /// 点的位置
    pub pos: usize,
}

#[cfg(any(feature = "slr", feature = "lalr"))]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
struct LR0ItemSet(Vec<LR0Item>);

#[cfg(any(feature = "slr", feature = "lalr"))]
#[derive(Clone)]
struct LR0FSM(Vec<(LR0ItemSet, BTreeMap<Sym, usize>)>);

#[cfg(any(feature = "lr1", feature = "lalr"))]
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

#[cfg(any(feature = "lr1", feature = "lalr"))]
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
struct LR1ItemSet(Vec<LR1Item>);

#[cfg(any(feature = "lr1", feature = "lalr"))]
#[derive(Clone)]
struct LR1FSM(Vec<(LR1ItemSet, BTreeMap<Sym, usize>)>);

impl Grammar {
    fn print(&self) -> std::io::Result<()> {
        let mut f = std::fs::File::create("grammar.txt")?;
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
            for &(j, _) in &i.rhs {
                write!(f, "{}, ", format_sym(j, self))?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }

    fn gen_code(&self, actions: Vec<Vec<Action>>, gotos: Vec<Vec<Option<usize>>>) -> TokenStream {
        let nts1: Vec<_> = self.nt.clone();
        let nts2: Vec<_> = self.nt.clone();
        let nt_string: Vec<_> = self.nt.clone().into_iter().map(|nt| format(nt)).collect();
        let types: Vec<_> = self.nt_types.clone();
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
        let mut func = Vec::new();
        let funcs: Vec<_> = self
            .funcs
            .clone()
            .into_iter()
            .enumerate()
            .map(|(i, block)| {
                let fn_ident = Ident::new(&format!("_f{}", i), Span::call_site());
                func.push(fn_ident.clone());
                let return_type = self.nt_types[self.rules[i].lhs].clone();
                let variant = self.nt[self.rules[i].lhs].clone();
                let idents: Vec<_> = self.rules[i]
                    .rhs
                    .clone()
                    .into_iter()
                    .enumerate()
                    .map(|(i, (sym, ident))| match sym {
                        Sym::NT(nt) => {
                            let return_type = self.nt_types[nt].clone();
                            let variant = self.nt[nt].clone();
                            quote! {
                                let mut #ident: #return_type = match child[#i].clone(){
                                    Ast::NT(node)=>{
                                        match node.lhs{
                                            Nonterminal::#variant(i)=> *i,
                                            _=>unreachable!()
                                        }
                                    }
                                    _=>unreachable!()
                                };
                            }
                        }
                        Sym::T(_) => {
                            quote! {
                                let #ident = match child[#i].clone(){
                                    Ast::T(token)=>token,
                                    _=>unreachable!()
                                };
                            }
                        }
                    })
                    .collect();
                quote! {
                    fn #fn_ident(child:&[Ast])->Nonterminal{
                        #(#idents)*
                        let val: #return_type = #block;
                        Nonterminal::#variant(Box::new(val))
                    }
                }
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
            const LHS:&[usize] = &[#(#lhs),*];
            const LENS:&[usize] = &[#(#lens),*];
            const FUNC:&[fn(&[Ast])->Nonterminal] = &[#(#func),*];
            const TERMINAL_NUM:usize = #empty;
            #[derive(Clone)]
            pub enum Nonterminal{
                #(#nts1(Box<#types>)),*
            }
            impl std::fmt::Debug for Nonterminal{
                fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    use Nonterminal::*;
                    match self{
                        #(#nts2(_)=>write!(f,#nt_string)),*
                    }
                }
            }
            fn match_token(t:&Token)->Option<usize>{
                match t{
                    #(#tokens=>Some(#values),)*
                    _=>None
                }
            }
            #(#funcs)*
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
                for (sym, _) in rule.rhs.iter() {
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

    #[cfg(feature = "slr")]
    fn follow_set(&self, first_set: Vec<(BTreeSet<usize>, bool)>) -> Vec<(BTreeSet<usize>, bool)> {
        let mut ans: Vec<_> = (0..self.nt.len())
            .map(|_| (BTreeSet::new(), false))
            .collect();
        if let &Sym::NT(i) = &self.rules[0].rhs[0].0 {
            ans[i].1 = true;
        } else {
            panic!("")
        }
        loop {
            let mut stop = true;
            for rule in self.rules.iter() {
                let mut follow = ans[rule.lhs].clone();
                for (sym, _ident) in rule.rhs.iter().rev() {
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

    #[cfg(any(feature = "slr", feature = "lalr"))]
    fn lr0_closure(&self, core: &LR0ItemSet) -> LR0ItemSet {
        let mut closure: BTreeSet<_> = core.0.iter().cloned().collect();
        let mut to_add: VecDeque<_> = core.0.iter().cloned().collect();
        while let Some(item) = to_add.pop_front() {
            if let Some(&(Sym::NT(ref nt), _)) = self.rules[item.rule].rhs.get(item.pos) {
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

    #[cfg(any(feature = "slr", feature = "lalr"))]
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
                    next.entry(rule.rhs[item.pos].0)
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

    #[cfg(feature = "slr")]
    fn slr(&self) -> TokenStream {
        println!("Using SLR");
        self.print().unwrap();
        let first = self.first_set();
        print_set(&first, self, "first_set.txt").unwrap();
        let follow = self.follow_set(first);
        print_set(&follow, self, "follow_set.txt").unwrap();
        let lr0fsm = self.lr0fsm();
        lr0fsm.draw(self, "lr0fsm.dot").unwrap();
        let core = lr0fsm.clone().into_core_items().0;
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
                            Action::Reduce(j) => panic!(
                                "Reduce-Reduce Conflict at ACTION[{}][{}], input:{}, old:R{}, new:R{}, actions:{:?}",
                                i, *f, format(self.t[*f].clone()), j, item.rule,actions
                            ),
                            Action::Shift(j) => {
                                let shift = &(core[j].0).0;
                                if shift.len() == 1 {
                                    if item.rule <= shift[0].rule{
                                        actions[i][*f] = Action::Reduce(item.rule);
                                    }
                                }else{
                                    panic!(
                                        "Shift-Reduce Conflict at ACTION[{}][{}], input:{}, old:S{}, new:R{}, actions:{:?}",
                                        i, *f, format(self.t[*f].clone()), j, item.rule,actions
                                    );
                                }
                            },
                            Action::Accept => unreachable!(),
                        }
                    }
                    if follow.1 {
                        actions[i][self.t.len()] = Action::Reduce(item.rule)
                    }
                }
            }
        }
        print_table(&actions, &gotos, self, "slr.csv").unwrap();
        self.gen_code(actions, gotos)
    }

    #[cfg(any(feature = "lr1", feature = "lalr"))]
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

    #[cfg(any(feature = "lr1", feature = "lalr"))]
    fn lr1fsm(&self) -> LR1FSM {
        let mut states = Vec::new();
        let mut states_map = BTreeMap::new();
        let mut core_items = BTreeMap::new();
        let first_set = self.first_set();
        print_set(&first_set, self, "first_set.txt").unwrap();
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

    #[cfg(feature = "lr1")]
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
                        Action::Reduce(j) => panic!(
                            "Reduce-Reduce Conflict at ACTION[{}][{}], input:{}, old:R{}, new:R{}, actions:{:?}",
                            i, f, format(self.t[f].clone()), j, item.rule,actions
                        ),
                        // FIXME: core
                        Action::Shift(j) => {
                            panic!("There is some bug in LR(1),use SLR instead")
                            /*let shift = &(core[j].0).0;
                            if shift.len() == 1 {
                                if item.rule <= shift[0].rule{
                                    actions[i][f] = Action::Reduce(item.rule);
                                }
                            }else{
                                panic!(
                                    "Shift-Reduce Conflict at ACTION[{}][{}], input:{}, old:S{}, new:R{}, actions:{:?}",
                                    i, f, format(self.t[f].clone()), j, item.rule,actions
                                );
                            }*/
                        },
                        Action::Accept => unreachable!(),
                    }
                }
            }
        }
        print_table(&actions, &gotos, self, "lr1.csv").unwrap();
        self.gen_code(actions, gotos)
    }

    #[cfg(feature = "lalr")]
    fn lalr(&self) {
        println!("Using LALR(1)");
        let lr0fsm = self.lr0fsm();
        let core = lr0fsm.into_core_items().0;
        //core.draw(self,"core_lr0fsm.dot").unwrap();
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
    grammar: &Grammar,
    filename: &str,
) -> std::io::Result<()> {
    let mut f = std::fs::File::create(filename)?;
    write!(f, "S,")?;
    for t in &grammar.t {
        write!(f, "{},", format(t))?;
    }
    write!(f, "$,")?;
    for nt in &grammar.nt {
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

fn print_set(
    set: &Vec<(BTreeSet<usize>, bool)>,
    grammar: &Grammar,
    filename: &str,
) -> std::io::Result<()> {
    let mut f = std::fs::File::create(filename)?;
    for (id, (set, empty)) in set.iter().enumerate() {
        write!(f, "{}:{{ ", grammar.nt[id])?;
        for i in set.iter() {
            write!(f, "{}, ", format(&grammar.t[*i]))?;
        }
        if *empty {
            write!(f, "ε ")?;
        }
        write!(f, "}}\n")?;
    }
    Ok(())
}

#[cfg(any(feature = "slr", feature = "lalr"))]
impl LR0FSM {
    /// Print the state machine in graphviz format.
    pub fn draw(&self, grammar: &Grammar, filename: &str) -> std::io::Result<()> {
        let mut f = std::fs::File::create(filename)?;
        write!(
            f,
            "digraph G {{\nnode [shape=\"box\",style=\"rounded\",penwidth=1,width=2.0];"
        )?;
        for (i, &(ref item_set, ref trans)) in self.0.iter().enumerate() {
            write!(f, "s{}[label=\"s{}\\n", i, i)?;
            for item in item_set.0.iter() {
                let rule = &grammar.rules[item.rule];
                write!(f, "{}->", grammar.nt[rule.lhs])?;
                for j in 0..item.pos {
                    write!(f, "{} ", format_sym(rule.rhs[j].0, grammar))?;
                }
                write!(f, ".")?;
                for j in item.pos..rule.rhs.len() {
                    write!(f, "{} ", format_sym(rule.rhs[j].0, grammar))?;
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
                    format_sym(sym, grammar)
                )?;
            }
        }
        write!(f, "}}\n")?;
        Ok(())
    }

    pub fn into_core_items(self) -> LR0FSM {
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

#[cfg(any(feature = "lr1", feature = "lalr"))]
impl LR1FSM {
    pub fn draw(&self, grammer: &Grammar, filename: &str) -> std::io::Result<()> {
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
    #[cfg(feature = "lalr")]
    pub fn into_lalr(self) {
        //TODO:
        unimplemented!()
    }
}
