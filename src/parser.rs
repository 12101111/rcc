use crate::ir::*;
use crate::lexer::KeyWord::*;
use crate::lexer::Lexer;
use crate::lexer::Symbol::*;
use crate::lexer::Token::{self, *};
use parser::parser;
use std::collections::{HashMap, VecDeque};
use std::io::Write;

macro_rules! fail {
    ($fmt:expr, $($arg:tt)+) => {
        println!();
        eprintln!($fmt, $($arg)+);
        std::process::exit(1);
    };
}

parser! {"syntax.rs"}

enum Action {
    // 移进(状态)
    Shift(usize),
    // 归约(产生式)
    Reduce(usize),
    Accept,
    Error,
}

#[derive(Debug, Clone)]
pub enum Ast {
    T(Token),
    NT(Box<Node>),
}

#[derive(Debug, Clone)]
pub struct Node {
    pub lhs: Nonterminal,
    pub rule: usize,
    pub child: Vec<Ast>,
}

impl Ast {
    pub fn unwrap_program(self) -> crate::ir::Program {
        let node = match self {
            Ast::NT(node) => *node,
            _ => unreachable!(),
        };
        match node.lhs {
            Nonterminal::Program(p) => *p,
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Ast::T(t) => write!(f, "{}", t),
            Ast::NT(node) => write!(f, "{}", node),
        }
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}:{{", self.lhs)?;
        for i in &self.child {
            write!(f, "{} ", i)?;
        }
        write!(f, "}}")
    }
}

#[cfg(test)]
pub fn fail(msg: &str, line: usize, col: usize) -> ! {
    panic!("\nSyntax Error: {}\nLine:{} ,Col:{}\n", msg, line, col);
}

#[cfg(not(test))]
pub fn fail(msg: &str, line: usize, col: usize) -> ! {
    println!();
    eprintln!("Syntax Error: {}\nLine:{} ,Col:{}", msg, line, col);
    std::process::exit(1)
}

pub fn parse(lexer: Lexer) -> Ast {
    let mut states = Vec::new();
    states.push(0);
    let mut ast = Vec::new();
    // Ignore Comment
    let mut lex = lexer
        .filter(|t| {
            if let (Token::Comment(_), _, _) = t {
                false
            } else {
                true
            }
        })
        .peekable();
    let (mut line, mut col) = (0, 0);
    let mut input = String::new();
    loop {
        print!("{:?}\t", states);
        print!("input:");
        let i = lex
            .peek()
            .map(|(i, l, c)| {
                line = *l;
                col = *c;
                input = format!("{}", i);
                print!(" {}\t", i);
                match_token(i).unwrap_or_else(|| {
                    fail(&format!("no match token for this input:{}", i), *l, *c)
                })
            })
            .unwrap_or(TERMINAL_NUM);
        match ACTIONS[*states.last().unwrap()][i] {
            Action::Shift(i) => {
                println!("S{}", i);
                states.push(i);
                ast.push(Ast::T(lex.next().unwrap().0))
            }
            Action::Reduce(rule) => {
                print!("R{}", rule);
                let len = ast.len() - LENS[rule];
                let child = ast.split_off(len);
                states.truncate(states.len() - LENS[rule]);
                print!("\tReduces: ");
                for j in &child {
                    match j {
                        Ast::T(t) => print!("{:?}, ", t),
                        Ast::NT(node) => print!("{:?}, ", node.lhs),
                    }
                }
                println!();
                states.push(GOTOS[*states.last().unwrap()][LHS[rule]].unwrap());
                let lhs = (FUNC[rule])(&child);
                ast.push(Ast::NT(Box::new(Node { rule, child, lhs })));
            }
            Action::Accept => {
                println!("\tAccept");
                return ast.pop().unwrap();
            }
            Action::Error => fail(&format!("Unexpected Token {}", input), line, col),
        }
    }
}

pub fn print_ast(ast: &Ast, filename: &str) -> std::io::Result<()> {
    let mut f = std::fs::File::create(filename)?;
    write!(
        f,
        "digraph G {{\nnode [shape=\"box\",style=\"rounded\",penwidth=1,width=2.0];"
    )?;
    let mut todo: VecDeque<(&Ast, Vec<usize>)> = VecDeque::new();
    todo.push_back((ast, Vec::new()));
    while let Some((ast, labels)) = todo.pop_front() {
        let mut s = String::new();
        for i in labels.iter() {
            s.push_str(&i.to_string());
        }
        let mut ac = s.clone();
        ac.pop();
        match ast {
            Ast::T(t) => {
                write!(f, "s{}[label=\"{}\"]\n", s, t)?;
            }
            Ast::NT(node) => {
                write!(f, "s{}[label=\"{:?}\"]\n", s, node.lhs)?;
                node.child.iter().enumerate().for_each(|(i, a)| {
                    let mut labels = labels.clone();
                    labels.push(i);
                    todo.push_back((a, labels));
                })
            }
        }
        if !s.is_empty() {
            write!(f, "s{}->s{}\n", ac, s)?;
        }
    }
    write!(f, "}}\n")?;
    Ok(())
}
