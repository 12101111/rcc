use crate::lexer::KeyWord::*;
use crate::lexer::Lexer;
use crate::lexer::Symbol::*;
use crate::lexer::Token::{self, *};
use parser::parser;
use std::collections::VecDeque;
use std::io::Write;

parser! {
    Program{
        Program->[Declaration,Program];
        Program->[];
        Declaration->[VarDecl,Symbol(Semicolon)];
        Declaration->[FuncDef,Symbol(Semicolon)];
        Declaration->[FuncDef,Symbol(LeftBrace),Statements,Symbol(RightBrace)];

        VarDecl->[Type,VarDeclList];
        VarDeclList->[VarDeclElem];
        VarDeclList->[VarDeclElem,Symbol(Comma),VarDeclList];
        VarDeclElem->[Ident(_)];
        VarDeclElem->[Ident(_),Symbol(Assignment),Expression];

        FuncDef->[Type,Ident(_),Symbol(LeftParen),TypedParameters,Symbol(RightParen)];
        TypedParameters->[];
        TypedParameters->[KeyWord(Void)];
        TypedParameters->[TypedParaList];
        TypedParaList->[TypedParameter];
        TypedParaList->[TypedParameter,Symbol(Comma),TypedParaList];
        //TypedParaList->[Symbol(Ellipsis)]; //This is for va_list
        TypedParameter->[Type,Ident(_)];

        //TODO: other type
        Type->[KeyWord(Char)];
        Type->[KeyWord(Double)];
        Type->[KeyWord(Float)];
        Type->[KeyWord(Int)];
        Type->[KeyWord(Void)];

        Statements->[Statement,Statements];
        Statements->[];
        Statement->[VarDecl,Symbol(Semicolon)];
        Statement->[Funcall,Symbol(Semicolon)];
        Statement->[KeyWord(Return),Expression,Symbol(Semicolon)];
        Statement->[IfBlock];
        Statement->[KeyWord(While),Symbol(LeftParen),Expression,Symbol(RightParen),Symbol(LeftBrace),Statements,Symbol(RightBrace)];
        Statement->[KeyWord(Do),Symbol(LeftBrace),Statements,Symbol(RightBrace),KeyWord(While),Symbol(LeftParen),Expression,Symbol(RightParen),Symbol(Semicolon)];
        Statement->[LValue,AssignOperator,Expression,Symbol(Semicolon)];

        Funcall->[Ident(_),Symbol(LeftParen),Parameters,Symbol(RightParen)];
        Parameters->[];
        Parameters->[ParaList];
        ParaList->[Expression];
        ParaList->[Expression,Symbol(Comma),ParaList];

        IfBlock->[KeyWord(If),Symbol(LeftParen),Expression,Symbol(RightParen),Symbol(LeftBrace),Statements,Symbol(RightBrace),ElseBlock];
        ElseBlock->[];
        ElseBlock->[KeyWord(Else),Symbol(LeftBrace),Statements,Symbol(RightBrace)];
        ElseBlock->[KeyWord(Else),IfBlock];

        //TODO: struct enum union
        LValue->[Ident(_)];
        LValue->[Ident(_),Symbol(Dot),Ident(_)];
        LValue->[Ident(_),Symbol(Arrow),Ident(_)];
        LValue->[Ident(_),Symbol(LeftBracket),Expression,Symbol(RightBracket)];
        LValue->[Symbol(Star),Ident(_)];
        Elem->[LValue];
        Elem->[Constant(_)];
        Elem->[Symbol(LeftParen),Expression,Symbol(RightParen)];
        Elem->[Funcall];
        Expression->[Elem];
        // 1: ! ~ - ++ -- & sizeof
        Expression->[Symbol(LogicalNot),Elem];
        Expression->[Symbol(Negation),Elem];
        Expression->[Symbol(Sub),Elem];
        Expression->[Symbol(Inc),Elem];
        Expression->[Symbol(Dec),Elem];
        Expression->[Elem,Symbol(Inc)];
        Expression->[Elem,Symbol(Dec)];
        Expression->[Symbol(And),Elem];
        Expression->[KeyWord(Sizeof),Elem];
        // 2: * % /
        Expression->[Expression,Symbol(Star),Expression];
        Expression->[Expression,Symbol(Mod),Expression];
        Expression->[Expression,Symbol(Divide),Expression];
        // 3: + -
        Expression->[Expression,Symbol(Add),Expression];
        Expression->[Expression,Symbol(Sub),Expression];
        // 4: << >>
        Expression->[Expression,Symbol(LeftShift),Expression];
        Expression->[Expression,Symbol(RightShift),Expression];
        // 5: < <= > >=
        Expression->[Expression,Symbol(Less),Expression];
        Expression->[Expression,Symbol(LessEqual),Expression];
        Expression->[Expression,Symbol(Greater),Expression];
        Expression->[Expression,Symbol(GreaterEqual),Expression];
        // 6: == !=
        Expression->[Expression,Symbol(Equal),Expression];
        Expression->[Expression,Symbol(NotEqual),Expression];
        // 7 &
        Expression->[Expression,Symbol(And),Expression];
        // 8 ^
        Expression->[Expression,Symbol(Xor),Expression];
        // 9 |
        Expression->[Expression,Symbol(Or),Expression];
        // 10 &&
        Expression->[Expression,Symbol(LogicalAnd),Expression];
        // 11 ||
        Expression->[Expression,Symbol(LogicalOr),Expression];
        //12 ? :
        Expression->[Expression,Symbol(Trinocular),Expression,Symbol(Colon),Expression];

        AssignOperator->[Symbol(Assignment)];
        AssignOperator->[Symbol(MultiAssign)];
        AssignOperator->[Symbol(ModAssign)];
        AssignOperator->[Symbol(XorAssign)];
        AssignOperator->[Symbol(AddAssign)];
        AssignOperator->[Symbol(SubAssign)];
        AssignOperator->[Symbol(DivAssign)];
        AssignOperator->[Symbol(LeftShiftAssign)];
        AssignOperator->[Symbol(RightShiftAssign)];
        AssignOperator->[Symbol(AndAssign)];
        AssignOperator->[Symbol(OrAssign)];

        // TODO: , Symbol(Comma)
    }
}

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
    NT(Node),
}

impl Ast {
    pub fn unwrap(self) -> Node {
        match self {
            Ast::NT(node) => node,
            Ast::T(t) => panic!("Unwrap Node on Token {}", t),
        }
    }
    pub fn unwrap_token(self) -> Token {
        match self {
            Ast::NT(node) => panic!("Unwrap Token on Node {}", node),
            Ast::T(t) => t,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub lhs: Nonterminal,
    pub rule: usize,
    pub child: Vec<Ast>,
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
                ast.push(Ast::NT(Node {
                    rule,
                    child,
                    lhs: NT[LHS[rule]],
                }));
            }
            Action::Accept => {
                println!("\tAccept");
                return ast.pop().unwrap()
            },
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
