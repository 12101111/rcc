use crate::lexer::KeyWord::*;
use crate::lexer::Lexer;
use crate::lexer::Symbol::*;
use crate::lexer::Token::{self, *};
use parser::parser;
use std::collections::VecDeque;
use std::io::Write;

parser! {
    Program{
        Program[Declaration,Program],
        Program[],
        Declaration[VarDecl,Symbol(Semicolon)],
        Declaration[FuncDecl],
        Declaration[FuncDef],
        VarDecl[Type,VarDeclList],
        VarDeclList[VarDeclElem,VarDeclSuffix],
        VarDeclSuffix[Symbol(Comma),VarDeclList],
        VarDeclSuffix[],
        VarDeclElem[DeclVar],
        VarDeclElem[DeclVarAssign],
        DeclVar[Ident(_)],
        DeclVarAssign[Ident(_),Symbol(Assignment),Expression],
        //TODO:
        Type[KeyWord(Char)],
        Type[KeyWord(Double)],
        Type[KeyWord(Float)],
        Type[KeyWord(Int)],
        Type[KeyWord(Void)],
        FuncDecl[FuncDePrefix,Symbol(Semicolon)],
        FuncDef[FuncDePrefix,Symbol(LeftBrace),Statements,Symbol(RightBrace)],
        FuncDePrefix[Type,Ident(_),Symbol(LeftParen),TypedParameters,Symbol(RightParen)],
        TypedParameters[],
        TypedParameters[KeyWord(Void)],
        TypedParameters[TypedParaList],
        TypedParaList[TypedParameter,TypedParaListSuffix],
        TypedParaListSuffix[Symbol(Comma),TypedParaList],
        TypedParaListSuffix[],
        //FIXME: Lexer has bug
        TypedParaListSuffix[Symbol(Comma),Symbol(Ellipsis)],
        TypedParameter[Type,Ident(_)],
        Statements[Statement,Statements],
        Statements[],
        Statement[VarDecl,Symbol(Semicolon)],
        Statement[Funcall,Symbol(Semicolon)],
        Statement[Return,Symbol(Semicolon)],
        Statement[IfBlock],
        Statement[WhileBlock],
        Statement[Assignment,Symbol(Semicolon)],
        Funcall[Ident(_),Symbol(LeftParen),Parameters,Symbol(RightParen)],
        Parameters[],
        Parameters[ParaList],
        ParaList[Expression,ParaListSuffix],
        ParaListSuffix[Symbol(Comma),ParaList],
        ParaListSuffix[],
        Return[KeyWord(Return),Expression],
        IfBlock[KeyWord(If),Symbol(LeftParen),Expression,Symbol(RightParen),Symbol(LeftBrace),Statements,Symbol(RightBrace),ElseBlock],
        ElseBlock[],
        ElseBlock[KeyWord(Else),Symbol(LeftBrace),Statements,Symbol(RightBrace)],
        ElseBlock[KeyWord(Else),IfBlock],
        WhileBlock[KeyWord(While),Symbol(LeftParen),Expression,Symbol(RightParen),Symbol(LeftBrace),Statements,Symbol(RightBrace)],
        Assignment[LValue,AssignOperator,Expression],
        //TODO: struct enum union pointer
        LValue[Ident(_)],
        Expression[ExprElem,ExprSuffix],
        ExprSuffix[],
        ExprSuffix[BinOperator,Expression],
        ExprElem[ConstantValue],
        ExprElem[Ident(_)],
        ExprElem[Symbol(LeftParen),Expression,Symbol(RightParen)],
        ExprElem[Funcall],
        ExprElem[MonoOperator,Ident(_)],
        ExprElem[MonoOperator,ConstantValue],
        ExprElem[MonoOperator,Symbol(LeftParen),Expression,Symbol(RightParen)],
        ConstantValue[Constant(_)],
        ConstantValue[Literal(_)],
        // TODO: [] () . ->
        // FIXME: 2 ! ~ - ++ --
        // TODO: * depointer  sizeof & getaddress (TYPE)
        MonoOperator[Symbol(LogicalNot)],
        MonoOperator[Symbol(Negation)],
        MonoOperator[Symbol(Sub)],
        MonoOperator[Symbol(Inc)],
        MonoOperator[Symbol(Dec)],
        // FIXME: 3 * % /
        BinOperator[Symbol(Star)],
        BinOperator[Symbol(Mod)],
        BinOperator[Symbol(Divide)],
        // FIXME: 4 + -
        BinOperator[Symbol(Add)],
        BinOperator[Symbol(Sub)],
        // FIXME: 5 << >>
        BinOperator[Symbol(LeftShift)],
        BinOperator[Symbol(RightShift)],
        // FIXME: 6 < <= > >=
        BinOperator[Symbol(Less)],
        BinOperator[Symbol(LessEqual)],
        BinOperator[Symbol(Greater)],
        BinOperator[Symbol(GreaterEqual)],
        // FIXME: 7 == !=
        BinOperator[Symbol(Equal)],
        BinOperator[Symbol(NotEqual)],
        // FIXME: 8 &
        BinOperator[Symbol(And)],
        // FIXME: 9 ^
        BinOperator[Symbol(Xor)],
        // FIXME: 10 |
        BinOperator[Symbol(Or)],
        // FIXME: 11 &&
        BinOperator[Symbol(LogicalAnd)],
        // FIXME: 12 ||
        BinOperator[Symbol(LogicalOr)],
        //TODO: 13 Symbol(Trinocular) Symbol(Colon)
        //14
        AssignOperator[Symbol(Assignment)],
        AssignOperator[Symbol(MultiAssign)],
        AssignOperator[Symbol(ModAssign)],
        AssignOperator[Symbol(XorAssign)],
        AssignOperator[Symbol(AddAssign)],
        AssignOperator[Symbol(SubAssign)],
        AssignOperator[Symbol(DivAssign)],
        AssignOperator[Symbol(LeftShiftAssign)],
        AssignOperator[Symbol(RightShiftAssign)],
        AssignOperator[Symbol(AndAssign)],
        AssignOperator[Symbol(OrAssign)]
        // TODO: 15 , Symbol(Comma)
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

#[derive(Debug, Clone)]
pub struct Node {
    subs: Vec<Ast>,
    rule: usize,
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
        write!(f, "{}:{{", NTS[LHS[self.rule]])?;
        for i in &self.subs {
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
    println!("");
    eprintln!("Syntax Error: {}\nLine:{} ,Col:{}", msg, line, col);
    std::process::exit(1);
}

pub fn parse(lexer: Lexer) -> Ast {
    let mut states = Vec::new();
    states.push(0);
    let mut ast = Vec::new();
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
        /*for t in &ast {
            print!("{} ", t);
        }*/
        print!("input:");
        let i = lex
            .peek()
            .map(|(i, l, c)| {
                line = *l;
                col = *c;
                input = format!("{}", i);
                print!(" {},", i);
                match_token(i).unwrap_or_else(|| {
                    fail(&format!("no match token for this input:{}", i), *l, *c)
                })
            })
            .unwrap_or(TERMINAL_NUM);
        print!(" #{}\t", i);
        match ACTIONS[*states.last().unwrap()][i] {
            Action::Shift(i) => {
                println!("S{}", i);
                states.push(i);
                ast.push(Ast::T(lex.next().unwrap().0))
            }
            Action::Reduce(rule) => {
                print!("R{}", rule);
                let len = ast.len() - LENS[rule];
                let subs = ast.split_off(len);
                states.truncate(states.len() - LENS[rule]);
                print!("\tReduces: ");
                for j in &subs {
                    match j {
                        Ast::T(t) => print!("{}, ", t),
                        Ast::NT(node) => print!("{}, ", NTS[LHS[node.rule]]),
                    }
                }
                println!();
                states.push(GOTOS[*states.last().unwrap()][LHS[rule]].unwrap());
                ast.push(Ast::NT(Node { rule, subs }));
            }
            Action::Accept => return ast.pop().unwrap(),
            Action::Error => fail(&format!("Unexpected Token {}", input), line, col),
        }
    }
}

pub fn print_ast(ast: &Ast) -> std::io::Result<()> {
    let mut f = std::fs::File::create("ast.dot")?;
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
                write!(f, "s{}[label=\"{}\"]\n", s, NTS[LHS[node.rule]])?;
                node.subs.iter().enumerate().for_each(|(i, a)| {
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
