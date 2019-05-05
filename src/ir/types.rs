use super::op::*;
use crate::lexer::{Constant, KeyWord, Token};
use crate::syntax::{print_ast, Ast, Node, Nonterminal};
use std::collections::HashMap;
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Char,
    Int,
    UInt,
    Float,
    Void,
    Struct(Struct),
}

impl Type {
    // 18 19 20 21 22
    fn from_type(mut node: Node) -> Type {
        assert_eq!(node.lhs, Nonterminal::Type);
        let typekw = match node.child.pop().unwrap().unwrap_token() {
            Token::KeyWord(kw) => kw,
            _ => unreachable!(),
        };
        use KeyWord::*;
        match typekw {
            Char => Type::Char,
            Double | Float => Type::Float,
            Int | Long | Short | Signed => Type::Int,
            Unsigned => Type::UInt,
            Void => Type::Void,
            _ => unreachable!(),
        }
    }
    fn default(&self) -> TypedValue {
        match self {
            Type::Char => TypedValue::Char(0),
            Type::Int => TypedValue::Int(0),
            Type::UInt => TypedValue::UInt(0),
            Type::Float => TypedValue::Float(0.0),
            Type::Void => TypedValue::Void,
            Type::Struct(s) => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypedValue {
    Char(u8),
    Int(i64),
    UInt(u64),
    Float(f64),
    Void,
}

impl TypedValue {
    pub fn get_type(&self) -> Type {
        match self {
            TypedValue::Char(_) => Type::Char,
            TypedValue::Int(_) => Type::Int,
            TypedValue::UInt(_) => Type::UInt,
            TypedValue::Float(_) => Type::Float,
            TypedValue::Void => Type::Void,
        }
    }
}

impl TypedValue {
    /*
        9: VarDeclElem->[Ident(_)];
        10: VarDeclElem->[Ident(_),Symbol(Assignment),Expression];
    */
    pub fn from_global_decl(node: Node) -> Vec<(String, TypedValue)> {
        // 3: Declaration->[VarDecl,Symbol(Semicolon)];
        assert_eq!(node.lhs, Nonterminal::Declaration);
        let decl = node.child.into_iter().next().unwrap().unwrap();
        // 6: VarDecl->[Type,VarDeclList];
        assert_eq!(decl.lhs, Nonterminal::VarDecl);
        let mut ans = Vec::new();
        let mut child = decl.child.into_iter();
        let val_type = Type::from_type(child.next().unwrap().unwrap());
        let mut list = child.next().unwrap().unwrap(); // VarDeclList
        loop {
            let mut child = list.child.into_iter();
            let mut elem = child.next().unwrap().unwrap(); // VarDeclElem
            let mut elem = elem.child.into_iter();
            let ident = match elem.next().unwrap().unwrap_token() {
                Token::Ident(i) => i,
                _ => unreachable!(),
            };
            let val = match elem.next() {
                // 9: VarDeclElem->Ident ( _ ),
                None => val_type.default(),
                // 10: VarDeclElem->Ident ( _ ), Symbol ( Assignment ), Expression,
                Some(_) => {
                    let expr = elem.next().unwrap().unwrap();
                    let val = Expression::const_expr(expr);
                    if val.get_type() != val_type {
                        panic!("Expect {:?},Found Type {:?}", valtype, val.get_type());
                    }
                    val
                }
            };
            ans.push((ident, val));
            match child.next() {
                // 7: VarDeclList->[VarDeclElem];
                None => break,
                // 8: VarDeclList->[VarDeclElem,Symbol(Comma),VarDeclList];
                Some(_) => list = child.next().unwrap().unwrap(),
            }
        }
        ans
    }
    pub fn from_constant(mut node: Node) -> TypedValue {
        assert_eq!(node.lhs, Nonterminal::Expr0);
        let val = node.child.pop().unwrap().unwrap_token();
        let val = match val {
            Token::Constant(c) => c,
            _ => unreachable!(),
        };
        match val {
            Constant::Char(c) => TypedValue::Char(c),
            Constant::Int(i) => TypedValue::Int(i),
            Constant::UInt(u) => TypedValue::UInt(u),
            Constant::Float(f) => TypedValue::Float(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    ident: String,
    member: HashMap<String, Type>,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub ident: String,
    pub return_type: Type,
    pub para: Vec<Type>,
    pub para_name: HashMap<String, usize>,
    pub body: Option<Block>,
}

impl Func {
    pub fn from_decl(node: Node) -> Func {
        assert_eq!(node.lhs, Nonterminal::Declaration);
        let mut child = node.child.into_iter();
        let def = child.next().unwrap().unwrap();
        let mut def = Self::from_def(def);
        let _ = child.next();
        match child.next() {
            // 4: Declaration->[FuncDef,Symbol(Semicolon)];
            None => break,
            // 5: Declaration->[FuncDef,Symbol(LeftBrace),Statements,Symbol(RightBrace)];
            Some(s) => def.body = Some(Block::from_statements(s.unwrap())),
        }
        def
    }
    fn from_def(node: Node) -> Func {
        // 11: FuncDef->[Type,Ident(_),Symbol(LeftParen),TypedParameters,Symbol(RightParen)];
        assert_eq!(node.lhs, Nonterminal::FuncDef);
        let mut child = node.child.into_iter();
        let return_type = Type::from_type(child.next().unwrap().unwrap());
        let ident = match child.next().unwrap().unwrap_token() {
            Token::Ident(i) => i,
            _ => unreachable!(),
        };
        let _ = child.next();
        let mut typed_paras = child.next().unwrap().unwrap();
        // 12: TypedParameters->[];
        // 13: TypedParameters->[KeyWord(Void)];
        let mut para = Vec::new();
        let mut para_name = HashMap::new();
        let body = None;
        // 14: TypedParameters->[TypedParaList];
        if let Some(Ast::NT(list)) = typed_paras.child.pop() {
            let mut child = list.child.into_iter();
            loop {
                // 17: TypedParameter->Type, Ident ( _ ),
                let mut typed_para = child.next().unwrap().unwrap();
                let ident = match typed_para.child.pop().unwrap().unwrap_token() {
                    Token::Ident(i) => i,
                    _ => unreachable!(),
                };
                let para_type = Type::from_type(typed_para.child.pop().unwrap().unwrap());
                para_name.insert(ident, para.len());
                para.push(para_type);
                match child.next() {
                    // 15: TypedParaList->[TypedParameter];
                    None => break,
                    // 16: TypedParaList->[TypedParameter,Symbol(Comma),TypedParaList];
                    Some(_) => child = child.next().unwrap().unwrap().child.into_iter(),
                }
            }
        }
        Func {
            ident,
            return_type,
            para,
            para_name,
            body,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    context: HashMap<String, TypedValue>,
    statement: Vec<Statement>,
}

impl Block {
    pub fn from_statements(mut node: Node) -> Block {
        assert_eq!(node.lhs, Nonterminal::Statements);
        let mut context = HashMap::new();
        let mut statement = Vec::new();
        loop {
            // 24: Statements->[];
            if node.child.len() == 0 {
                break;
            }
            // 23: Statements->[Statement,Statements];
            let mut child = node.child.into_iter();
            let st = child.next().unwrap().unwrap();
            match st.rule {
                // Statement->[VarDecl,Symbol(Semicolon)];
                25 => {
                    let list = TypedValue::from_vardecl(st);
                    context.extend(list);
                }
                // Statement->[Funcall,Symbol(Semicolon)];
                26 => {
                    let call = st.child.into_iter().next().unwrap().unwrap();
                    statement.extend(Statement::from_call(call));
                }
                // Statement->[KeyWord(Return),Expression,Symbol(Semicolon)];
                27 => {}
                // Statement->[IfBlock];
                28 => {}
                // Statement->[KeyWord(While),Symbol(LeftParen),Expression,Symbol(RightParen),
                // Symbol(LeftBrace),Statements,Symbol(RightBrace)];
                29 => {}
                // Statement->[KeyWord(Do),Symbol(LeftBrace),Statements,Symbol(RightBrace),
                // KeyWord(While),Symbol(LeftParen),Expression,Symbol(RightParen),Symbol(Semicolon)];
                30 => {}
                // Statement->[LValue,AssignOperator,Expression,Symbol(Semicolon)];
                31 => {}
                _ => unreachable!(),
            }
            // 23: Statements->[Statement,Statements];
            node = child.next().unwrap().unwrap();
        }
        Block { context, statement }
    }
    fn from_expression(mut node: Node) -> Block {
        unimplemented!()
    }
}
