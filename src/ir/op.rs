use super::types::*;
use crate::lexer::{Constant, KeyWord, Token};
use crate::syntax::{print_ast, Ast, Node, Nonterminal};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Statement {
    Funcall(String, Vec<String>),
    Return(String),
    Assignment(String, Expression),
    If(String, usize),
    Goto(usize),
}
impl Statement {
    pub fn from_call(node: Node) -> Vec<Statement> {
        assert_eq!(node.lhs, Nonterminal::Funcall);
        let mut child = node.child.into_iter();
        let ident = child.next().unwrap().unwrap_token();
        let _ = child.next();
        let mut paras = child.next().unwrap().unwrap();
        //let args = Vec::new();
        if paras.child.len() > 0 {
            let list = paras.child.pop().unwrap().unwrap();
            let mut list = list.child.into_iter();
            let exprs = ExpressionBuilder::new(list.next().unwrap().unwrap());
        }
        unimplemented!()
    }
}
#[derive(Debug, Clone)]
pub enum Expression {
    LValue(String),
    Constant(TypedValue),
    Funcall(String, Vec<String>),
    Mono(MonoOp, String),
    Bin(String, BinOp, String),
}
impl Expression {
    pub fn const_expr(mut node: Node) -> TypedValue {
        unimplemented!()
    }
}

struct ExpressionBuilder {
    tmp_var: HashMap<String, TypedValue>,
    extern_var: HashMap<String, Type>,
    expr: Vec<Expression>,
    ast: Ast,
}

impl ExpressionBuilder {
    pub fn new(node: Node) -> ExpressionBuilder {
        unimplemented!()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Star,
    Mod,
    Divide,
}
#[derive(Debug, Clone, Copy)]
pub enum MonoOp {
    LogicalNot,
    Negation,
    Sub,
}
