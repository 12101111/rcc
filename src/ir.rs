use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

macro_rules! fail {
    ($fmt:expr, $($arg:tt)+) => {
        println!();
        eprintln!($fmt, $($arg)+);
        std::process::exit(1);
    };
}

#[derive(Debug, Clone)]
pub struct Program {
    globals: Vec<(String, TypedValue)>,
    globals_map: HashMap<String, usize>,
    funcs: Vec<Func>,
    funcs_map: HashMap<String, usize>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            globals: Vec::new(),
            globals_map: HashMap::new(),
            funcs: Vec::new(),
            funcs_map: HashMap::new(),
        }
    }
    pub fn add_decl(mut self, decl: Declaration) -> Self {
        match decl {
            Declaration::Variables(vars) => vars.into_iter().for_each(|(id, val)| {
                if self.globals_map.get(&id).is_some() {
                    fail!("Redeclare variable: {}", id);
                }
                self.globals_map.insert(id.clone(), self.globals.len());
                self.globals.push((id, val));
            }),
            Declaration::Func(f) => {
                if self.funcs_map.get(&f.ident).is_some() {
                    fail!("Redeclare function: {}", f.ident);
                }
                self.funcs_map.insert(f.ident.clone(), self.funcs.len());
                self.funcs.push(f);
            }
        }
        self
    }
}
impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        writeln!(f, "Global Variables:")?;
        for (ident, val) in self.globals.iter().rev() {
            writeln!(f, "{} : {:?}", ident, val)?;
        }
        writeln!(f, "Functions:")?;
        for func in self.funcs.iter().rev() {
            writeln!(f, "{}", func)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Variables(Vec<(String, TypedValue)>),
    Func(Func),
}

#[derive(Debug, Clone)]
pub struct Func {
    pub ident: String,
    pub return_type: Type,
    pub paras: Vec<(String, Type)>,
    pub name_map: HashMap<String, usize>,
    pub body: Option<Block>,
}

impl Display for Func {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} {}(", self.return_type, self.ident)?;
        for i in 0..self.paras.len() {
            write!(f, "{} {}", self.paras[i].1, self.paras[i].0)?;
            if i != self.paras.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")?;
        match &self.body {
            Some(block) => write!(f, "\n{}", block),
            None => write!(f, ";"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    Char,
    Int,
    UInt,
    Float,
    Double,
    Void,
    //Struct(Struct),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Type::Char => write!(f, "char"),
            Type::Int => write!(f, "int"),
            Type::UInt => write!(f, "unsigned int"),
            Type::Float => write!(f, "float"),
            Type::Double => write!(f, "double"),
            Type::Void => write!(f, "void"),
        }
    }
}
impl Type {
    fn default_value(&self) -> TypedValue {
        match self {
            Type::Char => TypedValue::Char(0),
            Type::Int => TypedValue::Int(0),
            Type::UInt => TypedValue::UInt(0),
            Type::Float => TypedValue::Float(0.0),
            Type::Double => TypedValue::Double(0.0),
            Type::Void => TypedValue::Void,
            //Type::Struct(s) => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypedValue {
    Char(u8),
    Int(i64),
    UInt(u64),
    Float(f32),
    Double(f64),
    Void,
}

impl TypedValue {
    pub fn get_type(&self) -> Type {
        match self {
            TypedValue::Char(_) => Type::Char,
            TypedValue::Int(_) => Type::Int,
            TypedValue::UInt(_) => Type::UInt,
            TypedValue::Float(_) => Type::Float,
            TypedValue::Double(_) => Type::Double,
            TypedValue::Void => Type::Void,
        }
    }
    pub fn from_constant(val: crate::lexer::Token) -> TypedValue {
        let val = match val {
            crate::lexer::Token::Constant(c) => c,
            _ => panic!("Not a constant: {:?}", val),
        };
        use crate::lexer::Constant;
        match val {
            Constant::Char(c) => TypedValue::Char(c),
            Constant::Int(i) => TypedValue::Int(i),
            Constant::UInt(u) => TypedValue::UInt(u),
            Constant::Float(f) => TypedValue::Double(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    context: HashMap<String, Type>,
    statement: Vec<Statement>,
    tmp: usize,
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        writeln!(f, "\tLocal Variables:")?;
        for (ident, ty) in self.context.iter() {
            writeln!(f, "\t{} : {}", ident, ty)?;
        }
        writeln!(f, "\tStatements:")?;
        for st in self.statement.iter() {
            writeln!(f, "\t{}", st)?;
        }
        Ok(())
    }
}

impl Block {
    pub fn new() -> Block {
        Block {
            context: HashMap::new(),
            statement: Vec::new(),
            tmp: 0,
        }
    }
    pub fn len(&self) -> usize {
        self.statement.len()
    }
    pub fn from_expr(ty: Type, var: ExprBlock) -> Block {
        assert!(var.rtn.is_none());
        Block {
            context: var.vars.into_iter().map(|i| (i, ty)).collect(),
            statement: var
                .statement
                .into_iter()
                .map(|(l, e)| Statement::Assignment(l, e))
                .collect(),
            tmp: var.tmp,
        }
    }
    pub fn insert_statement(&mut self, stmt: Statement) {
        self.statement.push(stmt);
    }
    pub fn merge(&mut self, mut block: Block) -> usize {
        self.context.extend(block.context);
        let bump = self.tmp;
        block.statement.iter_mut().for_each(|s| s.bump_tmp(bump));
        self.tmp += block.tmp;
        self.statement.extend(block.statement);
        bump
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Call(String, Vec<LValue>),
    Return(LValue),
    Assignment(LValue, Expression),
    /// Jump to second if mem[first] equal zero
    Je(LValue, isize),
    Jmp(isize),
    Block(Block),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Statement::Call(ident, args) => {
                write!(f, "{}(", ident)?;
                for a in args.iter() {
                    write!(f, "{}", a)?;
                }
                write!(f, ")")
            }
            Statement::Return(ptr) => write!(f, "return {}", ptr),
            Statement::Assignment(ptr, expr) => write!(f, "{} = {}", ptr, expr),
            Statement::Je(ptr, dst) => write!(f, "if {} == 0 jmp {}", ptr, dst),
            Statement::Jmp(dst) => write!(f, "jmp {}", dst),
            Statement::Block(b) => write!(f, "\rBlock:{}End block", b),
        }
    }
}

impl Statement {
    pub fn bump_tmp(&mut self, val: usize) {
        match self {
            Statement::Call(_, v) => v.iter_mut().for_each(|l| l.bump_tmp(val)),
            Statement::Return(l) => l.bump_tmp(val),
            Statement::Assignment(l, e) => {
                l.bump_tmp(val);
                e.bump_tmp(val);
            }
            Statement::Je(l, _) => l.bump_tmp(val),
            _ => {}
        }
    }
}
#[derive(Debug, Clone)]
pub enum Expression {
    LValue(LValue),
    Unary(Unary, LValue),
    Bin(LValue, BinOp, LValue),
    Constant(TypedValue),
    Func(String, Vec<LValue>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Expression::LValue(ptr) => write!(f, "{}", ptr),
            Expression::Unary(op, ptr) => write!(f, "{:?} {}", op, ptr),
            Expression::Bin(ptr1, op, ptr2) => write!(f, "{} {:?} {}", ptr1, op, ptr2),
            Expression::Constant(val) => write!(f, "{:?}", val),
            Expression::Func(ident, args) => {
                write!(f, "{}(", ident)?;
                for a in args.iter() {
                    write!(f, "{}", a)?;
                }
                write!(f, ")")
            }
        }
    }
}
impl Expression {
    pub fn bump_tmp(&mut self, val: usize) {
        match self {
            Expression::LValue(l) => l.bump_tmp(val),
            Expression::Unary(_, l) => l.bump_tmp(val),
            Expression::Bin(l1, _, l2) => {
                l1.bump_tmp(val);
                l2.bump_tmp(val);
            }
            Expression::Constant(_) => {}
            Expression::Func(_, v) => v.iter_mut().for_each(|l| l.bump_tmp(val)),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ExprBlock {
    vars: HashSet<String>,
    rtn: Option<Expression>,
    statement: Vec<(LValue, Expression)>,
    tmp: usize,
}

impl ExprBlock {
    pub fn new() -> ExprBlock {
        ExprBlock {
            vars: HashSet::new(),
            rtn: None,
            statement: Vec::new(),
            tmp: 0,
        }
    }
    pub fn from_expr(expr: Expression) -> ExprBlock {
        let mut block = ExprBlock::new();
        block.rtn = Some(expr);
        block
    }
    pub fn tmp(&mut self) -> LValue {
        self.tmp += 1;
        LValue::Tmp(self.tmp - 1)
    }
    pub fn get_return(&mut self) -> Expression {
        let val = self.rtn.clone().expect("Void");
        self.rtn = None;
        val
    }
    pub fn declare(&mut self, id: String) {
        self.vars.insert(id);
    }
    pub fn define(&mut self, id: String) {
        self.declare(id.clone());
        let val = self.get_return();
        self.statement.push((LValue::Ident(id), val));
    }
    pub fn assign(&mut self, lv: LValue) {
        let val = self.get_return();
        self.statement.push((lv, val));
    }
    pub fn assign_to_tmp(&mut self) -> LValue {
        match self.get_return() {
            Expression::LValue(l) => l,
            e @ _ => {
                let tmp = self.tmp();
                self.statement.push((tmp.clone(), e));
                tmp
            }
        }
    }
    pub fn insert_statement(&mut self, stmt: (LValue, Expression)) {
        self.statement.push(stmt);
    }
    pub fn assign_return(&mut self, e: Expression) {
        assert!(self.rtn.is_none());
        self.rtn = Some(e);
    }
    pub fn merge(&mut self, mut block: ExprBlock) -> usize {
        assert!(self.rtn.is_none());
        assert!(block.rtn.is_none());
        let bump = self.tmp;
        self.vars.extend(block.vars);
        block.statement.iter_mut().for_each(|(l, e)| {
            l.bump_tmp(bump);
            e.bump_tmp(bump);
        });
        self.tmp += block.tmp;
        self.statement.extend(block.statement);
        bump
    }
    pub fn bin_op(mut self, op:BinOp, mut block: ExprBlock)->ExprBlock{
        let l = self.assign_to_tmp();
        let mut r = block.assign_to_tmp();
        let bump = self.merge(block);
        r.bump_tmp(bump);
        self.assign_return(Expression::Bin(l, op, r));
        self
    }
}

#[derive(Debug, Clone)]
pub enum LValue {
    Tmp(usize),
    Ident(String),
    //Dot,
    //Arrow
    //Array
    //Pointer
}

impl Display for LValue {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            LValue::Tmp(i) => write!(f, "*{}", i),
            LValue::Ident(i) => write!(f, "{}", i),
        }
    }
}

impl LValue {
    pub fn bump_tmp(&mut self, val: usize) {
        match self {
            LValue::Tmp(l) => *self = LValue::Tmp(*l + val),
            _ => {}
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub enum BinOp {
    Star,
    Mod,
    Divide,
    Add,
    Sub,
    LeftShift,
    RightShift,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Xor,
    Or,
    LogicalAnd,
    LogicalOr,
}

#[derive(Clone, Debug, Copy)]
pub enum Unary {
    LogicalNot,
    Negation,
    Sub,
}
