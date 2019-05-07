use crate::lexer::{Constant, KeyWord, Symbol, Token};
use crate::syntax::{Ast, Node, Nonterminal};
use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Debug)]
pub struct Program {
    globals: HashMap<String, TypedValue>,
    funcs: HashMap<String, Func>,
}

impl Program {
    pub fn new(ast: Ast) -> Program {
        let mut p = Program {
            globals: HashMap::new(),
            funcs: HashMap::new(),
        };
        //1: Program->[Declaration,Program];
        let mut node = ast.unwrap();
        assert_eq!(node.lhs, Nonterminal::Program);
        while !node.child.is_empty() {
            let mut child = node.child.into_iter();
            let decl = child.next().unwrap().unwrap(); //Declaration
            node = child.next().unwrap().unwrap(); //Program
            match decl.rule {
                // 3: Declaration->[VarDecl,Symbol(Semicolon)];
                3 => {
                    let val_decl = decl.child.into_iter().next().unwrap().unwrap();
                    let mut block = Block::new();
                    block.insert_var_decl(val_decl);
                    p.globals.extend(TypedValue::from_const_block(block));
                }
                // 4: Declaration->[FuncDef,Symbol(Semicolon)];
                // 5: Declaration->[FuncDef,Symbol(LeftBrace),Statements,Symbol(RightBrace)];
                _ => {
                    let func = Func::from_decl(decl);
                    p.funcs.insert(func.ident.clone(), func);
                }
            }
        }
        p
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        writeln!(f, "Global Variables:")?;
        for (ident, val) in self.globals.iter() {
            writeln!(f, "{} : {:?}", ident, val)?;
        }
        writeln!(f, "Functions:")?;
        for (_, p) in self.funcs.iter() {
            write!(f, "{}", p)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub ident: String,
    pub return_type: Type,
    pub para: Vec<(String, Type)>,
    pub para_name: HashMap<String, usize>,
    pub body: Option<Block>,
}

impl std::fmt::Display for Func {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} {}(", self.return_type, self.ident)?;
        for i in 0..self.para.len() {
            write!(f, "{} {}", self.para[i].1, self.para[i].0)?;
            if i != self.para.len() - 1 {
                write!(f, ", ")?;
            }
        }
        writeln!(f, ")")?;
        match &self.body {
            Some(block) => {
                writeln!(f, "\tLocal Variables:")?;
                for (ident, ptr) in block.context.iter() {
                    writeln!(f, "\t{} : {}", ident, ptr)?;
                }
                writeln!(f, "\tStatements:")?;
                for st in block.statement.iter() {
                    writeln!(f, "\t{}", st)?;
                }
                Ok(())
            }
            None => write!(f, ";"),
        }
    }
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
            None => {}
            // 5: Declaration->[FuncDef,Symbol(LeftBrace),Statements,Symbol(RightBrace)];
            Some(s) => {
                let mut block = Block::new();
                block.insert_statements(s.unwrap());
                def.body = Some(block);
            }
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
                para_name.insert(ident.clone(), para.len());
                para.push((ident, para_type));
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    Char,
    Int,
    UInt,
    Float,
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
            Type::Void => write!(f, "void"),
        }
    }
}

impl Type {
    // 18 19 20 21 22
    fn from_type(mut node: Node) -> Type {
        assert_eq!(node.lhs, Nonterminal::Type);
        let keyword = match node.child.pop().unwrap().unwrap_token() {
            Token::KeyWord(kw) => kw,
            _ => unreachable!(),
        };
        use KeyWord::*;
        match keyword {
            Char => Type::Char,
            Double | Float => Type::Float,
            Int | Long | Short | Signed => Type::Int,
            Unsigned => Type::UInt,
            Void => Type::Void,
            _ => unreachable!(),
        }
    }
    fn default_value(&self) -> TypedValue {
        match self {
            Type::Char => TypedValue::Char(0),
            Type::Int => TypedValue::Int(0),
            Type::UInt => TypedValue::UInt(0),
            Type::Float => TypedValue::Float(0.0),
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
    pub fn from_constant(mut node: Node) -> TypedValue {
        assert_eq!(node.lhs, Nonterminal::Elem);
        let val = node.child.pop().unwrap().unwrap_token();
        let val = match val {
            Token::Constant(c) => c,
            _ => panic!("Not a constant: {:?}", val),
        };
        match val {
            Constant::Char(c) => TypedValue::Char(c),
            Constant::Int(i) => TypedValue::Int(i),
            Constant::UInt(u) => TypedValue::UInt(u),
            Constant::Float(f) => TypedValue::Float(f),
        }
    }
    pub fn from_const_block(block: Block) -> HashMap<String, TypedValue> {
        let Block {
            context,
            count,
            statement,
        } = block;
        let mut mem: Vec<TypedValue> = vec![TypedValue::Void; count];
        for st in statement {
            match st {
                Statement::Assignment(i, expr) => {
                    let val = match expr {
                        Expression::Constant(val) => val,
                        Expression::Mono(op, i) => unimplemented!(),
                        Expression::Bin(i, op, j) => unimplemented!(),
                        _ => unimplemented!(),
                    };
                    mem[i.1] = val;
                }
                _ => unreachable!(),
            }
        }
        context.into_iter().map(|(s, i)| (s, mem[i.1])).collect()
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    context: HashMap<String, Pointer>,
    count: usize,
    statement: Vec<Statement>,
}

impl Block {
    pub fn new() -> Block {
        Block {
            context: HashMap::new(),
            count: 0,
            statement: Vec::new(),
        }
    }
    fn add_tmp(&mut self, ty: Type) -> Pointer {
        let t = self.count;
        self.count += 1;
        Pointer(ty, t)
    }
    pub fn insert_statements(&mut self, mut node: Node) {
        assert_eq!(node.lhs, Nonterminal::Statements);
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
                    let decl = st.child.into_iter().next().unwrap().unwrap();
                    self.insert_var_decl(decl);
                }
                // Statement->[Funcall,Symbol(Semicolon)];
                26 => {
                    let call = st.child.into_iter().next().unwrap().unwrap();
                    let (i, a) = self.call(call);
                    self.statement.push(Statement::Call(i, a));
                }
                // Statement->[KeyWord(Return),Expression,Symbol(Semicolon)];
                27 => {
                    let mut it = st.child.into_iter();
                    let _ = it.next();
                    let expr = it.next().unwrap().unwrap();
                    let expr = self.insert_expression(expr);
                    self.statement.push(Statement::Return(expr));
                }
                // Statement->[IfBlock];
                28 => unimplemented!(),
                // Statement->[KeyWord(While),Symbol(LeftParen),Expression,Symbol(RightParen),
                // Symbol(LeftBrace),Statements,Symbol(RightBrace)];
                29 => unimplemented!(),
                // Statement->[KeyWord(Do),Symbol(LeftBrace),Statements,Symbol(RightBrace),
                // KeyWord(While),Symbol(LeftParen),Expression,Symbol(RightParen),Symbol(Semicolon)];
                30 => unimplemented!(),
                // Statement->[LValue,AssignOperator,Expression,Symbol(Semicolon)];
                31 => unimplemented!(),
                _ => unreachable!(),
            }
            // 23: Statements->[Statement,Statements];
            node = child.next().unwrap().unwrap();
        }
    }
    pub fn insert_var_decl(&mut self, node: Node) {
        // 6: VarDecl->[Type,VarDeclList];
        assert_eq!(node.lhs, Nonterminal::VarDecl);
        let mut child = node.child.into_iter();
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
            let ptr = match elem.next() {
                // 9: VarDeclElem->Ident ( _ ),
                None => self.add_tmp(val_type),
                // 10: VarDeclElem->Ident ( _ ), Symbol ( Assignment ), Expression,
                Some(_) => self.insert_expression(elem.next().unwrap().unwrap()),
            };
            self.context.insert(ident, ptr);
            match child.next() {
                // 7: VarDeclList->[VarDeclElem];
                None => break,
                // 8: VarDeclList->[VarDeclElem,Symbol(Comma),VarDeclList];
                Some(_) => list = child.next().unwrap().unwrap(),
            }
        }
    }
    pub fn insert_expression(&mut self, mut node: Node) -> Pointer {
        assert_eq!(node.lhs, Nonterminal::Expression);
        match node.rule {
            50 => self.insert_elem(node.child.pop().unwrap().unwrap()),
            51...59 => unimplemented!(),
            60...77 => {
                let mut child = node.child.into_iter();
                let left = child.next().unwrap().unwrap();
                let op = child.next().unwrap().unwrap_token();
                let right = child.next().unwrap().unwrap();
                let left = self.insert_expression(left);
                let right = self.insert_expression(right);
                let op = BinOp::from_symbol(op);
                let expr = Expression::Bin(left, op, right);
                let ptr = self.add_tmp(left.0);
                self.statement.push(Statement::Assignment(ptr, expr));
                ptr
            }
            _ => unimplemented!(),
        }
    }
    fn insert_elem(&mut self, mut node: Node) -> Pointer {
        assert_eq!(node.lhs, Nonterminal::Elem);
        match node.rule {
            46 => {
                let lval = node.child.pop().unwrap().unwrap();
                let mut child = lval.child.into_iter();
                match lval.rule{
                    41=>{
                        let ident = child.next().unwrap().unwrap_token();
                        let ident = match ident{
                            Token::Ident(i)=>i,
                            _=>unreachable!()
                        };
                        match self.context.get(&ident){
                            Some(ptr)=>*ptr,
                            None=>unimplemented!()
                        }
                    }
                    42...45=>unimplemented!(),
                    _=>unreachable!()
                }
            }, //LValue
            47 => {
                let val = TypedValue::from_constant(node);
                let ptr = self.add_tmp(val.get_type());
                self.statement
                    .push(Statement::Assignment(ptr, Expression::Constant(val)));
                ptr
            }
            48 => {
                let mut child = node.child.into_iter();
                let _ = child.next();
                let expr = child.next().unwrap().unwrap();
                self.insert_expression(expr)
            }
            49 => {
                let (i, a) = self.call(node.child.pop().unwrap().unwrap());
                let expr = Expression::Func(i, a); //TODO: Type
                let ptr = self.add_tmp(unimplemented!("get func return type"));
                self.statement.push(Statement::Assignment(ptr, expr));
                ptr
            }
            _ => unreachable!(),
        }
    }
    pub fn call(&mut self, node: Node) -> (String, Vec<Pointer>) {
        assert_eq!(node.lhs, Nonterminal::Funcall);
        let mut child = node.child.into_iter();
        let ident = child.next().unwrap().unwrap_token();
        let ident = match ident {
            Token::Ident(i) => i,
            _ => unreachable!(),
        };
        let _ = child.next();
        let mut paras = child.next().unwrap().unwrap();
        let mut args = Vec::new();
        if paras.child.len() > 0 {
            let list = paras.child.pop().unwrap().unwrap();
            let mut list = list.child.into_iter();
            loop {
                let expr = list.next().unwrap().unwrap();
                let expr = self.insert_expression(expr);
                args.push(expr);
                match list.next() {
                    None => break,
                    Some(_) => list = list.next().unwrap().unwrap().child.into_iter(),
                }
            }
        }
        (ident, args)
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Call(String, Vec<Pointer>),
    Return(Pointer),
    Assignment(Pointer, Expression),
    /// Jump to second if mem[first] equal zero
    Je(Pointer, usize),
    Jmp(usize),
}

impl std::fmt::Display for Statement {
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
            Statement::Je(ptr, dst) => write!(f, "if {} jmp {}", ptr, dst),
            Statement::Jmp(dst) => write!(f, "jmp {}", dst),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    LValue(Pointer),
    Constant(TypedValue),
    Func(String, Vec<Pointer>),
    Mono(MonoOp, Pointer),
    Bin(Pointer, BinOp, Pointer),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Expression::LValue(ptr) => write!(f, "{}", ptr),
            Expression::Constant(val) => write!(f, "{:?}", val),
            Expression::Func(ident, args) => {
                write!(f, "{}(", ident)?;
                for a in args.iter() {
                    write!(f, "{}", a)?;
                }
                write!(f, ")")
            }
            Expression::Mono(op, ptr) => write!(f, "{:?} {}", op, ptr),
            Expression::Bin(ptr1, op, ptr2) => write!(f, "{} {:?} {}", ptr1, op, ptr2),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Pointer(Type, usize);

impl std::fmt::Display for Pointer {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "({}*){}", self.0, self.1)
    }
}

macro_rules! bin_op {
    { $($name:ident),*} => {
        #[derive(Clone,Debug, Copy)]
        pub enum BinOp{
            $($name,)*
        }
        impl BinOp{
            pub fn from_symbol(token:Token)->BinOp{
                let sym = match token{
                    Token::Symbol(s)=>s,
                    _=>unreachable!(),
                };
                match sym{
                    $(Symbol::$name=>BinOp::$name,)*
                    _=>unreachable!()
                }
            }
        }
    }
}

bin_op! {
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
    LogicalOr
}

#[derive(Debug, Clone, Copy)]
pub enum MonoOp {
    LogicalNot,
    Negation,
    Sub,
}

/*
#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    ident: String,
    member: HashMap<String, Type>,
}
*/
