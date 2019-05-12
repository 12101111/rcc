fn Program(decl: Declaration, p: Program) -> Program {
    p.add_decl(decl)
}
fn Program() -> Program {
    Program::new()
}
fn Declaration(decl: VarDecl, _s: Symbol(Semicolon)) -> Declaration {
    // TODO: calc const block
    let vars: Vec<_> = decl
        .statement
        .iter()
        .map(|st| match st {
            Statement::Assignment(lv, expr) => {
                let id = match lv {
                    LValue::Ident(id) => id.clone(),
                    _ => unimplemented!(),
                };
                let val = match expr {
                    Expression::Constant(val) => val.clone(),
                    _ => unimplemented!(),
                };
                (id, val)
            }
            _ => unimplemented!(),
        })
        .collect();
    Declaration::Variables(vars)
}
fn Declaration(func: FuncDef, _s: Symbol(Semicolon)) -> Declaration {
    Declaration::Func(func)
}
fn Declaration(
    func: FuncDef,
    _lb: Symbol(LeftBrace),
    stmts: Statements,
    _rb: Symbol(RightBrace),
) -> Declaration {
    func.body = Some(stmts);
    Declaration::Func(func)
}
fn VarDecl(ty: Type, var: VarDeclList) -> Block {
    Block::from_expr(ty, var)
}
fn VarDeclList(var: VarDeclElem) -> ExprBlock {
    var
}
fn VarDeclList(var: VarDeclElem, _s: Symbol(Comma), list: VarDeclList) -> ExprBlock {
    list.merge(var);
    list
}
fn VarDeclElem(id: Ident) -> ExprBlock {
    let mut expr = ExprBlock::new();
    expr.declare(id);
    expr
}
fn VarDeclElem(id: Ident, _s: Symbol(Assignment), expr: Expression) -> ExprBlock {
    expr.define(id);
    expr
}
fn Ident(id: Ident(_)) -> String {
    match id {
        Token::Ident(id) => id,
        _ => unreachable!(),
    }
}
fn FuncDef(
    return_type: Type,
    ident: Ident,
    _lp: Symbol(LeftParen),
    paras: TypedParameters,
    _rp: Symbol(RightParen),
) -> Func {
    let mut name_map = HashMap::new();
    paras
        .clone()
        .into_iter()
        .enumerate()
        .for_each(|(i, (id, _))| {
            let r = name_map.insert(id.clone(), i);
            if r.is_some() {
                fail!("redefine parameter: {}", id);
            }
        });
    Func {
        ident,
        return_type,
        paras,
        name_map,
        body: None,
    }
}
fn TypedParameters() -> Vec<(String, Type)> {
    Vec::new()
}
fn TypedParameters(_kw: KeyWord(Void)) -> Vec<(String, Type)> {
    Vec::new()
}
fn TypedParameters(para_list: TypedParaList) -> Vec<(String, Type)> {
    para_list
}
fn TypedParaList(para: TypedParameter) -> Vec<(String, Type)> {
    vec![para]
}
fn TypedParaList(
    para: TypedParameter,
    _s: Symbol(Comma),
    para_list: TypedParaList,
) -> Vec<(String, Type)> {
    para_list.push(para);
    para_list
}
//fn TypedParaList(_s:Symbol(Ellipsis)) -> ?? {} //This is for va_list
fn TypedParameter(ty: Type, id: Ident) -> (String, Type) {
    (id, ty)
}

//TODO: other type
fn Type(_kw: KeyWord(Char)) -> Type {
    Type::Char
}
fn Type(_kw: KeyWord(Double)) -> Type {
    Type::Double
}
fn Type(_kw: KeyWord(Float)) -> Type {
    Type::Float
}
fn Type(_kw: KeyWord(Int)) -> Type {
    Type::Int
}
fn Type(_kw: KeyWord(Void)) -> Type {
    Type::Void
}

fn Statements(st: Statement, stmts: Statements) -> Block {
    st.merge(stmts);
    st
}
fn Statements() -> Block {
    Block::new()
}
fn Statement(var: VarDecl, _s: Symbol(Semicolon)) -> Block {
    var
}
fn Statement(call: Funcall, _s: Symbol(Semicolon)) -> Block {
    let (mut expr_block, id, paras) = call;
    let mut block = Block::from_expr(Type::Void, expr_block);
    block.insert_statement(Statement::Call(id, paras));
    block
}
fn Statement(_kw: KeyWord(Return), expr: Expression, _s: Symbol(Semicolon)) -> Block {
    let val = expr.assign_to_tmp();
    let mut block = Block::from_expr(Type::Void, expr);
    block.insert_statement(Statement::Return(val));
    block
}
fn Statement(block: IfBlock) -> Block {
    block
}
fn Statement(
    _kw: KeyWord(While),
    _lp: Symbol(LeftParen),
    cond: Expression,
    _rp: Symbol(RightParen),
    _lb: Symbol(LeftBrace),
    stmts: Statements,
    _rb: Symbol(RightBrace),
) -> Block {
    let c = cond.assign_to_tmp();
    let mut block = Block::from_expr(Type::Void, cond);
    block.insert_statement(Statement::Je(c, 2));
    block.insert_statement(Statement::Block(stmts));
    block.insert_statement(Statement::Jmp(-(block.statement.len() as isize) - 1));
    block
}
fn Statement(
    _kw: KeyWord(Do),
    _lb: Symbol(LeftBrace),
    stmts: Statements,
    _rb: Symbol(RightBrace),
    _kw2: KeyWord(While),
    _lp: Symbol(LeftParen),
    cond: Expression,
    _rp: Symbol(RightParen),
    _s: Symbol(Semicolon),
) -> Block {
    let mut block = Block::new();
    block.insert_statement(Statement::Block(stmts));
    let mut c = cond.assign_to_tmp();
    let cond = Block::from_expr(Type::Void, cond);
    let bump = block.merge(cond);
    c.bump_tmp(bump);
    block.insert_statement(Statement::Je(c, 1));
    block.insert_statement(Statement::Jmp(-(block.statement.len() as isize) - 1));
    block
}
fn Statement(lv: LValue, op: AssignOperator, expr: Expression, _s: Symbol(Semicolon)) -> Block {
    expr.assign(lv);
    Block::from_expr(Type::Void, expr)
}

fn Funcall(
    id: Ident,
    _lp: Symbol(LeftParen),
    para: Parameters,
    _rp: Symbol(RightParen),
) -> (ExprBlock, String, Vec<LValue>) {
    let (expr_block, paras) = para;
    (expr_block, id, paras)
}
fn Parameters() -> (ExprBlock, Vec<LValue>) {
    (ExprBlock::new(), Vec::new())
}
fn Parameters(para: ParaList) -> (ExprBlock, Vec<LValue>) {
    para
}
fn ParaList(expr: Expression) -> (ExprBlock, Vec<LValue>) {
    let l = expr.assign_to_tmp();
    (expr, vec![l])
}
fn ParaList(expr: Expression, _s: Symbol(Comma), pl: ParaList) -> (ExprBlock, Vec<LValue>) {
    let mut l = expr.assign_to_tmp();
    let (mut expr_block, mut ll) = pl;
    let bump = expr_block.merge(expr);
    l.bump_tmp(bump);
    ll.push(l);
    (expr_block, ll)
}

fn IfBlock(
    _kw: KeyWord(If),
    _lp: Symbol(LeftParen),
    cond: Expression,
    _rp: Symbol(RightParen),
    _lb: Symbol(LeftBrace),
    stmts: Statements,
    _rb: Symbol(RightBrace),
    else_block: ElseBlock,
) -> Block {
    let c = cond.assign_to_tmp();
    let mut block = Block::from_expr(Type::Void, cond);
    match else_block {
        None => {
            block.insert_statement(Statement::Je(c, 1));
            block.insert_statement(Statement::Block(stmts));
        }
        Some(else_block) => {
            block.insert_statement(Statement::Je(c, 2));
            block.insert_statement(Statement::Block(stmts));
            block.merge(else_block);
        }
    }
    block
}
fn ElseBlock() -> Option<Block> {
    None
}
fn ElseBlock(
    _kw: KeyWord(Else),
    _lb: Symbol(LeftBrace),
    stmts: Statements,
    _rb: Symbol(RightBrace),
) -> Option<Block> {
    let mut block = Block::new();
    block.insert_statement(Statement::Jmp(1));
    block.insert_statement(Statement::Block(stmts));
    Some(block)
}
fn ElseBlock(_kw: KeyWord(Else), if_block: IfBlock) -> Option<Block> {
    let mut block = Block::new();
    block.insert_statement(Statement::Jmp(if_block.len() as isize));
    block.merge(if_block);
    Some(block)
}

//TODO: struct enum union
fn LValue(id: Ident) -> LValue {
    LValue::Ident(id)
}
/*fn LValue(id: Ident, _s: Symbol(Dot), member: Ident -> LValue {
    unimplemented!()
}
fn LValue(ptr: Ident, _s: Symbol(Arrow), member: Ident -> LValue {
    unimplemented!()
}
fn LValue(
    id: Ident,
    _lb: Symbol(LeftBracket),
    expr: Expression,
    _rb: Symbol(RightBracket),
) -> Block {
    unimplemented!()
}
fn LValue(_s: Symbol(Star), id: Ident -> Block {
    unimplemented!()
}*/
fn Elem(lv: LValue) -> ExprBlock {
    ExprBlock::from_expr(Expression::LValue(lv))
}
fn Elem(c: Constant(_)) -> ExprBlock {
    ExprBlock::from_expr(Expression::Constant(TypedValue::from_constant(c)))
}
fn Elem(_lp: Symbol(LeftParen), expr: Expression, _rp: Symbol(RightParen)) -> ExprBlock {
    expr
}
fn Elem(call: Funcall) -> ExprBlock {
    let (mut expr_block, id, paras) = call;
    expr_block.assign_return(Expression::Func(id, paras));
    expr_block
}
fn Expression(el: Elem) -> ExprBlock {
    el
}

fn Expression(_s: Symbol(LogicalNot), e: Elem) -> ExprBlock {
    let l = e.assign_to_tmp();
    e.assign_return(Expression::Unary(Unary::LogicalNot, l));
    e
}

fn Expression(_s: Symbol(Negation), e: Elem) -> ExprBlock {
    let l = e.assign_to_tmp();
    e.assign_return(Expression::Unary(Unary::Negation, l));
    e
}
fn Expression(_s: Symbol(Sub), e: Elem) -> ExprBlock {
    let l = e.assign_to_tmp();
    e.assign_return(Expression::Unary(Unary::Sub, l));
    e
} /*
  fn Expression(_s: Symbol(Inc), e: Elem) -> ExprBlock {
      unimplemented!()
  }
  fn Expression(_s: Symbol(Dec), e: Elem) -> ExprBlock {
      unimplemented!()
  }
  fn Expression(e: Elem, _s: Symbol(Inc)) -> ExprBlock {
      unimplemented!()
  }
  fn Expression(e: Elem, _s: Symbol(Dec)) -> ExprBlock {
      unimplemented!()
  }
  fn Expression(_s: Symbol(And), e: Elem) -> ExprBlock {
      unimplemented!()
  }
  fn Expression(_s: KeyWord(Sizeof), e: Elem) -> ExprBlock {
      unimplemented!()
  }*/
fn Expression(e1: Expression, _s: Symbol(Star), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::Star, e2)
}
fn Expression(e1: Expression, _s: Symbol(Mod), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::Mod, e2)
}
fn Expression(e1: Expression, _s: Symbol(Divide), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::Divide, e2)
}
fn Expression(e1: Expression, _s: Symbol(Add), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::Add, e2)
}
fn Expression(e1: Expression, _s: Symbol(Sub), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::Sub, e2)
}
fn Expression(e1: Expression, _s: Symbol(LeftShift), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::LeftShift, e2)
}
fn Expression(e1: Expression, _s: Symbol(RightShift), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::RightShift, e2)
}
fn Expression(e1: Expression, _s: Symbol(Less), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::Less, e2)
}
fn Expression(e1: Expression, _s: Symbol(LessEqual), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::LessEqual, e2)
}
fn Expression(e1: Expression, _s: Symbol(Greater), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::Greater, e2)
}
fn Expression(e1: Expression, _s: Symbol(GreaterEqual), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::GreaterEqual, e2)
}
fn Expression(e1: Expression, _s: Symbol(Equal), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::Equal, e2)
}
fn Expression(e1: Expression, _s: Symbol(NotEqual), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::NotEqual, e2)
}
fn Expression(e1: Expression, _s: Symbol(And), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::And, e2)
}
fn Expression(e1: Expression, _s: Symbol(Xor), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::Xor, e2)
}
fn Expression(e1: Expression, _s: Symbol(Or), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::Or, e2)
}
fn Expression(e1: Expression, _s: Symbol(LogicalAnd), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::LogicalAnd, e2)
}
fn Expression(e1: Expression, _s: Symbol(LogicalOr), e2: Expression) -> ExprBlock {
    e1.bin_op(BinOp::LogicalOr, e2)
}
/*fn Expression(
    cond: Expression,
    _s1: Symbol(Trinocular),
    e1: Expression,
    _s2: Symbol(Colon),
    e2: Expression,
) -> Block {
}*/
fn AssignOperator(_s: Symbol(Assignment)) -> Option<BinOp> {
    None
}
fn AssignOperator(_s: Symbol(MultiAssign)) -> Option<BinOp> {
    Some(BinOp::Star)
}
fn AssignOperator(_s: Symbol(ModAssign)) -> Option<BinOp> {
    Some(BinOp::Mod)
}
fn AssignOperator(_s: Symbol(XorAssign)) -> Option<BinOp> {
    Some(BinOp::Xor)
}
fn AssignOperator(_s: Symbol(AddAssign)) -> Option<BinOp> {
    Some(BinOp::Add)
}
fn AssignOperator(_s: Symbol(SubAssign)) -> Option<BinOp> {
    Some(BinOp::Sub)
}
fn AssignOperator(_s: Symbol(DivAssign)) -> Option<BinOp> {
    Some(BinOp::Divide)
}
fn AssignOperator(_s: Symbol(LeftShiftAssign)) -> Option<BinOp> {
    Some(BinOp::LeftShift)
}
fn AssignOperator(_s: Symbol(RightShiftAssign)) -> Option<BinOp> {
    Some(BinOp::RightShift)
}
fn AssignOperator(_s: Symbol(AndAssign)) -> Option<BinOp> {
    Some(BinOp::And)
}
fn AssignOperator(_s: Symbol(OrAssign)) -> Option<BinOp> {
    Some(BinOp::Or)
}
// TODO: , Symbol(Comma)*/
