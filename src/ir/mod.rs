mod op;
mod types;
use crate::syntax::{print_ast, Ast, Node, Nonterminal};
use std::collections::HashMap;
use types::{Func, TypedValue};

#[derive(Debug)]
pub struct Program {
    globals: HashMap<String, TypedValue>,
    funcs: HashMap<String, Func>,
}
impl Program {
    pub fn new(ast: Ast) -> Program {
        //1: Program->[Declaration,Program];
        assert_eq!(node.lhs, Nonterminal::Program);
        let mut globals = HashMap::new();
        let mut funcs = HashMap::new();
        let mut node = ast.unwrap();
        while node.child.len() == 2 {
            let mut child = node.child.into_iter();
            let mut decl = child.next().unwrap().unwrap(); //Declaration
            node = child.next().unwrap().unwrap(); //Program
            match decl.rule {
                // 3: Declaration->[VarDecl,Symbol(Semicolon)];
                3 => globals.extend(TypedValue::from_global_decl(decl)),
                // 4: Declaration->[FuncDef,Symbol(Semicolon)];
                // 5: Declaration->[FuncDef,Symbol(LeftBrace),Statements,Symbol(RightBrace)];
                _ => {
                    let func = Func::from_decl(decl);
                    funcs.insert(func.ident.clone(), func);
                }
            }
        }
        Program { funcs, globals }
    }
}
