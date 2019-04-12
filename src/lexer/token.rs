macro_rules! token_enum {
    ($class:ident { $($name:ident($type:ty)),*}) => {
        #[derive(Debug, PartialEq)]
        pub enum $class{
            $($name($type),)*
        }

        impl std::fmt::Display for $class{
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self{
                    $($class::$name(s)=>write!(f,"{}",s),)*
                }
            }
        }
    };
}

token_enum!{
    Token{
        Ident(String),
        KeyWord(KeyWord),
        Symbol(Symbol),
        Constant(Constant),
        Literal(String)
    }
}

token_enum!{
    Constant {
        Char(i8),
        UChar(u8),
        Short(i16),
        UShort(u16),
        Int(i32),
        UInt(u32),
        LLong(i64),
        ULLong(u64),
        Float(f32),
        Double(f64)
    }
}

macro_rules! token {
    ($class:ident { $($name:ident:$value:expr),*}) => {
        #[derive(Debug, PartialEq)]
        pub enum $class{
            $($name,)*
        }

        impl $class{
            pub fn map(val:&str)->Option<Self>{
                match val{
                    $($value=>Some($class::$name),)*
                    _=>None,
                }
            }
        }
        impl std::fmt::Display for $class{
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self{
                    $($class::$name=>write!(f,"{}",$value),)*
                }
            }
        }
    };
}

token! {
    KeyWord {
        //基本数据类型
        Char:"char",
        Double:"double",
        Float:"float",
        Int:"int",
        Long:"long",
        Short:"short",
        Signed:"signed",
        Unsigned:"unsigned",
        Void:"void",
        //复合数据类型
        Enum:"enum",
        Struct:"struct",
        Union:"union",
        //流程控制
        Break:"break",
        Case:"case",
        Continue:"continue",
        Default:"default",
        Do:"do",
        Else:"else",
        For:"for",
        Goto:"goto",
        If:"if",
        Return:"return",
        Switch:"switch",
        While:"while",
        //变量存储
        Auto:"auto",
        Const:"const",
        Extern:"extern",
        Register:"register",
        Static:"static",
        Volatile:"volatile",
        //功能
        Sizeof:"sizeof",
        Typedef:"typedef"
    }
}

token! {
    Symbol{
        Assignment:"=",
        Add:"+",
        Inc:"++",
        AddAssign:"+=",
        Sub:"-",
        Dec:"--",
        SubAssign:"-=",
        Star:"*",
        MultiAssign:"*=",
        Divide:"/",
        DivAssign:"/=",
        Mod:"%",
        ModAssign:"%=",
        LeftShift:"<<",
        LeftShiftAssign:"<<=",
        RightShift:">>",
        RightShiftAssign:">>=",
        And:"&",
        AndAssign:"&=",
        Or:"|",
        OrAssign:"|=",
        Xor:"^",
        XorAssign:"^=",
        Negation:"~",
        LogicalNot:"!",
        LogicalAnd:"&&",
        LogicalOr:"||",
        Equal:"==",
        NotEqual:"!=",
        Less:"<",
        LessEqual:"<=",
        Greater:">",
        GreaterEqual:">=",
        Trinocular:"?",
        Colon:":",
        Dot:".",
        Arrow:"->",
        Comma:",",
        Semicolon:";",
        Ellipsis:"...",
        LeftBrace:"{",
        RightBrace:"}",
        LeftParen:"(",
        RightParen:")",
        LeftBracket:"[",
        RightBracket:"]"
    }
}
