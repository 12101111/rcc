use std::iter::Peekable;
use std::str::Chars;

pub struct TokenStream<'code> {
    chars: Peekable<Chars<'code>>,
    line: usize,
    col: usize,
    pos: usize,
}

#[derive(Clone,Debug, PartialEq)]
pub struct TokenItem {
    pub token: Token,
    pub line: usize,
    pub col: usize,
    pub pos: usize,
}

macro_rules! token_enum {
    ($class:ident { $($name:ident($type:ty)),*}) => {
        #[derive(Clone,Debug, PartialEq)]
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

token_enum! {
    Token{
        Ident(String),
        KeyWord(KeyWord),
        Symbol(Symbol),
        Constant(Constant),
        Literal(String),
        Comment(String)
    }
}

token_enum! {
    Constant {
        Char(u8),
        Int(i64),
        UInt(u64),
        Float(f64)
    }
}

macro_rules! token {
    ($class:ident { $($name:ident:$value:expr),*}) => {
        #[derive(Clone,Copy,Debug,PartialEq)]
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
        //Suffix is Equal =
        Assignment:"=",
        Equal:"==",
        LogicalNot:"!",
        NotEqual:"!=",
        Star:"*",
        MultiAssign:"*=",
        Mod:"%",
        ModAssign:"%=",
        Xor:"^",
        XorAssign:"^=",

        Add:"+",
        Inc:"++",
        AddAssign:"+=",

        Sub:"-",
        Dec:"--",
        SubAssign:"-=",
        Arrow:"->",

        Divide:"/",
        DivAssign:"/=",
        //OneLineComment:"//",
        //MultiLineComment:"/*",

        Less:"<",
        LessEqual:"<=",
        LeftShift:"<<",
        LeftShiftAssign:"<<=",

        Greater:">",
        GreaterEqual:">=",
        RightShift:">>",
        RightShiftAssign:">>=",

        And:"&",
        LogicalAnd:"&&",
        AndAssign:"&=",

        Or:"|",
        LogicalOr:"||",
        OrAssign:"|=",

        Dot:".",
        Ellipsis:"...",

        //No Suffix
        Negation:"~",
        Trinocular:"?",
        Colon:":",
        Comma:",",
        Semicolon:";",
        LeftBrace:"{",
        RightBrace:"}",
        LeftBracket:"[",
        RightBracket:"]",
        LeftParen:"(",
        RightParen:")"
    }
}

impl<'code> TokenStream<'code> {
    pub fn new(code: &'code str) -> TokenStream<'code> {
        TokenStream {
            chars: code.chars().peekable(),
            line: 1,
            col: 1,
            pos: 0,
        }
    }
    #[cfg(test)]
    pub fn fail(&self, msg: &str) -> ! {
        panic!("\nError: {}\nLine:{} ,Col:{}\n", msg, self.line, self.col);
    }
    #[cfg(not(test))]
    pub fn fail(&self, msg: &str) -> ! {
        println!();
        eprintln!("Error: {}\nLine:{} ,Col:{}", msg, self.line, self.col);
        std::process::exit(1)
    }
    fn next_char_or_none(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            self.pos += 1;
            if c == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
            Some(c)
        } else {
            None
        }
    }
    fn next_char(&mut self, mag: &str) -> char {
        if let Some(c) = self.chars.next() {
            self.pos += 1;
            if c == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
            c
        } else {
            self.fail(mag)
        }
    }
    fn next_is_valid_ident_suffix(&mut self) -> bool {
        if let Some(&c) = self.chars.peek() {
            if let 'A'...'Z' | 'a'...'z' | '_' | '0'...'9' = c {
                true
            } else {
                false
            }
        } else {
            false
        }
    }
    fn next_is_digits(&mut self) -> bool {
        if let Some(&c) = self.chars.peek() {
            match c {
                '0'...'9' => true,
                _ => false,
            }
        } else {
            false
        }
    }
    fn next_is_hex(&mut self) -> bool {
        if let Some(&c) = self.chars.peek() {
            match c {
                '0'...'9' | 'a'...'f' | 'A'...'F' => true,
                _ => false,
            }
        } else {
            false
        }
    }
    fn next_is_oct(&mut self) -> bool {
        if let Some(&c) = self.chars.peek() {
            match c {
                '0'...'7' => true,
                _ => false,
            }
        } else {
            false
        }
    }
    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.chars.peek() {
            if !c.is_whitespace() {
                break;
            } else {
                let _ = self.next_char("");
            }
        }
    }
    fn next_not_whitespace_char(&mut self) -> Option<char> {
        self.skip_whitespace();
        self.next_char_or_none()
    }
    fn map_escape(&mut self, c: char) -> u8 {
        // https://en.wikipedia.org/wiki/Escape_sequences_in_C
        match c {
            'a' => 0x7,
            'b' => 0x8,
            'e' => 0x1B,
            'f' => 0x0C,
            'n' => 0x0A,
            'r' => 0x0D,
            't' => 0x09,
            'v' => 0x0B,
            '\\' => 0x5C,
            '\'' => 0x27,
            '"' => 0x22,
            '?' => 0x3F,
            _ => self.fail("Unknown escape sequence"),
        }
    }
    fn make_token(&self, token: Token) -> TokenItem {
        TokenItem {
            token,
            line: self.line,
            col: self.col,
            pos: self.pos,
        }
    }
    fn next_token(&mut self) -> Option<Token> {
        if let Some(c) = self.next_not_whitespace_char() {
            match c {
                '"' => {
                    let mut s = String::new();
                    while let Some(c) = self.next_char_or_none() {
                        match c {
                            '"' => return Some(Token::Literal(s)),
                            '\\' => {
                                let next = self.next_char("Unclosed string");
                                s.push(self.map_escape(next) as u8 as char);
                            }
                            _ => s.push(c),
                        }
                    }
                    self.fail("Unclosed string")
                }
                '\'' => {
                    let c = match self.next_char("Unclosed char") {
                        '\\' => {
                            let c = self.next_char("Unclosed char");
                            self.map_escape(c)
                        }
                        '\'' => self.fail("Empty character"),
                        c @ _ => c as u8,
                    };
                    if self.next_char("Unclosed character") == '\'' {
                        Some(Token::Constant(Constant::Char(c)))
                    } else {
                        self.fail("more than one character in char")
                    }
                }
                '0' => {
                    if let Some(&next) = self.chars.peek() {
                        match next {
                            'x' | 'X' => {
                                let mut num = String::new();
                                let _ = self.next_char("");
                                while self.next_is_hex() {
                                    num.push(self.next_char(""))
                                }
                                if let Ok(int) = u64::from_str_radix(&num, 16) {
                                    Some(Token::Constant(Constant::UInt(int)))
                                } else {
                                    self.fail(&format!("Invalid number: 0x{}", num));
                                }
                            }
                            '0'...'9' => {
                                let mut num = String::new();
                                while self.next_is_oct() {
                                    num.push(self.next_char(""))
                                }
                                if let Ok(int) = u64::from_str_radix(&num, 8) {
                                    Some(Token::Constant(Constant::UInt(int)))
                                } else {
                                    self.fail(&format!("Invalid number: 0{}", num));
                                }
                            }
                            '.' => {
                                let mut num = String::new();
                                num.push('0');
                                num.push(self.next_char(""));
                                while self.next_is_digits() {
                                    num.push(self.next_char(""))
                                }
                                Some(Token::Constant(Constant::Float(
                                    num.parse::<f64>().unwrap(),
                                )))
                            }
                            _ => Some(Token::Constant(Constant::Int(0))),
                        }
                    } else {
                        Some(Token::Constant(Constant::Int(0)))
                    }
                }
                '1'...'9' => {
                    let mut num = String::new();
                    num.push(c);
                    while self.next_is_digits() {
                        num.push(self.next_char(""))
                    }
                    if self.chars.peek() == Some(&'.') {
                        num.push(self.next_char(""));
                        while self.next_is_digits() {
                            num.push(self.next_char(""))
                        }
                        Some(Token::Constant(Constant::Float(
                            num.parse::<f64>().unwrap(),
                        )))
                    } else {
                        Some(Token::Constant(Constant::Int(num.parse::<i64>().unwrap())))
                    }
                }
                'A'...'Z' | 'a'...'z' | '_' => {
                    let mut ident = String::new();
                    ident.push(c);
                    while self.next_is_valid_ident_suffix() {
                        ident.push(self.next_char(""));
                    }
                    if let Some(keyword) = KeyWord::map(&ident) {
                        Some(Token::KeyWord(keyword))
                    } else {
                        Some(Token::Ident(ident))
                    }
                }
                _ => {
                    let mut sym = String::new();
                    sym.push(c);
                    match c {
                        '~' | '?' | ':' | ',' | ';' | '{' | '}' | '[' | ']' | '(' | ')' => {}
                        '=' | '!' | '*' | '%' | '^' => {
                            if self.chars.peek() == Some(&'=') {
                                sym.push(self.next_char(""));
                            }
                        }
                        '+' | '&' | '|' => {
                            if let Some(&ch) = self.chars.peek() {
                                if ch == c || ch == '=' {
                                    sym.push(self.next_char(""));
                                }
                            }
                        }
                        '<' | '>' => {
                            if self.chars.peek() == Some(&c) {
                                sym.push(self.next_char(""));
                            }
                            if self.chars.peek() == Some(&'=') {
                                sym.push(self.next_char(""));
                            }
                        }
                        '-' => {
                            if let Some(&ch) = self.chars.peek() {
                                if ch == '-' || ch == '=' || ch == '>' {
                                    sym.push(self.next_char(""));
                                }
                            }
                        }
                        '.' => {
                            if let Some(&'.') = self.chars.peek() {
                                sym.push(self.next_char(""));
                                if self.next_char("Unexpected \"..\"") == '.' {
                                    sym.push(self.next_char(""));
                                }
                            }
                        }
                        '/' => {
                            if let Some(&c) = self.chars.peek() {
                                match c {
                                    '/' => {
                                        let _ = self.next_char("");
                                        let mut comment = String::new();
                                        while let Some(c) = self.next_char_or_none() {
                                            if c == '\n' {
                                                return Some(Token::Comment(comment));
                                            } else {
                                                comment.push(c);
                                            }
                                        }
                                        return Some(Token::Comment(comment));
                                    }
                                    '*' => {
                                        let _ = self.next_char("");
                                        let mut comment = String::new();
                                        while let Some(c) = self.next_char_or_none() {
                                            if c == '*' {
                                                if self.chars.peek() == Some(&'/') {
                                                    let _ = self.next_char("");
                                                    return Some(Token::Comment(comment));
                                                }
                                            }
                                            comment.push(c)
                                        }
                                        self.fail("Unclosed multi-line comment");
                                    }
                                    '=' => sym.push(self.next_char("")),
                                    _ => {}
                                }
                            }
                        }
                        _ => self.fail(&format!("invalid character: {}", c)),
                    }
                    Some(Token::Symbol(Symbol::map(&sym).unwrap()))
                }
            }
        } else {
            None
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = TokenItem;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(token) = self.next_token() {
            match token {
                Token::Comment(_) => continue,
                _ => return Some(self.make_token(token)),
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! fail {
        ($id:ident,$code:expr) => {
            #[test]
            #[should_panic]
            fn $id() {
                for _ in Lexer::new($code) {}
            }
        };
    }

    macro_rules! success {
        ($id:ident,$code:expr) => {
            #[test]
            fn $id() {
                for _ in Lexer::new($code) {}
            }
        };
    }

    macro_rules! token_eq {
        ($id:ident,$raw:expr,$lex:expr) => {
            #[test]
            fn $id() {
                assert_eq!(Lexer::new($raw).next().unwrap().token, $lex);
            }
        };
    }

    macro_rules! string_token_eq {
        ($id:ident,$raw:expr,$lex:expr) => {
            token_eq!($id, $raw, Token::Literal($lex.to_owned()));
        };
    }

    macro_rules! char_token_eq {
        ($id:ident,$raw:expr,$lex:expr) => {
            token_eq!($id, $raw, Token::Constant(Constant::Char($lex as u8)));
        };
    }

    macro_rules! int_token_eq {
        ($id:ident,$raw:expr,$lex:expr) => {
            token_eq!($id, $raw, Token::Constant(Constant::Int($lex as i64)));
        };
    }

    macro_rules! uint_token_eq {
        ($id:ident,$raw:expr,$lex:expr) => {
            token_eq!($id, $raw, Token::Constant(Constant::UInt($lex as u64)));
        };
    }

    macro_rules! float_token_eq {
        ($id:ident,$raw:expr,$lex:expr) => {
            token_eq!($id, $raw, Token::Constant(Constant::Float($lex as f64)));
        };
    }

    fail!(macro_not_handled, "#");
    fail!(macro_after_ident, "int a#ifdef");
    success!(macro_in_comment1, "//#define a");
    success!(macro_in_comment2, "/*\n#define a\n*/");

    string_token_eq!(normol_string, "\"Hello,World\n\"", "Hello,World\n");
    fail!(unclosed_string, "\"Hello,World");
    fail!(unexpected_escape_in_string, "\"\\\"");
    fail!(unexpected_escape_in_string2, "\"\\\r\"");
    fail!(invalid_escape_in_string1, "\"\\s\"");
    fail!(invalid_escape_in_string2, "\"\\.\"");
    success!(valid_escape_in_string1, "\"\\a\"");
    success!(valid_escape_in_string2, "\"\\n\"");
    success!(valid_escape_in_string3, "\"\\\"\"");
    success!(valid_escape_in_string4, "\"\\\\\"");
    success!(valid_escape_in_string5, "\"\'\"");
    success!(valid_escape_in_string6, "\"?\"");
    string_token_eq!(string_empty, "\"\"", "");

    char_token_eq!(char1, "\'a\'", 'a');
    char_token_eq!(char2, "\'?\'", '?');
    char_token_eq!(char3, "\'\\?\'", '?');
    char_token_eq!(char4, "\'\\'\'", '\'');
    fail!(unclosed_char0, "\'");
    fail!(unclosed_char1, "\'\\");
    fail!(unclosed_char2, "\'\\\'");
    fail!(unclosed_char3, "\'ab\'");
    fail!(empty_char, "\'\'");

    uint_token_eq!(zero0, "0x0", 0);
    int_token_eq!(zero1, "0", 0);
    uint_token_eq!(zero2, "00", 0);
    uint_token_eq!(hex0, "0x10", 16);
    uint_token_eq!(hex1, "0xff", 255);
    uint_token_eq!(hex2, "0xAd", 0xad);
    uint_token_eq!(oct, "070", 56);
    float_token_eq!(f1, "0.0", 0.0);
    float_token_eq!(f2, "10.5", 10.5);
    float_token_eq!(f3, "0.5", 0.5);
}
