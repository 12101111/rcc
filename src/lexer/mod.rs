use std::iter::Peekable;
use std::str::Chars;
mod token;
use token::*;
pub struct Lexer<'code> {
    chars: Peekable<Chars<'code>>,
    line: usize,
    col: usize,
}

impl<'code> Lexer<'code> {
    pub fn new(code: &'code str) -> Lexer<'code> {
        Lexer {
            chars: code.chars().peekable(),
            line: 1,
            col: 1,
        }
    }
    #[cfg(test)]
    fn fail(&self, msg: &str) -> ! {
        panic!("\nError: {}\nLine:{} ,Col:{}\n", msg, self.line, self.col);
    }
    #[cfg(not(test))]
    fn fail(&self, msg: &str) -> ! {
        eprintln!("Error: {}\nLine:{} ,Col:{}", msg, self.line, self.col);
        std::process::exit(1);
    }
    fn next_char(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
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
    fn skip_oneline_comment(&mut self) {
        while let Some(c) = self.next_char() {
            if c == '\n' {
                return;
            }
        }
    }
    fn skip_multiline_comment(&mut self) {
        while let Some(c) = self.next_char() {
            if c == '*' {
                if self.next_char() == Some('/') {
                    return;
                }
            }
        }
        self.fail("Unclosed multi line comment");
    }
    fn next_is_valid_symbol(&mut self) -> bool {
        if let Some(&c) = self.chars.peek() {
            match c {
                '!' | '"' | '%'...'/' | ':'...'?' | '[' | ']' | '^' | '{'...'~' => true,
                _ => false,
            }
        } else {
            false
        }
    }
    fn next_is_valid_ident_suffix(&mut self) -> bool {
        if let Some(&c) = self.chars.peek() {
            match c {
                'A'...'Z' | 'a'...'z' | '_' | '0'...'9' => true,
                _ => false,
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
                let _ = self.next_char();
            }
        }
    }
    fn next_not_whitespace_char(&mut self) -> Option<char> {
        self.skip_whitespace();
        self.next_char()
    }
    fn map_escape(&mut self, c: char) -> i8 {
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
}
/*
原始C89定义
D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*

{L}({L}|{D})*           标识符/关键字
0[xX]{H}+{IS}?	        16进制整数
0{D}+{IS}?		        8进制整数
{D}+{IS}?		        10进制整数
L?'(\\.|[^\\'])+'	    char
{D}+{E}{FS}?		    科学计数法整数
{D}*"."{D}+({E})?{FS}?	小数 注意,单个.是不合法的,.之前或之后必须有数字
{D}+"."{D}*({E})?{FS}?	小数 但是只有一边有数字是合法的
L?\"(\\.|[^\\"])*\"	    字符串字面量 STRING LITERAL
*/

/*
简化
不支持宽字符,Unicode
不支持小数的小数点前或后省略数字
不支持科学计数法
D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
                                                        前缀
{L}({L}|{D})*           标识符/关键字                   a-zA-Z_
0[xX]{H}+               16进制整数                      0
{D}+		            10进制整数                      1-9
'(\\.|[^\\'])+' 	    char                            '
{D}+"."{D}+	            小数                            0-9
\"(\\.|[^\\"])*\"	    字符串字面量 STRING LITERAL     "


*/
impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(c) = self.next_not_whitespace_char() {
            match c {
                '#' => self.fail("Macro not handled"),
                '"' => {
                    let mut s = String::new();
                    while let Some(c) = self.next_char() {
                        if c == '"' {
                            return Some(Token::Literal(s));
                        } else if c == '\\' {
                            if let Some(next) = self.next_char() {
                                if next == '\r' {
                                    if let Some(next) = self.next_char() {
                                        if next == '\n' {
                                            continue;
                                        }
                                    } else {
                                        self.fail("Unclosed string")
                                    }
                                } else if next == '\n' {
                                    continue;
                                }
                                let c = self.map_escape(next);
                                s.push(c as u8 as char);
                            } else {
                                self.fail("Unclosed string")
                            }
                        } else {
                            s.push(c)
                        }
                    }
                    self.fail("Unclosed string")
                }
                '\'' => {
                    if let Some(c) = self.next_char() {
                        if c == '\\' {
                            if let Some(next) = self.next_char() {
                                let c = self.map_escape(next);
                                if self.next_char() == Some('\'') {
                                    return Some(Token::Constant(Constant::Char(c)));
                                } else {
                                    self.fail("Unclosed character")
                                }
                            } else {
                                self.fail("Unclosed character")
                            }
                        } else if c == '\'' {
                            self.fail("Empty character")
                        } else {
                            if self.next_char() == Some('\'') {
                                return Some(Token::Constant(Constant::Char(c as i8)));
                            } else {
                                self.fail("Unclosed character")
                            }
                        }
                    } else {
                        self.fail("Unclosed '\'")
                    }
                }
                '0'...'9' => {
                    let mut num = String::new();
                    num.push(c);
                    if c == '0' {
                        if let Some(&next) = self.chars.peek() {
                            match next {
                                'x' | 'X' => {
                                    num.pop();
                                    let _ = self.next_char();
                                    while self.next_is_hex() {
                                        num.push(self.next_char().unwrap())
                                    }
                                    if let Ok(int) = u64::from_str_radix(&num, 16) {
                                        return Some(Token::Constant(Constant::ULLong(int)));
                                    } else {
                                        self.fail(&format!("Invalid number: 0x{}", num));
                                    }
                                }
                                '0'...'9' => {
                                    num.pop();
                                    while self.next_is_oct() {
                                        num.push(self.next_char().unwrap())
                                    }
                                    if let Ok(int) = u64::from_str_radix(&num, 8) {
                                        return Some(Token::Constant(Constant::ULLong(int)));
                                    } else {
                                        self.fail(&format!("Invalid number: 0{}", num));
                                    }
                                }
                                _ => return Some(Token::Constant(Constant::Int(0))),
                            }
                        }
                    }
                    while self.next_is_digits() {
                        num.push(self.next_char().unwrap())
                    }
                    if self.chars.peek() == Some(&'.') {
                        num.push(self.next_char().unwrap());
                        while self.next_is_digits() {
                            num.push(self.next_char().unwrap())
                        }
                        return Some(Token::Constant(Constant::Double(
                            num.parse::<f64>().unwrap(),
                        )));
                    } else {
                        return Some(Token::Constant(Constant::Int(num.parse::<i32>().unwrap())));
                    }
                }
                'A'...'Z' | 'a'...'z' | '_' => {
                    let mut ident = String::new();
                    ident.push(c);
                    while self.next_is_valid_ident_suffix() {
                        ident.push(self.next_char().unwrap());
                    }
                    if let Some(keyword) = KeyWord::map(&ident) {
                        return Some(Token::KeyWord(keyword));
                    } else {
                        return Some(Token::Ident(ident));
                    }
                }
                '\\' => {
                    if let Some(c) = self.next_char() {
                        if c == '\n' || c == '\r' {
                            continue;
                        }
                    }
                    self.fail("Unexpected \"\\\"")
                }
                //              ( ) * + , - . /  : ; < = > ?                 { | } ~
                '!' | '%' | '&' | '('...'/' | ':'...'?' | '[' | ']' | '^' | '{'...'~' => {
                    let mut sym = String::new();
                    sym.push(c);
                    if self.next_is_valid_symbol() {
                        sym.push(*self.chars.peek().unwrap());
                        if sym == ".." {
                            let _ = self.next_char();
                            if self.next_char() == Some('.') {
                                return Some(Token::Symbol(Symbol::Ellipsis));
                            }
                            self.fail("Unexpected \"..\"")
                        }
                        if let Some(symbol) = Symbol::map(&sym) {
                            let _ = self.next_char();
                            if (sym == "<<" || sym == ">>") && self.chars.peek() == Some(&'=') {
                                let _ = self.next_char();
                                match sym.as_ref() {
                                    "<<=" => return Some(Token::Symbol(Symbol::LeftShiftAssign)),
                                    ">>=" => return Some(Token::Symbol(Symbol::RightShiftAssign)),
                                    _ => unreachable!(),
                                }
                            }
                            return Some(Token::Symbol(symbol));
                        }
                        if sym == "/*" {
                            self.skip_multiline_comment();
                            continue;
                        }
                        if sym == "//" {
                            self.skip_oneline_comment();
                            continue;
                        }
                        sym.pop();
                    }
                    if let Some(sym) = Symbol::map(&sym) {
                        return Some(Token::Symbol(sym));
                    } else {
                        self.fail(&format!("Unexpected \"{}\"", c))
                    }
                }
                // $ @ ` control code,non-ascii
                _ => self.fail(&format!("invalid character: {}", c)),
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

    fail!(macro_not_handled, "#");
    fail!(macro_after_ident, "int a#ifdef");
    success!(macro_in_comment1, "//#define a");
    success!(macro_in_comment2, "/*\n#define a\n*/");

    success!(normol_string, "\"Hello,World\"");
    fail!(unexpected_escape_in_string, "\"\\\"");
    fail!(invalid_escape_in_string1, "\"\\s\"");
    fail!(invalid_escape_in_string2, "\"\\.\"");
    success!(valid_escape_in_string1, "\"\\a\"");
    success!(valid_escape_in_string2, "\"\\n\"");
    success!(valid_escape_in_string3, "\"\\\"\"");
    success!(valid_escape_in_string4, "\"\\\\\"");
    success!(valid_escape_in_string5, "\"\'\"");
    success!(valid_escape_in_string6, "\"?\"");

    #[test]
    fn string_empty() {
        assert_eq!(
            Some(Token::Literal("".to_owned())),
            Lexer::new("\"\"").next()
        );
    }
    #[test]
    fn char_1() {
        assert_eq!(
            Some(Token::Constant(Constant::Char(b'a' as i8))),
            Lexer::new("\'a\'").next()
        );
    }
    fail!(unclosed_char, "\'\\");

}
