use std::iter::Peekable;
use std::str::Chars;
mod token;
pub use token::*;
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
    pub fn fail(&self, msg: &str) -> ! {
        panic!("\nError: {}\nLine:{} ,Col:{}\n", msg, self.line, self.col);
    }
    #[cfg(not(test))]
    pub fn fail(&self, msg: &str) -> ! {
        println!("");
        eprintln!("Error: {}\nLine:{} ,Col:{}", msg, self.line, self.col);
        std::process::exit(1);
    }
    fn next_char_or_none(&mut self) -> Option<char> {
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
    fn next_char(&mut self, mag: &str) -> char {
        if let Some(c) = self.chars.next() {
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
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, usize, usize);
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(c) = self.next_not_whitespace_char() {
            match c {
                '"' => {
                    let mut s = String::new();
                    while let Some(c) = self.next_char_or_none() {
                        match c {
                            '"' => return Some((Token::Literal(s), self.line, self.col)),
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
                        Some((Token::Constant(Constant::Char(c)), self.line, self.col))
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
                                    Some((
                                        Token::Constant(Constant::UInt(int)),
                                        self.line,
                                        self.col,
                                    ))
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
                                    Some((
                                        Token::Constant(Constant::UInt(int)),
                                        self.line,
                                        self.col,
                                    ))
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
                                Some((
                                    Token::Constant(Constant::Float(num.parse::<f64>().unwrap())),
                                    self.line,
                                    self.col,
                                ))
                            }
                            _ => Some((Token::Constant(Constant::Int(0)), self.line, self.col)),
                        }
                    } else {
                        Some((Token::Constant(Constant::Int(0)), self.line, self.col))
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
                        Some((
                            Token::Constant(Constant::Float(num.parse::<f64>().unwrap())),
                            self.line,
                            self.col,
                        ))
                    } else {
                        Some((
                            Token::Constant(Constant::Int(num.parse::<i64>().unwrap())),
                            self.line,
                            self.col,
                        ))
                    }
                }
                'A'...'Z' | 'a'...'z' | '_' => {
                    let mut ident = String::new();
                    ident.push(c);
                    while self.next_is_valid_ident_suffix() {
                        ident.push(self.next_char(""));
                    }
                    if let Some(keyword) = KeyWord::map(&ident) {
                        Some((Token::KeyWord(keyword), self.line, self.col))
                    } else {
                        Some((Token::Ident(ident), self.line, self.col))
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
                                                return Some((
                                                    Token::Comment(comment),
                                                    self.line,
                                                    self.col,
                                                ));
                                            } else {
                                                comment.push(c);
                                            }
                                        }
                                        return Some((
                                            Token::Comment(comment),
                                            self.line,
                                            self.col,
                                        ));
                                    }
                                    '*' => {
                                        let _ = self.next_char("");
                                        let mut comment = String::new();
                                        while let Some(c) = self.next_char_or_none() {
                                            if c == '*' {
                                                if self.chars.peek() == Some(&'/') {
                                                    let _ = self.next_char("");
                                                    return Some((
                                                        Token::Comment(comment),
                                                        self.line,
                                                        self.col,
                                                    ));
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
                    Some((
                        Token::Symbol(Symbol::map(&sym).unwrap()),
                        self.line,
                        self.col,
                    ))
                }
            }
        } else {
            None
        }
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

    macro_rules! string_token_eq {
        ($id:ident,$raw:expr,$lex:expr) => {
            #[test]
            fn $id() {
                assert_eq!(
                    Token::Literal($lex.to_owned()),
                    Lexer::new($raw).next().unwrap().0
                );
            }
        };
    }

    macro_rules! float_token_eq {
        ($id:ident,$raw:expr,$lex:expr) => {
            #[test]
            fn $id() {
                assert_eq!(
                    Token::Constant(Constant::Float($lex as f64)),
                    Lexer::new($raw).next().unwrap().0
                );
            }
        };
    }

    macro_rules! char_token_eq {
        ($id:ident,$raw:expr,$lex:expr) => {
            #[test]
            fn $id() {
                assert_eq!(
                    Token::Constant(Constant::Char($lex as u8)),
                    Lexer::new($raw).next().unwrap().0
                );
            }
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

    float_token_eq!(f1, "0.5", 0.5);
}
