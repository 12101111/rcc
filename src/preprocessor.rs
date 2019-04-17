/// Only deal with line start with # and end with \\r\n and \\n
pub fn preprocess(s: String) -> String {
    let mut r = String::new();
    let mut s = s.split('\n');
    while let Some(s) = s.next() {
        if s.trim_start().bytes().nth(0) == Some(b'#') {
            eprintln!("rcc don't support macro now,all macro will be ignored!");
            eprintln!("marco ignored:{}", s);
        } else if s.ends_with("\\\r") {
            r.push_str(&s[..s.len() - 2]);
        } else if s.ends_with("\\") {
            r.push_str(&s[..s.len() - 1])
        } else {
            r.push_str(s);
            r.push('\n');
        }
    }
    r
}