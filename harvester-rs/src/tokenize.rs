use regex::Regex;

pub fn tokenize(s : &str) -> Vec<String> {
    lazy_static! {
        static ref PATTERN1  : Regex = Regex::new("(\\.\\.\\.+|[\\p{Po}\\p{Ps}\\p{Pe}\\p{Pi}\\p{Pf}\u{2013}\u{2014}\u{2015}])").unwrap();
        static ref PATTERN1A : Regex = Regex::new(" (\\.|') ").unwrap();
        static ref PATTERN1B : Regex = Regex::new("([^\\p{L}\\.])(\\.)([^\\p{L}\\.])|([^\\p{L}\\.])(\\.)$|([^\\.]\\p{L})(\\.)|([^\\p{L}])(')([^\\p{L}])|([^\\p{L}])(')$").unwrap();
        static ref PATTERN1C : Regex = Regex::new("(\\p{L})('s|n't)(\\p{Z}|$)").unwrap();
        static ref PATTERN2  : Regex = Regex::new("\\p{C}|^\\p{Z}+|\\p{Z}+$").unwrap();
        static ref PATTERN3  : Regex = Regex::new("\\p{Z}+").unwrap();
    }
    let s1 = PATTERN1.replace_all(s, " $1 ");
    let s1a = PATTERN1A.replace_all(&s1, "$1");
    //let s1b = PATTERN1b.replace_all(s1a.as_str(), "$1$4$6$8$11 $2$5$7$9$12 $3$10");
    let s1b = PATTERN1B.replace_all(&s1a, "$1$4$6 $2$5$7 $3");
    let s1c = PATTERN1C.replace_all(&s1b, "$1 $2$3");
    let s2 = PATTERN2.replace_all(&s1c, "");
    let s3 = PATTERN3.split(&s2);
    s3.map(|s| String::from(s)).collect()
}


