extern crate clap;
extern crate flate2;
extern crate serde;
extern crate serde_json;
extern crate regex;
#[macro_use] extern crate lazy_static;

mod tweet_filters;
mod tokenize;
mod gdex;

#[cfg(test)]
mod tests {
    use gdex::*;
    use tweet_filters::*;
    use tokenize::tokenize;
    use std::collections::HashSet;

    #[test]
    fn test_short_words() {

        assert!(lots_of_small_words(tokenize("a b c d").iter()));
        assert!(!lots_of_small_words(tokenize("monolingual enormous elephants").iter()));

    }

    #[test]
    fn test_newlines() {
        assert!(lots_of_newlines("foo\n\n\n\n\nbar"));
        assert!(!lots_of_newlines("foo\nbar"));
    }

    #[test]
    fn test_tag_mentions() {
        assert!(proportion_tag_mentions(tokenize("@foo #great #awesome").iter()));
        assert!(!proportion_tag_mentions(tokenize("just talk normally").iter()));
    }

    #[test]
    fn test_english() {
        let mut dictionary = HashSet::new();
        dictionary.insert("this".to_string());
        dictionary.insert("is".to_string());
        dictionary.insert("a".to_string());
        dictionary.insert("die".to_string());

        assert!(lots_of_non_dict_words(tokenize("die Test ist gut").iter(), &dictionary));
        assert!(!lots_of_non_dict_words(tokenize("This IS A test").iter(), &dictionary));
    }

    #[test]
    fn test_gdex() {
        let mut top_n = HashSet::new();
        top_n.insert("string".to_string());
        top_n.insert("is".to_string());
        top_n.insert("a".to_string());
        top_n.insert("good".to_string());
        top_n.insert("example".to_string());
        top_n.insert(".".to_string());
        top_n.insert(",".to_string());

        assert_eq!(gdex("A string is a good example, a good example a string is.",
                        &top_n), 
                   4.0);
        assert_eq!(gdex("one bad examples", &top_n), 0.0);
    }
                   

}
