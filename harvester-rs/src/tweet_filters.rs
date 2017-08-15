extern crate regex;

use regex::Regex;
use std::fmt::Display;
use std::io::prelude::*;
use std::fs::File;
use std::io::BufReader;
use std::collections::HashSet;
use std::slice::Iter;
use tokenize::tokenize;


trait ErrStr<T> {
    fn s(self) -> Result<T,String>;
}

impl<T,E:Display> ErrStr<T> for Result<T,E> {
    fn s(self) -> Result<T,String> {
        self.map_err(|e| format!("{}", e))
    }
}

fn load_dic(words : &str) -> Result<HashSet<String>, String> {

    let chars_non_words = Regex::new("[B-HJ-LN-TV-Zb-hj-ln-tv-z]").s()?;
    let dictionary_full : Vec<String> = BufReader::new(
        File::open(words).s()?).lines().map(|_line| {
        _line.unwrap_or("".to_string())
    }).collect();
    let dictionary : HashSet<String> = dictionary_full.iter()
        .filter(|l| {
            !l.contains(" ") &&
            !l.contains("-") &&
            (l.len() > 1 || !chars_non_words.is_match(l))
        })
        .map(|c| c.to_lowercase())
        .collect();
    Ok(dictionary)
}

static LOTS_OF_SMALL_WORDS_THRESHOLD : f64 = 0.3;

pub fn lots_of_small_words(tokens : Iter<String>) -> bool {
    let mut l = 0;
    let mut lshort = 0;
    for word in tokens {
        if word.len() <= 2 {
            lshort += 1;
        }
        l += 1;
    }
    l == 0 || (lshort as f64) / (l as f64) > LOTS_OF_SMALL_WORDS_THRESHOLD
}

pub fn lots_of_newlines(tw : &str) -> bool {
    tw.split("\n").collect::<Vec<&str>>().len() > 2
}

//fn is_uppercase(s : &str) -> bool {
//    s.chars().all(|c| char::is_uppercase(c))
//}
//
//fn is_alpha(s : &str) -> bool {
//    s.chars().all(|c| char::is_alphabetic(c))
//}
//
//
///// Intended for manual detection of obscure acronyms
//pub fn remove_obscure_acronyms(tw : &str, bad_all_caps : &HashSet<String>,
//                           ok_all_caps : &HashSet<String>,
//                           dictionary : &HashSet<String>) -> bool {
//    let mut ret = false;
//    for tok in tokenize(tw) {
//        if bad_all_caps.contains(&tok) {
//            ret = true
//        } else if is_uppercase(&tok) &&
//            is_alpha(&tok) &&
//            !ok_all_caps.contains(&tok) &&
//            !dictionary.contains(&tok) &&
//            !dictionary.contains(&tok.to_lowercase()) {
//            println!("{}", tok);
//        }
//    }
//    ret
//}

static PROPORTION_TAGS_MENTION_THRESHOLD : f64 = 0.3;

pub fn proportion_tag_mentions(_tokens : Iter<String>) -> bool {
    let mut tags_mentions = 0.0;
    let mut tokens = 0.0;
    for tok in _tokens {
        tok.chars().nth(0).map(|c| {
            if c == '@' || c == '#' {
                tags_mentions += 1.0;
            }
        });
        tokens += 1.0;
    }
    tags_mentions / tokens > PROPORTION_TAGS_MENTION_THRESHOLD
}

static DICTIONARY_WORDS : f64 = 0.7;

pub fn lots_of_non_dict_words(tokens : Iter<String>, dictionary : &HashSet<String>) -> bool {
    let mut i = 0.0;
    let mut j = 0.0;
    for tok in tokens {
        if dictionary.contains(&tok.to_lowercase()) {
            i += 1.0;
        }
        j += 1.0;
    }
    i / j < DICTIONARY_WORDS
}

pub struct TweetFilter{
//    bad_all_caps : HashSet<String>,               
//    ok_all_caps : HashSet<String>, 
    dictionary : HashSet<String>
}

impl TweetFilter {
    pub fn filter(&self, tw : &str) -> bool {
        let tokens = tokenize(tw);
       !lots_of_small_words(tokens.iter()) &&
       !lots_of_newlines(tw) &&
//       !remove_obscure_acronyms(tw, &self.bad_all_caps, &self.ok_all_caps,
//                                &self.dictionary) &&
       !lots_of_non_dict_words(tokens.iter(), &self.dictionary) &&                                
       !proportion_tag_mentions(tokens.iter())
    }
}

pub fn setup_tweet_filters(words : &str) -> Result<TweetFilter,String> {
    let dictionary = load_dic(words)?;
//
//    let mut bad_all_caps = HashSet::new();
//    let mut ok_all_caps = HashSet::new();
//
//    bad_all_caps.insert("ICMFP".to_string());
//    ok_all_caps.insert("CHIKFALA".to_string());
//    ok_all_caps.insert("D".to_string());
//    ok_all_caps.insert("E".to_string());
//    ok_all_caps.insert("F".to_string());
//    ok_all_caps.insert("P".to_string());
//    ok_all_caps.insert("S".to_string());
//    ok_all_caps.insert("SS".to_string());
//    ok_all_caps.insert("FOIIOW".to_string());
//    ok_all_caps.insert("FOIIOWING".to_string());
//    ok_all_caps.insert("JMU".to_string());
//    ok_all_caps.insert("MMA".to_string());
//    ok_all_caps.insert("MOMMIES".to_string());
//    ok_all_caps.insert("PSL".to_string());
//    ok_all_caps.insert("RT".to_string());
//    ok_all_caps.insert("SF".to_string());
//    ok_all_caps.insert("SFG".to_string());
//    ok_all_caps.insert("THATS".to_string());
//    ok_all_caps.insert("THERES".to_string());
//    ok_all_caps.insert("THESSE".to_string());
//    ok_all_caps.insert("TL".to_string());
    Ok(TweetFilter {
//        bad_all_caps: bad_all_caps,
//        ok_all_caps: ok_all_caps,
        dictionary: dictionary
    })
}
