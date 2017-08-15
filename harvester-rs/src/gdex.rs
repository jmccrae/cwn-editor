use tokenize::tokenize;
use std::collections::{HashMap,HashSet};
use std::cmp::Ordering;

pub fn top_n(elems : &HashMap<String, f64>, n : usize) -> HashSet<String> {
    let mut v : Vec<String> = elems.keys().map(|s| s.clone()).collect();
    v.sort_by(|x,y| {
        let sx = elems[x];
        let sy = elems[y];
        if sx < sy {
            Ordering::Greater
        } else if sx > sy {
            Ordering::Less
        } else {
            Ordering::Equal
        }
    });
    v.into_iter().take(n).collect()
}

static LENGTH_WT : f64 = 1.0;
static FREQ_WT : f64 = 1.0;
static ANAPHOR_WT : f64 = 1.0;
static SENT_WT : f64 = 1.0;


fn length_score(words : &Vec<String>) -> f64 {
    if words.len() >= 10 && words.len() <= 25 {
        1.0
    } else {
        0.0
    }
}

fn freq_score(words : &Vec<String>, top_words : &HashSet<String>) -> f64 {
    let mut freq_words : usize = 0;
    for word in words {
        if top_words.contains(&word.to_lowercase()) {
            freq_words += 1;
        }
    }
    (freq_words as f64) / (words.len() as f64)
}

fn anaphor_score(words : &Vec<String>) -> f64 {
    for word in words {
        let wordl = word.to_lowercase();
        if wordl == "this" || wordl == "that" || wordl == "one" || 
            wordl == "it" || wordl == "http" {
            return 0.0
        }
    }
    1.0
}

fn sent_score(words : &Vec<String>) -> f64 {
    if char::is_uppercase(words[0].chars().nth(0).unwrap_or('a')) &&
        (words[words.len() - 1] == "." ||
         words[words.len() - 1] == "?" ||
         words[words.len() - 1] == "!") {
        1.0
    } else {
        0.0
    }
}

pub fn gdex(example : &str, top_words : &HashSet<String>) -> f64 {
    let words = tokenize(example);
    LENGTH_WT * length_score(&words) +
        FREQ_WT * freq_score(&words, top_words) +
        ANAPHOR_WT * anaphor_score(&words) +
        SENT_WT * sent_score(&words)
}
