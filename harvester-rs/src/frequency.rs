extern crate clap;
extern crate flate2;
extern crate serde;
extern crate serde_xml;
extern crate regex;
#[macro_use] extern crate lazy_static;

mod tokenize;
mod gdex;

use clap::{App, Arg, ArgMatches};
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use regex::Regex;
use serde_xml::value::{Element,Content};
use std::cmp::Ordering;
use std::collections::{HashMap,HashSet};
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::io::prelude::*;
use std::path::Path;
use std::process::exit;
use tokenize::tokenize;

fn english_stopwords() -> HashSet<String> {
    ["a", "about", "above", "after", "again", 
     "against", "all", "am", "an", "and", "any", "are",
     "aren't", "as", "at", "be", "because", "been", 
     "before", "being", "below", "between", "both", 
     "but", "by", "can't", "cannot", "could", 
     "couldn't", "did", "didn't", "do", "does",
     "doesn't", "doing", "don't", "down", "during", 
     "each", "few", "for", "from", "further", "had",
     "hadn't", "has", "hasn't", "have", "haven't",
     "having", "he", "he'd", "he'll", "he's", "her",
     "here", "here's", "hers", "herself", "him",
     "himself", "his", "how", "how's", "i", "i'd",
     "i'll", "i'm", "i've", "if", "in", "into", "is",
     "isn't", "it", "it's", "its", "itself", "let's",
     "me", "more", "most", "mustn't", "my", "myself",
     "no", "nor", "not", "of", "off", "on", "once", 
     "only", "or", "other", "ought", "our", "ours", 
     "ourselves", "out", "over", "own", "same", 
     "shan't", "she", "she'd", "she'll", "she's", 
     "should", "shouldn't", "so", "some", "such", 
     "than", "that", "that's", "the", "their",
     "theirs", "them", "themselves", "then", "there",
     "there's", "these", "they", "they'd", "they'll",
     "they're", "they've", "this", "those", "through",
     "to", "too", "under", "until", "up", "very",
     "was", "wasn't", "we", "we'd", "we'll", "we're",
     "we've", "were", "weren't", "what", "what's",
     "when", "when's", "where", "where's", "which", 
     "while", "who", "who's", "whom", "why", "why's",
     "with", "won't", "would", "wouldn't", "you",
     "you'd", "you'll", "you're", "you've", "your",
     "yours", "yourself", "yourselves"].iter()
             .map(|s| s.to_string()).collect()
}

fn min(i : usize, j : usize) -> usize {
    if i < j { i } else { j }
}

fn context(tokens : &Vec<String>, i : usize, j : usize) -> String {
    if i >= 8 {
        tokens[(i-8)..min(tokens.len(),j+8)].join(" ")
    } else {
        tokens[0..min(tokens.len(), j+8)].join(" ")
    }
}

fn file_exists(path : &str) -> bool {
    Path::new(path).exists()
}

struct Config {
    pub terms_file : String,
    pub stopwords_file : Option<String>,
    pub unigrams_file : String,
    pub bigrams_file : String,
    pub corpus_file : String,
    pub wordnet_file : String,
    pub colloq_wordnet_file : String,
    pub cwn_data_file : String,
    pub out_file : String,
    pub freq_min : f64,
    pub field_no : usize
}

impl Config {
    fn new(matches : &ArgMatches) -> Result<Config,&'static str> {
        let terms = matches.value_of("terms").unwrap_or("terms.txt");
        let stopwords = matches.value_of("stopwords");
        let unigram = matches.value_of("unigrams").unwrap_or("count_1w.txt");
        let bigram = matches.value_of("bigrams").unwrap_or("count_2w.txt");
        let corpus = matches.value_of("corpus").unwrap_or("twitter.txt.gz");
        let wordnet = matches.value_of("wordnet").unwrap_or("wn31.xml");
        let colloq_wordnet = matches.value_of("colloq_wordnet").unwrap_or("colloqwn-1.0.xml");
        let cwn_data = matches.value_of("cwn_data").unwrap_or("data.csv.gz");
        let out = matches.value_of("out").unwrap_or("queue.csv.gz");
        let freq_min = matches.value_of("freq_min").unwrap_or("5")
            .parse::<f64>().map_err(|_| "Frequency must be a number")?;
        let field_no = matches.value_of("field_no").unwrap_or("2")
            .parse::<usize>().map_err(|_| "Field number must be a number")?;

        if file_exists(&terms) {
            if file_exists(&unigram) {
                if file_exists(&bigram) {
                    if file_exists(&corpus) {
                        if file_exists(&wordnet) {
                            if file_exists(&colloq_wordnet) {
                                if file_exists(&cwn_data) {
                                    if stopwords.is_none() || file_exists(stopwords.unwrap()) {
                                        Ok(Config {
                                            terms_file: terms.to_string(),
                                            stopwords_file: stopwords.map(|x| x.to_string()),
                                            unigrams_file: unigram.to_string(),
                                            bigrams_file: bigram.to_string(),
                                            corpus_file: corpus.to_string(),
                                            wordnet_file: wordnet.to_string(),
                                            colloq_wordnet_file: colloq_wordnet.to_string(),
                                            cwn_data_file: cwn_data.to_string(),
                                            out_file: out.to_string(),
                                            freq_min: freq_min,
                                            field_no: field_no
                                        })
                                    } else {
                                        Err("Stopwords file does not exist")
                                    }
                                } else {
                                    Err("CWN Data file does not exist")
                                }
                            } else {
                                Err("Colloquial Wordnet file does not exist")
                            }
                        } else {
                            Err("Wordnet file does not exist")
                        }
                    } else {
                        Err("Corpus file does not exist")
                    }
                } else {
                    Err("Bigram file does not exist")
                }
            } else {
                Err("Unigram file does not exist")
            }
        } else {
            Err("Terms file does not exist")
        }
    }
}

fn main() {
    let mut app = App::new("frequency")
        .version("0.1")
        .author("John P. McCrae <john@mccr.ae>")
        .about("Calculate most frequent words compared to baseline")
        .arg(Arg::with_name("terms")
             .short("t")
             .long("terms")
             .value_name("terms.txt")
             .help("The list of terms to accept (e.g., from HarvestUrbanDic)")
             .takes_value(true))
        .arg(Arg::with_name("freq_min")
             .short("f")
             .long("freq")
             .help("The minimum frequency to output")
             .takes_value(true))
        .arg(Arg::with_name("stopwords")
             .long("stopwords")
             .help("The stopword list file (optional)")
             .takes_value(true))
        .arg(Arg::with_name("unigrams")
             .long("unigrams")
             .help("The unigram file e.g., from http://norvig.com/ngrams/count_1w.txt")
             .takes_value(true))
        .arg(Arg::with_name("bigrams")
             .long("bigrams")
             .help("The bigram file e.g., from http://norvig.com/ngrams/count_2w.txt")
             .takes_value(true))
        .arg(Arg::with_name("corpus")
             .short("c")
             .long("corpus")
             .help("The corpus file")
             .takes_value(true))
        .arg(Arg::with_name("cwn_data")
             .short("d")
             .long("cwn-data")
             .help("The CWN data file")
             .takes_value(true))
        .arg(Arg::with_name("field_no")
             .short("n")
             .long("field-no")
             .help("Assume the tweet is in the nth column of the CSV")
             .takes_value(true))
        .arg(Arg::with_name("wordnet")
             .long("wordnet")
             .help("The wordnet file, e.g., http://john.mccr.ae/wn31.xml")
             .takes_value(true))
        .arg(Arg::with_name("colloq_wordnet")
             .long("cwn")
             .help("The colloquial wordnet file")
             .takes_value(true));
    let matches = app.clone().get_matches();
    match Config::new(&matches) {
        Ok(config) => 
            match frequency(config) {
                Ok(()) => {},
                Err(msg) => {
                    eprintln!("{}", msg);
                    exit(-1)
                }
            },
        Err(msg) => {
            println!("Failed: {}",msg);
            app.print_help().expect("Could not print help");
            println!("");
            exit(-1)
        }
    }
}

fn file_to_set(file : &str) -> Result<HashSet<String>,String> {
    let words_res : Result<HashSet<String>,std::io::Error> = BufReader::new(
        File::open(file)
        .map_err(|e| format!("Could not read words: {}", e))?)
        .lines().map(|_line| {
            match _line {
                Ok(line) => {
                    if line.starts_with("* ") {
                        Ok(line[2..].to_lowercase())
                    } else {
                        Ok(line.to_lowercase())
                    }
                },
                Err(e) => Err(e)
            }
        }).collect();
    words_res.map_err(|e| format!("Could not read words: {}", e))
}

fn load_counts(file : &str) -> Result<HashMap<String, f64>,String> {
    let words_res : Result<HashMap<String,f64>,String> = BufReader::new(
        File::open(file)
        .map_err(|e| format!("Could not read words: {}", e))?)
        .lines().map(|_line| {
            match _line {
                Ok(line) => {
                    let s : Vec<&str> = line.split("\t").collect();
                    match s.get(0) {
                        Some(word) => {
                            match s.get(1) {
                                Some(f) => {
                                    let score = try!(f.parse::<f64>()
                                                     .map_err(|e| format!("Cannot parse frequency: {}", e)));
                                    Ok((word.to_string(), score))
                                },
                                None => Err(format!("Bad line: {}", line))
                                }
                        },
                        None => Err(format!("Bad line: {}", line))
                    }
                },
                Err(e) => Err(format!("Could not read line: {}", e))
            }
        }).collect();
    words_res.map_err(|e| format!("Could not read words: {}", e))
}

fn load_cwn_data(file : &str) -> Result<HashSet<String>,String> {
    let file = File::open(file)
        .map_err(|e| format!("Could not open CWN data file: {}", e))?;
    let reader = BufReader::new(GzDecoder::new(file)
        .map_err(|e| format!("Could not open CWN data file: {}", e))?);
    let mut data = HashSet::new();
    for _line in reader.lines() {
        let line = _line.map_err(|e| format!("Could not read CWN file: {}", e))?;
        let mut elems = line.split("|||");
        let term = elems.nth(2).ok_or(format!("Bad line in CWN file (1): {}", line))?;
        let is_pwn = elems.nth(2).ok_or(format!("Bad line in CWN file (2): {}", line))?;
        if is_pwn == "0" {
            data.insert(term.to_lowercase());
        } else if is_pwn != "1" {
            return Err(format!("Bad line in CWN file (3): {}", line))
        } 
    }
    Ok(data)
}

fn get_child<'a>(element : &'a Element, name : &str) -> Vec<&'a Element> {
    match element.members {
        Content::Members(ref map) =>
            match map.get(name) {
                Some(e) => e.iter().collect(),
                None => Vec::new()
            },
        Content::Text(_) => Vec::new(),
        Content::Nothing => Vec::new()
    }
}

fn load_wordnet_words(wordnet_file : &str) -> Result<HashSet<String>,String> {
    let mut reader = BufReader::new(File::open(wordnet_file)
        .map_err(|e| format!("Could not read wordnet: {}", e))?);
    let mut string = String::new();
    reader.read_to_string(&mut string)
        .map_err(|e| format!("Could not read wordnet: {}", e))?;
    let wordnet : Element = serde_xml::from_str(&string)
        .map_err(|e| format!("Could not parse wordnet: {}", e))?;
    let mut values = HashSet::new();
    for lexicon in get_child(&wordnet, "Lexicon") {
        for entry in get_child(&lexicon, "LexicalEntry") {
            for lemma in get_child(&entry, "Lemma") {
                match lemma.attributes.get("writtenForm") {
                    Some(v) => values.extend(v.iter().map(|c| c.clone())),
                    None => {}
                }
            }
        }
    }
    Ok(values)

}

enum FalsePositive {
    InWordNet,
    Inflection,
    TruePositive
}

use FalsePositive::*;

static BAD_PREFIXES : [&'static str;22] = ["i ", "you ", "we ", "he ", "she ",
    "they ", "it ", "my ", "your ", "our ", "his ", "her ", "their ", "its ",
    "a ", "all ", "not ", "some ", "that ", "the ", "those ", "to "];
static BAD_SUFFIXES : [&'static str;9] = ["ing", "s", " that", " me", 
    " you", " him", " her", " it", " them"];

fn false_postive(word : &str, wordnet_words : &HashSet<String>,
                 stopwords : &HashSet<String>) -> FalsePositive {
    if wordnet_words.contains(word) || stopwords.contains(word) {
        InWordNet
    } else {
        for bp in BAD_PREFIXES.iter() {
            if word.starts_with(bp) &&
                wordnet_words.contains(&word[(bp.len())..]) {
                return Inflection;
            }
        }
        for bs in BAD_SUFFIXES.iter() {
            if word.ends_with(bs) &&
                wordnet_words.contains(&word[0..(word.len()-bs.len())]) {
                return Inflection;
            }
        }
        TruePositive
    }
}

fn corpus_file(file : &str) -> Result<Box<Iterator<Item=Result<String,std::io::Error>>>,std::io::Error> { 
    if file.ends_with(".gz") {
        Ok(Box::new(BufReader::new(GzDecoder::new(File::open(file)?)?).lines()))
    } else {
        Ok(Box::new(BufReader::new(File::open(file)?).lines()))
    }
}

fn clean_line(line : &str) -> String {
    lazy_static! {
        static ref USER : Regex = Regex::new("@\\S+").expect("Bad Regex");
    }
    USER.replace_all(line, "").to_string()
}

fn get_examples(mut contexts : Vec<String>, top_words : &HashSet<String>) -> Vec<String> {
    contexts.truncate(200); // There will be good examples in the top 200, right?
    contexts.sort_by(|x,y| {
        let sx = gdex::gdex(x, top_words);
        let sy = gdex::gdex(y, top_words);
        if sx < sy {
            Ordering::Greater
        } else if sx > sy {
            Ordering::Less
        } else {
            Ordering::Equal
        }
    });
    let mut examples = Vec::new();
    for c in contexts {
        if examples.len() < 10 && is_distinct(&c, &examples) {
            examples.push(c);
        }
    }
    examples
}

fn is_distinct(c: &str, examples : &Vec<String>) -> bool {
    let c_words : HashSet<&str> = c.split(" ").collect();
    for s in examples {
        let example_words = s.split(" ").collect();
        if c_words.intersection(&example_words).count() > 5 {
            return false;
        }
    }
    return true;
}

fn frequency(config : Config) -> Result<(),String> {
    eprintln!("Loading terms");
    let words = file_to_set(&config.terms_file)?;
    eprintln!("Loaded {} terms", words.len());
    eprintln!("Loading stopwords");
    let stopwords = match config.stopwords_file {
        Some(stopwords_files) => file_to_set(&stopwords_files)?,
        None => english_stopwords()
    };
    eprintln!("Loaded {} stopwords", stopwords.len());

    eprintln!("Loading WordNet");
    let wordnet_words = load_wordnet_words(&config.wordnet_file)?;
    eprintln!("Loaded {} WordNet words", wordnet_words.len());

    eprintln!("Loading Colloquial WordNet");
    let colloq_wordnet_words = load_wordnet_words(&config.colloq_wordnet_file)?;
    eprintln!("Loaded {} WordNet words", colloq_wordnet_words.len());

    eprintln!("Loading Unigrams");
    let unigram_counts = load_counts(&config.unigrams_file)?;
    eprintln!("Loaded {} unigrams", unigram_counts.len());

    eprintln!("Loading Bigrams");
    let bigram_counts= load_counts(&config.bigrams_file)?;
    eprintln!("Loaded {} bigrams", bigram_counts.len());

    eprintln!("Loading CWN data");
    let cwn_words = load_cwn_data(&config.cwn_data_file)?;
    eprintln!("Loaded {} words", cwn_words.len());

    let mut out = BufWriter::new(
        GzEncoder::new(File::create(&config.out_file)
                             .map_err(|e| format!("Cannot open out for writing: {}", e))?, flate2::Compression::Default));

    let mut freqs : HashMap<String, f64> = HashMap::new();
    let mut contexts : HashMap<String, Vec<String>> = HashMap::new();

    let base_freq = 1e6f64;

    let mut line_no = 0;
//    eprint!("Scanning corpus");
//    let n_u64 : u64 = corpus_file(&config.corpus_file)
//        .map_err(|e| format!("Could not load corpus: {}", e))?.map(|_line| {
//        line_no += 1;
//        if line_no % 10000 == 0 {
//            eprint!(".");
//        }
//        match _line {
//            Ok(line) => tokenize(&line).len() as u64,
//            Err(_) => 0
//        }
//        
//    }).sum();
//    eprintln!("");
//    let n = n_u64 as f64; 
    let mut n_u64 : u64 = 0;
    let n2 : f64 = unigram_counts.values().sum();

    eprint!("Processing corpus");
    for _line in corpus_file(&config.corpus_file)
            .map_err(|e| format!("Could not read corpus: {}", e))? {
        let line = _line.map_err(|e| format!("Could not read line: {}", e))?;
        line_no += 1;
        if line_no % 10000 == 0 {
            eprint!(".");
        }

        let tweet = line.split(",").skip(config.field_no).fold::<Option<String>,_>(None,|x,y| {
            match x {
                Some(x) => Some(x + "," + y),
                None => Some(y.to_string())
            }
        }).unwrap_or("".to_string());
        let tokens = tokenize(&clean_line(&tweet));
        n_u64 += tokens.len() as u64;
        let mut contexted = HashSet::new();
        if tokens.len() > 0 {
            for i in 0..(tokens.len()-1) {
                for j in (i+1)..min(i+3,tokens.len()) {
                    let phrase = tokens[i..j].join(" ");
                    if words.contains(&phrase) {
                        let zero = 0.0f64;
                        let f = *freqs.get(&phrase).unwrap_or(&zero);
                        let u = if (j - i) == 1 {
                            unigram_counts.get(&phrase).unwrap_or(&base_freq) / n2 
                        } else if (j - i) == 2 {
                            bigram_counts.get(&phrase).unwrap_or(&base_freq) / n2 
                        } else {
                            bigram_counts.get(&tokens[i..(i+2)].join(" "))
                                .unwrap_or(&base_freq) / n2
                        };
                        freqs.insert(phrase.clone(), f + 1.0 / u);
                        if !contexted.contains(&phrase) {

                            contexts.entry(phrase.clone()).or_insert(Vec::new())
                                .push(context(&tokens, i, j));
                            contexted.insert(phrase);
                        }
                    }
                }
            }
        }
    }
    eprintln!("");

    let top_words = gdex::top_n(&unigram_counts, 17000);

    let n = n_u64 as f64;

    eprint!("Writing results [  0%]");
    let n_freqs = freqs.len();
    let mut freq_n : usize = 0;
    for (phrase, freq) in freqs.iter() {
        freq_n += 1;
        if freq_n % 1000 == 0 {
           eprint!("\rWriting results [{:3}%]", (((freq_n * 100) as f32) / (n_freqs as f32)) as usize);
        }
        if *freq > config.freq_min && !cwn_words.contains(phrase) {
            match contexts.get(phrase) {
                Some(c) => {
                    match false_postive(phrase, &wordnet_words, &stopwords) {
                        TruePositive =>  {
                            if !colloq_wordnet_words.contains(phrase) {
                                write!(out,"{}|||{}", freq / n, phrase)
                                    .map_err(|e| format!("Error writing: {}", e))?;
                                let mut first = true;
                                for content in get_examples(c.clone(), &top_words) {
                                    if first {
                                        write!(out,"|||{}", content).map_err(|e| format!("Error writing: {}", e))?;
                                        first = false;
                                    } else {
                                        write!(out,";;;{}", content).map_err(|e| format!("Error writing: {}", e))?;
                                    }
                                }
                                writeln!(out,"").map_err(|e| format!("Error writing: {}", e))?;
                            }
                        },
                        InWordNet => {
                            write!(out,"{}|||*{}", freq / n, phrase)
                                .map_err(|e| format!("Error writing: {}", e))?;
                            let mut first = true;
                            for content in get_examples(c.clone(), &top_words) {
                                if first {
                                    write!(out,"|||{}", content).map_err(|e| format!("Error writing: {}", e))?;
                                    first = false;
                                } else {
                                    write!(out,";;;{}", content).map_err(|e| format!("Error writing: {}", e))?;
                                }
                            }
                            writeln!(out,"").map_err(|e| format!("Error writing: {}", e))?;
                        },
                        Inflection => {}
                    }
                },
                None => {}
            }
        }
    }
    eprintln!("");
    Ok(())
}
