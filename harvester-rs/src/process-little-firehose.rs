extern crate clap;
extern crate flate2;
extern crate serde;
extern crate serde_json;
extern crate regex;
#[macro_use] extern crate lazy_static;

mod tweet_filters;
mod tokenize;

use clap::{Arg,App};
use std::io::{BufReader};
use std::io::prelude::*;
use std::fs::File;
use std::path::Path;
use std::process::exit;
use flate2::read::{GzDecoder,GzEncoder};
use flate2::Compression;
use serde_json::Value;
use tweet_filters::TweetFilter;

fn main() {
    let matches = App::new("Process Little Firehose")
        .version("0.1")
        .author("John P. McCrae <john@mccr.ae>")
        .about("Process the Little Firehose data")
        .arg(Arg::with_name("dir")
             .short("d")
             .long("dir")
             .value_name("DIR")
             .help("The directory containing the litle firehose files")
             .takes_value(true))
        .arg(Arg::with_name("prefix")
             .short("p")
             .long("prefix")
             .value_name("PREFIX")
             .help("The prefix that the file starts with")
             .takes_value(true))
        .arg(Arg::with_name("out")
             .short("o")
             .long("out")
             .value_name("OUT")
             .help("Where to write the output to")
             .takes_value(true))
        .arg(Arg::with_name("dict")
             .short("w")
             .long("dict")
             .value_name("DICT")
             .help("A list of valid words")
             .takes_value(true))
        .get_matches();

    let dir_file = matches.value_of("dir").unwrap_or(".");
    let prefix = matches.value_of("prefix").unwrap_or("littleFireHose.json.gz");
    let out_file = matches.value_of("out").unwrap_or("tweets.txt.gz");
    let dict_file = matches.value_of("dict").unwrap_or("words.txt");

    match 
        tweet_filters::setup_tweet_filters(dict_file).and_then(|filter| 
            process_little_firehose(dir_file, prefix, out_file, filter)) {
        Ok(()) => {},
        Err(msg) => {
            println!("{}", msg);
            exit(-1);
        }
    }
}

fn process_little_firehose(dir_file : &str, prefix : &str, 
                           out_file : &str, filter : TweetFilter) -> Result<(),String> {
    let out_handle = try!(File::create(out_file)
                          .map_err(|e| format!("Could not open output file: {}",
                                               e)));
    let mut out = GzEncoder::new(out_handle, Compression::Default);

    let dir = try!(Path::new(dir_file).read_dir()
                   .map_err(|e| format!("Could not read directory: {}", e)));


    for _file in dir {
        try!(_file
             .map_err(|e| format!("Could not read file: {}", e))
             .and_then(|file| {
            file.file_name().into_string()
                .map_err(|_| format!("Could not get file name"))
                .and_then(|file_name| {
                if file_name.starts_with(prefix) {
                    eprintln!("{}", file_name);
                    match process_file(&file.path(), &mut out, &filter) {
                        Ok(()) => {},
                        Err(e) => {
                            eprintln!("Could not process {} due to {}", 
                                     file_name, e);
                        }
                    }
                    Ok(())
                } else {
                    Ok(())
                }
            })
        }));
    }
    
    Ok(())
}

fn process_file<W:Write, P: AsRef<Path>>(file_name : &P, out : &mut W,
                                         filter : &TweetFilter) -> Result<(),std::io::Error> {
    let file = File::open(file_name)?;
    let decoder = GzDecoder::new(file)?;
    let reader = BufReader::new(decoder);

    for _line in reader.lines() {
        let line = _line?;
        if line.starts_with("{") {
            let data : Value = serde_json::from_str(&line)?;
            try!(data.as_object().and_then(|map| {
                map.get("lang").and_then(|lang| {
                    lang.as_str().map(|lang_str| {
                        if lang_str == "en"  {
                            let created_at = map.get("created_at")
                                .and_then(|c| c.as_str()).unwrap_or("");
                            let id = map.get("id")
                                .and_then(|i| i.as_i64()).unwrap_or(0);
                            let text = map.get("text")
                                .and_then(|t| t.as_str()).unwrap_or("");
                            if filter.filter(text) {
                                writeln!(out, "{},{},{}", created_at, id, 
                                         text.replace("\n","\\n"))
                            } else {
                                Ok(())
                            }
                        } else {
                            Ok(())
                        }
                    })
                })
            }).unwrap_or(Ok(())));
        }
    }
    Ok(())
    
}
