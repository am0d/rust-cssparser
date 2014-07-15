/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#![feature(macro_rules, globs)]

extern crate serialize;
extern crate encoding;
extern crate cssparser;

use std::{io, task};
use std::io::{File, Command, Writer, TempDir};
use serialize::{json};
use serialize::json::ToJson;

use encoding::label::encoding_from_whatwg_label;

use cssparser::*;
use cssparser::ast::*;

fn write_whole_file(path: &Path, data: &str) {
    match File::open_mode(path, io::Open, io::Write) {
        Ok(mut writer) => { writer.write(data.as_bytes()).unwrap(); },
        _ => fail!("could not open file"),
    }
}


fn almost_equals(a: &json::Json, b: &json::Json) -> bool {
    match (a, b) {
        (&json::Number(a), &json::Number(b)) => (a - b).abs() < 1e-6,
        (&json::String(ref a), &json::String(ref b)) => a == b,
        (&json::Boolean(a), &json::Boolean(b)) => a == b,
        (&json::List(ref a), &json::List(ref b))
            => a.iter().zip(b.iter()).all(|(ref a, ref b)| almost_equals(*a, *b)),
        (&json::Object(_), &json::Object(_)) => fail!("Not implemented"),
        (&json::Null, &json::Null) => true,
        _ => false,
    }
}


fn assert_json_eq(results: json::Json, expected: json::Json, message: String) {
    if !almost_equals(&results, &expected) {
        let temp = TempDir::new("rust-cssparser-tests").unwrap();
        let results = results.to_pretty_str().append("\n");
        let expected = expected.to_pretty_str().append("\n");
        task::try(proc() {
            let mut result_path = temp.path().clone();
            result_path.push("results.json");
            let mut expected_path = temp.path().clone();
            expected_path.push("expected.json");
            write_whole_file(&result_path, results.as_slice());
            write_whole_file(&expected_path, expected.as_slice());
            Command::new("colordiff")
                .arg("-u1000")
                .arg(result_path)
                .arg(expected_path)
                .status().unwrap_or_else(|e| fail!("Failed to get status of colordiff: {}", e));
        }).unwrap_or_else(|_e| fail!("Failed to execute task"));

        fail!(message)
    }
}


fn run_raw_json_tests(json_data: &str, run: |json::Json, json::Json|) {
    let items = match json::from_str(json_data) {
        Ok(json::List(items)) => items,
        _ => fail!("Invalid JSON")
    };
    assert!(items.len() % 2 == 0);
    let mut input = None;
    for item in items.move_iter() {
        match (&input, item) {
            (&None, json_obj) => input = Some(json_obj),
            (&Some(_), expected) => {
                let input = input.take_unwrap();
                run(input, expected)
            },
        };
    }
}


fn run_json_tests<T: ToJson>(json_data: &str, parse: |input: &str| -> T) {
    run_raw_json_tests(json_data, |input, expected| {
        match input {
            json::String(input) => {
                let result = parse(input.as_slice()).to_json();
                assert_json_eq(result, expected, input);
            },
            _ => fail!("Unexpected JSON")
        }
    });
}



#[test]
fn stylesheet() {
    /*run_json_tests(include_str!("../css-parsing-tests/stylesheet.json"), |input| {
        parse_stylesheet_rules(tokenize(input)).collect::<Vec<Result<Rule, SyntaxError>>>()
    });*/
}

#[test]
fn stylesheet_from_bytes() {
    /*run_raw_json_tests(include_str!("../css-parsing-tests/stylesheet_bytes.json"),
    |input, expected| {
        let map = match input {
            json::Object(map) => map,
            _ => fail!("Unexpected JSON")
        };

        let result = {
            let css = get_string(&map, &"css_bytes".to_string()).unwrap().chars().map(|c| {
                assert!(c as u32 <= 0xFF);
                c as u8
            }).collect::<Vec<u8>>();
            let protocol_encoding_label = get_string(&map, &"protocol_encoding".to_string());
            let environment_encoding = get_string(&map, &"environment_encoding".to_string())
                .and_then(encoding_from_whatwg_label);

            let (mut rules, used_encoding) = parse_stylesheet_rules_from_bytes(
                css.as_slice(), protocol_encoding_label, environment_encoding);

            (rules.collect::<Vec<Result<Rule, SyntaxError>>>(), used_encoding.name().to_string()).to_json()
        };
        assert_json_eq(result, expected, json::Object(map).to_pretty_str());
    });

    fn get_string<'a>(map: &'a json::Object, key: &String) -> Option<&'a str> {
        match map.find(key) {
            Some(&json::String(ref s)) => Some(s.as_slice()),
            Some(&json::Null) => None,
            None => None,
            _ => fail!("Unexpected JSON"),
        }
    }*/
}


fn run_color_tests(json_data: &str, to_json: |result: Option<Color>| -> json::Json) {
    run_json_tests(json_data, |input| {
        match parse_one_component_value(tokenize(input)) {
            Ok(component_value) => to_json(Color::parse(&component_value)),
            Err(_reason) => json::Null,
        }
    });
}


fn main() {
    let contents = File::open(&Path::new("test/less/css.less")).read_to_end();

    let contents = match contents {
        Err(e) => fail!(e),
        Ok(c) => c
    };

    println!("{}", contents);

    let (mut rules, used_encoding) = cssparser::parse_stylesheet_rules_from_bytes(
        contents.as_slice(), None, None);

    println!("{}", rules.collect::<Vec<Result<Rule, SyntaxError>>>().to_json());
}
