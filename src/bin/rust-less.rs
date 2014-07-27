/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#![feature(macro_rules, globs, phase)]

#[phase(plugin, link)] extern crate log;

extern crate serialize;
extern crate encoding;
extern crate cssparser;

use std::io;
use std::io::{File, Writer};

use cssparser::*;
use cssparser::ast::*;

macro_rules! try_write(
    ($buf:expr, $($args:tt)*) => (match write!($buf, $($args)*) { Ok(e) => e, Err(_e) => return Err(()) })
)

fn print_css(mut rules: Vec<Result<Rule, SyntaxError>>) -> Result<String, ()> {
    let mut buffer = io::MemWriter::new();

    fn print_prelude(buffer: &mut io::MemWriter, 
                     prelude: &mut Vec<ComponentValue>) -> Result<(), ()> {
        for p in prelude.mut_iter() {
            try_write!(buffer, "{}", p.to_css());
        }
        Ok(())
    }

    fn print_block(buffer: &mut io::MemWriter, 
                   block: &mut Vec<Node>) -> Result<(), ()> {
        try_write!(buffer, "{{");
        for &(ref mut b, _) in block.mut_iter() {
            try_write!(buffer, "{}", b.to_css());
        }
        try_write!(buffer, "}}");

        Ok(())
    }

    for r in rules.mut_iter() {
        match *r {
            Ok(ref mut rule) => {
                match *rule {
                    QualifiedRule(ref mut q) => {
                        try!(print_prelude(&mut buffer, &mut q.prelude));
                        try!(print_block(&mut buffer, &mut q.block));
                    },
                    AtRule(ref mut a) => {
                        //try_write!(buffer, "{}\n", a);
                        try_write!(buffer, "@{}", a.name);
                        try!(print_prelude(&mut buffer, &mut a.prelude));
                        match a.block {
                            None => (),
                            Some(ref mut b) => try!(print_block(&mut buffer, b))
                        }
                    },
                    NonRule(ref mut component_value) => {
                        try_write!(buffer, "{}", component_value.to_css());
                    }
                }
            },
            Err(e) => {
                try_write!(buffer, "Err: {}", e);
            }
        };
    }

    String::from_utf8(buffer.unwrap())
        .map_err(|_| ())
}

fn print_json(rules: &Vec<Result<Rule, SyntaxError>>) {
    use serialize::json::ToJson;
    debug!("{}", rules.to_json());
}

fn main() {
    let contents = File::open(&Path::new("test/less/variables.less")).read_to_end();

    let contents = match contents {
        Err(e) => fail!(e),
        Ok(c) => c
    };

    let (css_unicode, _encoding) = decode_stylesheet_bytes(contents.as_slice(), None, None);
    
    let tokens = tokenize(css_unicode.as_slice());
    let mut rules = parse_stylesheet_rules(tokens);

    let rules = rules.collect::<Vec<Result<Rule, SyntaxError>>>();
    print_json(&rules);

    match print_css(rules) {
        Ok(res) => println!("{}", res),
        Err(e) => println!("Err: {}", e)
    }
}
