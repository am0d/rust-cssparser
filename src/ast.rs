/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::str;
use std::fmt;
use std::slice;
use std::vec;
use serialize::{json};
use serialize::json::ToJson;


#[deriving(PartialEq)]
pub struct NumericValue {
    pub representation: String,
    pub value: f64,
    pub int_value: Option<i64>,
}


#[deriving(PartialEq)]
pub struct SourceLocation {
    pub line: uint,  // First line is 1
    pub column: uint,  // First character of a line is at column 1
}


pub type Node = (ComponentValue, SourceLocation);  // TODO this is not a good name


#[deriving(PartialEq)]
pub enum ComponentValue {
    // Preserved tokens.
    Ident(String),
    AtKeyword(String),
    Hash(String),
    IDHash(String),  // Hash that is a valid ID selector.
    String(String),
    URL(String),
    Delim(char),
    Number(NumericValue),
    Percentage(NumericValue),
    Dimension(NumericValue, String),
    UnicodeRange(u32, u32),  // (start, end) of range
    WhiteSpace,
    Colon,  // :
    Semicolon,  // ;
    Comma,  // ,
    IncludeMatch, // ~=
    DashMatch, // |=
    PrefixMatch, // ^=
    SuffixMatch, // $=
    SubstringMatch, // *=
    Column, // ||
    CDO,  // <!--
    CDC,  // -->

    // Function
    Function(String, Vec<ComponentValue>),  // name, arguments

    // Simple block
    ParenthesisBlock(Vec<ComponentValue>),  // (…)
    SquareBracketBlock(Vec<ComponentValue>),  // […]
    CurlyBracketBlock(Vec<Node>),  // {…}

    // These are always invalid
    BadURL,
    BadString,
    CloseParenthesis, // )
    CloseSquareBracket, // ]
    CloseCurlyBracket, // }
}


#[deriving(PartialEq)]
pub struct Declaration {
    pub location: SourceLocation,
    pub name: String,
    pub value: Vec<ComponentValue>,
    pub important: bool,
}

#[deriving(PartialEq)]
pub struct QualifiedRule {
    pub location: SourceLocation,
    pub prelude: Vec<ComponentValue>,
    pub block: Vec<Node>,
}

#[deriving(PartialEq)]
pub struct AtRule {
    pub location: SourceLocation,
    pub name: String,
    pub prelude: Vec<ComponentValue>,
    pub block: Option<Vec<Node>>,
}

#[deriving(PartialEq)]
pub enum DeclarationListItem {
    Declaration(Declaration),
    // A better idea for a name that means "at-rule" but is not "AtRule"?
    DeclAtRule(AtRule),
}

#[deriving(PartialEq)]
pub enum Rule {
    QualifiedRule(QualifiedRule),
    AtRule(AtRule),
}

#[deriving(PartialEq)]
pub struct SyntaxError {
    pub location: SourceLocation,
    pub reason: ErrorReason,
}

#[deriving(PartialEq)]
pub enum ErrorReason {
    ErrEmptyInput,  // Parsing a single "thing", found only whitespace.
    ErrExtraInput,  // Found more non-whitespace after parsing a single "thing".
    ErrMissingQualifiedRuleBlock,  // EOF in a qualified rule prelude, before '{'
    ErrInvalidDeclarationSyntax,
    ErrInvalidBangImportantSyntax,
    // This is meant to be extended
}

impl fmt::Show for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:u}:{:u} {:?}", self.location.line, self.location.column, self.reason)
    }
}


pub trait SkipWhitespaceIterable<'a> {
    fn skip_whitespace(self) -> SkipWhitespaceIterator<'a>;
}

impl<'a> SkipWhitespaceIterable<'a> for &'a [ComponentValue] {
    fn skip_whitespace(self) -> SkipWhitespaceIterator<'a> {
        SkipWhitespaceIterator{ iter_with_whitespace: self.iter() }
    }
}

#[deriving(Clone)]
pub struct SkipWhitespaceIterator<'a> {
    pub iter_with_whitespace: slice::Items<'a, ComponentValue>,
}

impl<'a> Iterator<&'a ComponentValue> for SkipWhitespaceIterator<'a> {
    fn next(&mut self) -> Option<&'a ComponentValue> {
        for component_value in self.iter_with_whitespace {
            if component_value != &WhiteSpace { return Some(component_value) }
        }
        None
    }
}


pub trait MoveSkipWhitespaceIterable {
    fn move_skip_whitespace(self) -> MoveSkipWhitespaceIterator;
}

impl MoveSkipWhitespaceIterable for Vec<ComponentValue> {
    fn move_skip_whitespace(self) -> MoveSkipWhitespaceIterator {
        MoveSkipWhitespaceIterator{ iter_with_whitespace: self.move_iter() }
    }
}

pub struct MoveSkipWhitespaceIterator {
    iter_with_whitespace: vec::MoveItems<ComponentValue>,
}

impl Iterator<ComponentValue> for MoveSkipWhitespaceIterator {
    fn next(&mut self) -> Option<ComponentValue> {
        for component_value in self.iter_with_whitespace {
            if component_value != WhiteSpace { return Some(component_value) }
        }
        None
    }
}

/* ToJson implementations for all the AST nodes
 *
 * These need to be implemented here because they can't be implemented in a
 * crate that doesn't define these nodes
 */

macro_rules! JString {
    ($e: expr) => { json::String($e.to_string()) }
}

macro_rules! JList {
    ($($e: expr),*) => { json::List(vec!( $($e),* )) }
}

fn list_to_json(list: &Vec<(ComponentValue, SourceLocation)>) -> Vec<json::Json> {
    list.iter().map(|tuple| {
        match *tuple {
            (ref c, _) => c.to_json()
        }
    }).collect()
}

impl ToJson for Result<Rule, SyntaxError> {
    fn to_json(&self) -> json::Json {
        match *self {
            Ok(ref a) => a.to_json(),
            Err(ref b) => b.to_json(),
        }
    }
}


impl ToJson for Result<DeclarationListItem, SyntaxError> {
    fn to_json(&self) -> json::Json {
        match *self {
            Ok(ref a) => a.to_json(),
            Err(ref b) => b.to_json(),
        }
    }
}


impl ToJson for Result<Declaration, SyntaxError> {
    fn to_json(&self) -> json::Json {
        match *self {
            Ok(ref a) => a.to_json(),
            Err(ref b) => b.to_json(),
        }
    }
}


impl ToJson for Result<ComponentValue, SyntaxError> {
    fn to_json(&self) -> json::Json {
        match *self {
            Ok(ref a) => a.to_json(),
            Err(ref b) => b.to_json(),
        }
    }
}


impl ToJson for SyntaxError {
    fn to_json(&self) -> json::Json {
        json::List(vec!(JString!("error"), JString!(match self.reason {
            ErrEmptyInput => "empty",
            ErrExtraInput => "extra-input",
            _ => "invalid",
        })))
    }
}


impl ToJson for super::Color {
    fn to_json(&self) -> json::Json {
        use super::{RGBA, CurrentColor};

        match *self {
            RGBA(RGBA { red: r, green: g, blue: b, alpha: a }) => vec!(r, g, b, a).to_json(),
            CurrentColor => JString!("currentColor"),
        }
    }
}


impl ToJson for Rule {
    fn to_json(&self) -> json::Json {
        match *self {
            QualifiedRule(ref rule) => rule.to_json(),
            AtRule(ref rule) => rule.to_json(),
        }
    }
}


impl ToJson for DeclarationListItem {
    fn to_json(&self) -> json::Json {
        match *self {
            Declaration(ref declaration) => declaration.to_json(),
            DeclAtRule(ref at_rule) => at_rule.to_json(),
        }
    }
}

impl ToJson for ComponentValue {
    fn to_json(&self) -> json::Json {
        fn numeric(value: &NumericValue) -> Vec<json::Json> {
            match *value {
                NumericValue{representation: ref r, value: ref v, int_value: ref i}
                => vec!(r.to_json(), v.to_json(),
                        JString!(match *i { Some(_) => "integer", _ => "number" }))
            }
        }

        match *self {
            Ident(ref value) => JList!(JString!("ident"), value.to_json()),
            AtKeyword(ref value) => JList!(JString!("at-keyword"), value.to_json()),
            Hash(ref value) => JList!(JString!("hash"), value.to_json(),
                                      JString!("unrestricted")),
            IDHash(ref value) => JList!(JString!("hash"), value.to_json(), JString!("id")),
            String(ref value) => JList!(JString!("string"), value.to_json()),
            URL(ref value) => JList!(JString!("url"), value.to_json()),
            Delim('\\') => JString!("\\"),
            Delim(value) => json::String(str::from_char(value)),

            Number(ref value) => json::List(vec!(JString!("number")) + numeric(value)),
            Percentage(ref value) => json::List(vec!(JString!("percentage")) + numeric(value)),
            Dimension(ref value, ref unit)
            => json::List(vec!(JString!("dimension")) + numeric(value) + &[unit.to_json()]),

            UnicodeRange(start, end)
            => JList!(JString!("unicode-range"), start.to_json(), end.to_json()),

            WhiteSpace => JString!(" "),
            Colon => JString!(":"),
            Semicolon => JString!(";"),
            Comma => JString!(","),
            IncludeMatch => JString!("~="),
            DashMatch => JString!("|="),
            PrefixMatch => JString!("^="),
            SuffixMatch => JString!("$="),
            SubstringMatch => JString!("*="),
            Column => JString!("||"),
            CDO => JString!("<!--"),
            CDC => JString!("-->"),

            Function(ref name, ref arguments)
            => json::List(vec!(JString!("function"), name.to_json()) +
                     arguments.iter().map(|a| a.to_json()).collect::<Vec<json::Json>>()),
            ParenthesisBlock(ref content)
            => json::List(vec!(JString!("()")) + content.iter().map(|c| c.to_json()).collect::<Vec<json::Json>>()),
            SquareBracketBlock(ref content)
            => json::List(vec!(JString!("[]")) + content.iter().map(|c| c.to_json()).collect::<Vec<json::Json>>()),
            CurlyBracketBlock(ref content)
            => json::List(vec!(JString!("{}")) + list_to_json(content)),

            BadURL => JList!(JString!("error"), JString!("bad-url")),
            BadString => JList!(JString!("error"), JString!("bad-string")),
            CloseParenthesis => JList!(JString!("error"), JString!(")")),
            CloseSquareBracket => JList!(JString!("error"), JString!("]")),
            CloseCurlyBracket => JList!(JString!("error"), JString!("}")),
        }
    }
}

impl ToJson for AtRule {
    fn to_json(&self) -> json::Json {
        match *self {
            AtRule{name: ref name, prelude: ref prelude, block: ref block, ..}
            => json::List(vec!(JString!("at-rule"), name.to_json(),
                               prelude.to_json(), block.as_ref().map(list_to_json).to_json()))
        }
    }
}


impl ToJson for QualifiedRule {
    fn to_json(&self) -> json::Json {
        match *self {
            QualifiedRule{prelude: ref prelude, block: ref block, ..}
            => json::List(vec!(JString!("qualified rule"),
                               prelude.to_json(), json::List(list_to_json(block))))
        }
    }
}


impl ToJson for Declaration {
    fn to_json(&self) -> json::Json {
        match *self {
            Declaration{name: ref name, value: ref value, important: ref important, ..}
            =>  json::List(vec!(JString!("declaration"), name.to_json(),
                                value.to_json(), important.to_json()))
        }
    }
}

