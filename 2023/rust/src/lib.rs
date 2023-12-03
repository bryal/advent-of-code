#![feature(pattern)]

pub use std::collections::HashMap;
pub use std::iter::{once, repeat};
use std::str::pattern::Pattern;

pub fn day_input(n: u8) -> String {
    std::fs::read_to_string(&format!("../input/day{n}.txt")).unwrap()
}

pub fn left_char<'s>(s: &'s str, pat: impl Pattern<'s>) -> char {
    let i = s.find(pat).unwrap();
    s[i..].chars().next().unwrap()
}

pub fn right_char<'s, P>(s: &'s str, pat: P) -> char
where
    P: Pattern<'s>,
    <P as Pattern<'s>>::Searcher: std::str::pattern::ReverseSearcher<'s>,
{
    let i = s.rfind(pat).unwrap();
    s[i..].chars().next().unwrap()
}

pub fn dig(c: char) -> u8 {
    c as u8 - b'0'
}
