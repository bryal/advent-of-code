#![allow(dead_code)]

pub fn part1() {
    let inp = std::fs::read_to_string("../inputs/day4.txt").unwrap();
    let parse_assignment = |s: &str| {
        let (l, r) = s.split_once('-').unwrap();
        (l.parse::<u32>().unwrap(), r.parse::<u32>().unwrap())
    };
    let parse_pair = |s: &str| {
        let (l, r) = s.split_once(',').unwrap();
        (parse_assignment(l), parse_assignment(r))
    };
    let contained_in = |(a1, a2), (b1, b2)| a1 >= b1 && a2 <= b2;
    let n_contained = inp
        .lines()
        .map(parse_pair)
        .filter(|&(l, r)| contained_in(l, r) || contained_in(r, l))
        .count();
    println!("part 1: {}", n_contained)
}

pub fn part2() {
    let inp = std::fs::read_to_string("../inputs/day4.txt").unwrap();
    let parse_assignment = |s: &str| {
        let (l, r) = s.split_once('-').unwrap();
        (l.parse::<u32>().unwrap(), r.parse::<u32>().unwrap())
    };
    let parse_pair = |s: &str| {
        let (l, r) = s.split_once(',').unwrap();
        (parse_assignment(l), parse_assignment(r))
    };
    let overlaps = |(a1, a2), (b1, b2)| a1 <= b2 && a2 >= b1;
    let n_contained = inp
        .lines()
        .map(parse_pair)
        .filter(|&(a, b)| overlaps(a, b))
        .count();
    println!("part 2: {}", n_contained)
}
