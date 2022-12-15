#![allow(dead_code)]

use std::cmp::{max, min};
use std::collections::HashSet;

pub fn part1() {
    let (mut obstacles, lowest) = parse();
    let mut drop_grain = || {
        let mut x = 500;
        for y in 0..lowest {
            match [x, x - 1, x + 1]
                .iter()
                .find(|&&x2| !obstacles.contains(&Vec2 { x: x2, y: y + 1 }))
            {
                Some(&x2) => x = x2,
                None => {
                    obstacles.insert(Vec2 { x, y });
                    return true;
                }
            }
        }
        false
    };
    let mut n_grains = 0;
    while drop_grain() {
        n_grains += 1;
    }
    println!("Part 1: {}", n_grains)
}

pub fn part2() {
    let (mut obstacles, lowest) = parse();
    let floor = lowest + 2;
    let mut drop_grain = || {
        let mut x = 500;
        for y in 0..(floor - 1) {
            match [x, x - 1, x + 1]
                .iter()
                .find(|&&x2| !obstacles.contains(&Vec2 { x: x2, y: y + 1 }))
            {
                Some(&x2) => x = x2,
                None => {
                    obstacles.insert(Vec2 { x, y });
                    return y > 0;
                }
            }
        }
        obstacles.insert(Vec2 { x, y: floor - 1 });
        true
    };
    let mut n_grains = 1;
    while drop_grain() {
        n_grains += 1;
    }
    println!("Part 2: {}", n_grains)
}

fn parse() -> (HashSet<Vec2>, usize) {
    let inp = std::fs::read_to_string("../inputs/day14.txt").unwrap();
    //let inp = std::fs::read_to_string("example").unwrap();
    let mut obstacles = HashSet::<Vec2>::new();
    for s_line in inp.lines() {
        for segment in s_line.split(" -> ").map(parse_point).collect::<Vec<_>>().windows(2) {
            let (x0, x1) = (min(segment[0].x, segment[1].x), max(segment[0].x, segment[1].x));
            let (y0, y1) = (min(segment[0].y, segment[1].y), max(segment[0].y, segment[1].y));
            obstacles.extend((x0..x1 + 1).flat_map(|x| (y0..y1 + 1).map(move |y| Vec2 { x, y })));
        }
    }
    let lowest = obstacles.iter().map(|p| p.y).max().unwrap();
    (obstacles, lowest)
}

fn parse_point(s: &str) -> Vec2 {
    let mut it = s.split(',').map(|s| s.parse().unwrap());
    Vec2 { x: it.next().unwrap(), y: it.next().unwrap() }
}

#[derive(PartialEq, Eq, Hash, Debug)]
struct Vec2 {
    x: usize,
    y: usize,
}
