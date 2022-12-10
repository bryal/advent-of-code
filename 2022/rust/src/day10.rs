#![allow(dead_code)]

pub fn part1() {
    let inp = std::fs::read_to_string("../inputs/day10.txt").unwrap();
    //let inp = std::fs::read_to_string("example").unwrap();
    let mut x = 1;
    let mut cycle = 1;
    let mut strength_sum = 0;
    let mut cycles = |n, x| {
        for _ in 0..n {
            if (cycle + 20) % 40 == 0 {
                println!("cycle: {}, x: {}, strength: {}", cycle, x, cycle * x);
                strength_sum += cycle * x;
            }
            cycle += 1;
        }
    };
    for mut words in inp.lines().map(str::split_whitespace) {
        match words.next().unwrap() {
            "addx" => {
                cycles(2, x);
                x += words.next().unwrap().parse::<i64>().unwrap();
            }
            _noop => cycles(1, x),
        }
    }
    println!("Part 1: {}", strength_sum);
}
