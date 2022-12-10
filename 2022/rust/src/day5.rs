#![allow(dead_code)]

pub fn part1() {
    let inp = std::fs::read_to_string("../inputs/day5.txt").unwrap();
    let (inp_stacks, inp) = inp.split_once(" 1   2").unwrap();
    let (inp, inp_moves) = inp.split_once("\n\n").unwrap();
    let n_stacks: usize = inp.split_whitespace().last().unwrap().parse().unwrap();

    let mut stacks = vec![vec![]; n_stacks];
    for line in inp_stacks.lines().rev() {
        for i in 0..n_stacks {
            let c = line.chars().nth(i * 4 + 1).unwrap();
            if c != ' ' {
                stacks[i].push(c)
            }
        }
    }

    for line in inp_moves.lines() {
        let words: Vec<_> = line.split_whitespace().collect();
        let count: usize = words[1].parse().unwrap();
        let from = words[3].parse::<usize>().unwrap() - 1;
        let to = words[5].parse::<usize>().unwrap() - 1;
        for _ in 0..count {
            let crate_ = stacks[from].pop().unwrap();
            stacks[to].push(crate_);
        }
    }

    let result = stacks
        .iter()
        .map(|st| st.last().cloned().unwrap_or(' '))
        .collect::<String>();
    println!("part 1: {}", result);
}

pub fn part2() {
    let inp = std::fs::read_to_string("../inputs/day5.txt").unwrap();
    let (inp_stacks, inp) = inp.split_once(" 1   2").unwrap();
    let (inp, inp_moves) = inp.split_once("\n\n").unwrap();
    let n_stacks: usize = inp.split_whitespace().last().unwrap().parse().unwrap();

    let mut stacks = vec![vec![]; n_stacks];
    for line in inp_stacks.lines().rev() {
        for i in 0..n_stacks {
            let c = line.chars().nth(i * 4 + 1).unwrap();
            if c != ' ' {
                stacks[i].push(c)
            }
        }
    }

    for line in inp_moves.lines() {
        let words: Vec<_> = line.split_whitespace().collect();
        let count: usize = words[1].parse().unwrap();
        let from = words[3].parse::<usize>().unwrap() - 1;
        let to = words[5].parse::<usize>().unwrap() - 1;
        let at = stacks[from].len() - count;
        let mut crates = stacks[from].split_off(at);
        stacks[to].append(&mut crates);
    }

    let result = stacks
        .iter()
        .map(|st| st.last().cloned().unwrap_or(' '))
        .collect::<String>();
    println!("part 2: {}", result);
}
