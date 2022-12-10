#![allow(dead_code)]

pub fn part1() {
    let total: u64 = parse()
        .1
        .into_iter()
        .filter(|&(_, size)| size <= 100000)
        .map(|(_, size)| size)
        .sum();
    println!("Part 1: {}", total)
}

pub fn part2() {
    let (root_size, dirs) = parse();
    let disk_size = 70000000;
    let unused = disk_size - root_size;
    let needed = 30000000 - unused;
    let mut dirs = dirs.into_iter().map(|(_, size)| size).collect::<Vec<_>>();
    dirs.sort();
    let i = match dirs.binary_search(&needed) {
        Ok(i) => i,
        Err(i) => i,
    };
    println!("Part 2: {}", dirs[i])
}

fn parse() -> (u64, Vec<(String, u64)>) {
    let inp = std::fs::read_to_string("../inputs/day7.txt").unwrap();
    let mut lines = inp.lines();
    lines.next();
    let mut dirs = Vec::new();
    let mut path = vec![];
    let mut sizes = vec![];
    let mut size = 0;
    let mut pop_scope = |size: &mut u64, sizes: &mut Vec<u64>, path: &mut Vec<&str>| {
        let inner_size = *size;
        *size = sizes.pop().unwrap();
        dirs.push((format!("/{}", path.join("/")), inner_size));
        path.pop();
        *size += inner_size;
    };
    for line in lines {
        match line.split_whitespace().collect::<Vec<_>>().as_slice() {
            ["$", "cd", ".."] => pop_scope(&mut size, &mut sizes, &mut path),
            ["$", "cd", name] => {
                path.push(name);
                sizes.push(size);
                size = 0;
            }
            ["$", "ls"] => (),
            ["dir", _] => (),
            [s, _name] => {
                size += s.parse::<u64>().unwrap_or(0);
            }
            _ => (),
        }
    }
    while !path.is_empty() {
        pop_scope(&mut size, &mut sizes, &mut path)
    }
    (size, dirs)
}
