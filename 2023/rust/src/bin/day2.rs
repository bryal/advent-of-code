use aoc2023::*;

static TEST: &str = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";

fn main() {
    let inp = day_input(2);
    // let inp = TEST;
    part1(&inp);
    part2(&inp);
}

fn part1(inp: &str) {
    let idsum: u64 = inp
        .lines()
        .filter_map(|s| {
            let (s0, s1) = s.split_once(':').unwrap();
            let id: u64 = s0["Game ".len()..].parse().unwrap();
            s1.split(';')
                .all(|set| {
                    set.split(',').all(|s| {
                        let (n, col) = s.trim().split_once(' ').unwrap();
                        let n: u64 = n.parse().unwrap();
                        match col {
                            "red" => n <= 12,
                            "green" => n <= 13,
                            _blue => n <= 14,
                        }
                    })
                })
                .then_some(id)
        })
        .sum();
    println!("part 1: {idsum}");
}

fn part2(inp: &str) {
    let powsum: u64 = inp
        .lines()
        .map(|s| {
            let (_, s1) = s.split_once(':').unwrap();
            let (r, g, b) = s1.split(';').flat_map(|set| set.split(',')).fold((0, 0, 0), |(r, g, b), s| {
                let (n, col) = s.trim().split_once(' ').unwrap();
                let n: u64 = n.parse().unwrap();
                match col {
                    "red" => (r.max(n), g, b),
                    "green" => (r, g.max(n), b),
                    _blue => (r, g, b.max(n)),
                }
            });
            r * g * b
        })
        .sum();
    println!("part 2: {powsum}");
}
