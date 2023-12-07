use aoc2023::*;

static TEST: &str = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";

fn main() {
    println!("== test ==");
    part1(TEST);
    part2(TEST);
    println!("== real ==");
    let inp = day_input(4);
    part1(&inp);
    part2(&inp);
}

fn part1(inp: &str) {
    let score: u64 = inp
        .lines()
        .map(|l| {
            let (_, s) = l.split_once(": ").unwrap();
            let (s1, s2) = s.split_once(" | ").unwrap();
            let w = s1.split_ascii_whitespace().filter(|s| !s.is_empty()).collect::<HashSet<_>>();
            (1 << s2.split_ascii_whitespace().filter(|n| w.contains(n)).count()) >> 1
        })
        .sum();
    println!("part 1: {score}")
}

fn part2(inp: &str) {
    let card_n_wins: Vec<u8> = inp
        .lines()
        .map(|l| {
            let (s1, s2) = l.split_once(": ").unwrap().1.split_once(" | ").unwrap();
            let w = s1.split_ascii_whitespace().filter(|s| !s.is_empty()).collect::<HashSet<_>>();
            s2.split_ascii_whitespace().filter(|n| w.contains(n)).count() as u8
        })
        .collect();
    let mut cards = vec![1; card_n_wins.len()];
    let mut count = cards.len();
    for i in 0..cards.len() {
        let n = cards[i];
        for j in 0..(card_n_wins[i] as usize).min(cards.len()) {
            cards[i + 1 + j] += n;
            count += n;
        }
    }
    println!("part 2: {count}")
}
