use aoc2023::*;

fn main() {
    println!("== test ==");
    part1(TEST);
    part2(TEST);
    println!("== real ==");
    let inp = day_input(7);
    part1(&inp);
    part2(&inp);
}

fn part1(inp: &str) {
    fn val(c: u8) -> u8 {
        match c {
            b'A' => 14,
            b'K' => 13,
            b'Q' => 12,
            b'J' => 11,
            b'T' => 10,
            _ => c - b'0',
        }
    }
    fn typ(hand: [u8; 5]) -> u8 {
        let mut counts = [(0u8, 255u8); 5];
        for card in hand.into_iter() {
            let i = counts
                .iter()
                .position(|&(_, card2)| card == card2)
                .or_else(|| counts.iter().position(|&(_, card2)| card2 == 255))
                .unwrap();
            counts[i].0 += 1;
            counts[i].1 = card;
        }
        counts.sort_by(|a, b| b.cmp(a));
        match (counts[0].0, counts[1].0, counts[2].0) {
            (5, 0, 0) => 6,
            (4, 1, 0) => 5,
            (3, 2, 0) => 4,
            (3, 1, 1) => 3,
            (2, 2, 1) => 2,
            (2, 1, 1) => 1,
            _ => 0,
        }
    }
    let mut hands: Vec<(u8, [u8; 5], u16)> = inp
        .lines()
        .map(|l| {
            let (hand, bid) = l.split_once(' ').unwrap();
            let hand = std::array::from_fn(|i| val(hand.as_bytes()[i]));
            (typ(hand), hand, bid.parse().unwrap())
        })
        .collect();
    hands.sort();
    let winnings: u64 = hands.iter().enumerate().map(|(i, &(_, _, bid))| (i as u64 + 1) * bid as u64).sum();
    println!("part 1: {}", winnings);
}

fn part2(inp: &str) {
    fn val(c: u8) -> u8 {
        match c {
            b'A' => 14,
            b'K' => 13,
            b'Q' => 12,
            b'J' => 1,
            b'T' => 10,
            _ => c - b'0',
        }
    }
    fn typ(hand: [u8; 5]) -> u8 {
        let mut counts = [(0u8, 255u8); 5];
        let mut joks = 0;
        for card in hand.into_iter() {
            if card == val(b'J') {
                joks += 1;
                continue;
            }
            let i = counts
                .iter()
                .position(|&(_, card2)| card == card2)
                .or_else(|| counts.iter().position(|&(_, card2)| card2 == 255))
                .unwrap();
            counts[i].0 += 1;
            counts[i].1 = card;
        }
        counts.sort_by(|a, b| b.cmp(a));
        let counts = counts.map(|(n, _)| n);
        match (counts, joks) {
            ([_, 0, ..], _) => 6,
            ([_, 1, 0, ..], _) => 5,
            ([_, 2, 0, ..], _) => 4,
            ([_, 1, 1, 0, ..], _) => 3,
            ([2, 2, 1, ..], 0) => 2,
            ([_, 1, 1, 1, 0], _) => 1,
            ([1, 1, 1, 1, 1], 0) => 0,
            _ => panic!("?? {counts:?}, {joks}"),
        }
    }
    let mut hands: Vec<(u8, [u8; 5], u16)> = inp
        .lines()
        .map(|l| {
            let (hand, bid) = l.split_once(' ').unwrap();
            let hand = std::array::from_fn(|i| val(hand.as_bytes()[i]));
            (typ(hand), hand, bid.parse().unwrap())
        })
        .collect();
    hands.sort();
    let winnings: u64 = hands.iter().enumerate().map(|(i, &(_, _, bid))| (i as u64 + 1) * bid as u64).sum();
    println!("part 2: {}", winnings);
}

const TEST: &str = r#"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"#;
