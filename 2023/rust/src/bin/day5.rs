use aoc2023::*;

fn main() {
    // println!("== test ==");
    // part1(TEST);
    // part2(TEST);
    // println!("== real ==");
    let inp = day_input(5);
    // part1(&inp);
    part2(&inp);
}

fn part1(inp: &str) {
    let (seeds, inp) = inp.split_once("\n\n").unwrap();
    let seeds = seeds[7..].split_ascii_whitespace().map(|s| s.parse().unwrap()).collect::<Vec<u64>>();
    let maps: Vec<Vec<(Range<u64>, u64)>> = inp
        .split("\n\n")
        .map(|section| {
            section
                .lines()
                .skip(1)
                .map(|l| {
                    let mut ns = l.split_ascii_whitespace().map(|s| s.parse::<u64>().unwrap());
                    let dst0 = ns.next().unwrap();
                    let src0 = ns.next().unwrap();
                    let len = ns.next().unwrap();
                    (src0..(src0 + len), dst0)
                })
                .collect()
        })
        .collect();
    let mut xs = seeds;
    for map in maps {
        for x in &mut xs {
            for (src, dst) in &map {
                if src.contains(x) {
                    *x = *x - src.start + dst;
                    break;
                }
            }
        }
    }
    let locs = xs;
    println!("part 1: {}", locs.iter().min().unwrap());
}

fn part2(inp: &str) {
    let (seed_ranges, inp) = inp.split_once("\n\n").unwrap();
    let seed_ranges = seed_ranges[7..].split_ascii_whitespace().map(|s| s.parse().unwrap()).collect::<Vec<u64>>();
    let seed_ranges: Vec<Range<u64>> = seed_ranges.chunks(2).map(|r| r[0]..(r[0] + r[1])).collect();
    let maps: Vec<Vec<(Range<u64>, u64)>> = inp
        .split("\n\n")
        .map(|section| {
            section
                .lines()
                .skip(1)
                .map(|l| {
                    let mut ns = l.split_ascii_whitespace().map(|s| s.parse::<u64>().unwrap());
                    let dst0 = ns.next().unwrap();
                    let src0 = ns.next().unwrap();
                    let len = ns.next().unwrap();
                    (src0..(src0 + len), dst0)
                })
                .collect()
        })
        .collect();
    let loc_ranges = maps.iter().fold(seed_ranges, |mut x_ranges, map| {
        let mut y_ranges = Vec::with_capacity(x_ranges.len());
        'outer: while let Some(x_range) = x_ranges.pop() {
            for (src, dst) in map {
                let start = src.start.max(x_range.start);
                let end = src.end.min(x_range.end);
                if start >= end {
                    continue;
                }
                y_ranges.push((start - src.start + dst)..(end - src.start + dst));
                if x_range.start < start {
                    x_ranges.push(x_range.start..start)
                }
                if end < x_range.end {
                    x_ranges.push(end..x_range.end)
                }
                continue 'outer;
            }
            y_ranges.push(x_range);
        }
        y_ranges
    });
    let min_loc = loc_ranges.iter().map(|range| range.start).min().unwrap();
    println!("part 2: {}", min_loc);
}

const TEST: &str = r#"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"#;
