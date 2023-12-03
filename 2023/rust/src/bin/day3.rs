use aoc2023::*;

static TEST: &str = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..";

fn main() {
    let inp = day_input(3);
    // let inp = TEST;
    part1(&inp);
    part2(&inp);
}

fn part1(inp: &str) {
    let engine = inp.lines().collect::<Vec<_>>();
    let (w, h) = (engine[0].len(), engine.len());
    #[derive(Clone, Copy)]
    struct NumSpan {
        row: usize,
        col0: usize,
        len: usize,
    }
    let adjs = |span: NumSpan| {
        let left = span.col0 > 0;
        let right = span.col0 + span.len < w;
        let up = span.row > 0;
        let down = span.row + 1 < h;
        let mut adjs = vec![];
        if left {
            adjs.push((span.col0 - 1, span.row));
        }
        if left && up {
            adjs.push((span.col0 - 1, span.row - 1))
        }
        if up {
            adjs.extend((0..span.len).map(|i| (span.col0 + i, span.row - 1)))
        }
        if up && right {
            adjs.push((span.col0 + span.len, span.row - 1))
        }
        if right {
            adjs.push((span.col0 + span.len, span.row));
        }
        if right && down {
            adjs.push((span.col0 + span.len, span.row + 1))
        }
        if down {
            adjs.extend((0..span.len).map(|i| (span.col0 + i, span.row + 1)))
        }
        if down && left {
            adjs.push((span.col0 - 1, span.row + 1))
        }
        adjs
    };
    let sum: u64 = engine
        .iter()
        .enumerate()
        .flat_map(|(i, &row)| {
            row.split(|c: char| !c.is_ascii_digit())
                .filter(|&s| !s.is_empty())
                .map(move |s| NumSpan { row: i, col0: (s.as_ptr() as usize - row.as_ptr() as usize), len: s.len() })
                .filter(|&numspan| {
                    adjs(numspan)
                        .iter()
                        .any(|&(x, y)| engine[y][x..].starts_with(|c: char| c.is_ascii_punctuation() && c != '.'))
                })
                .map(|partspan| engine[partspan.row][partspan.col0..][..partspan.len].parse::<u64>().unwrap())
        })
        .sum();
    println!("part 1: {sum}");
}

fn part2(inp: &str) {
    let engine = inp;
    let w = engine.find('\n').unwrap();
    let h = engine.len() / (w + 1);
    #[derive(Clone, Copy)]
    struct NumSpan {
        row: usize,
        col0: usize,
        len: usize,
    }
    let adjs = |span: NumSpan| {
        let left = span.col0 > 0;
        let right = span.col0 + span.len < w;
        let up = span.row > 0;
        let down = span.row + 1 < h;
        let mut adjs = vec![];
        if left {
            adjs.push((span.col0 - 1, span.row));
        }
        if left && up {
            adjs.push((span.col0 - 1, span.row - 1))
        }
        if up {
            adjs.extend((0..span.len).map(|i| (span.col0 + i, span.row - 1)))
        }
        if up && right {
            adjs.push((span.col0 + span.len, span.row - 1))
        }
        if right {
            adjs.push((span.col0 + span.len, span.row));
        }
        if right && down {
            adjs.push((span.col0 + span.len, span.row + 1))
        }
        if down {
            adjs.extend((0..span.len).map(|i| (span.col0 + i, span.row + 1)))
        }
        if down && left {
            adjs.push((span.col0 - 1, span.row + 1))
        }
        adjs
    };
    let sum: u64 = engine
        .split(|c: char| !c.is_ascii_digit())
        .filter(|&s| !s.is_empty())
        .map(move |s| {
            let i = s.as_ptr() as usize - engine.as_ptr() as usize;
            NumSpan { row: i / (w + 1), col0: i % (w + 1), len: s.len() }
        })
        .flat_map(|numspan| {
            adjs(numspan)
                .into_iter()
                .filter_map(move |(x, y)| engine[y * (w + 1) + x..].starts_with('*').then_some(((x, y), numspan)))
        })
        .fold(HashMap::new(), |mut acc, (pos, NumSpan { row, col0, len })| {
            let num = engine[row * (w + 1) + col0..][..len].parse::<u64>().unwrap();
            acc.entry(pos).or_insert(vec![]).push(num);
            acc
        })
        .into_values()
        .filter(|nums| nums.len() == 2)
        .map(|nums| nums[0] * nums[1])
        .sum();
    println!("part 2: {sum}");
}
