use aoc2023::*;

fn main() {
    println!("== test ==");
    part1(TEST);
    part2(TEST);
    println!("== real ==");
    let inp = day_input(6);
    part1(&inp);
    part2(&inp);
}

fn part1(inp: &str) {
    // It's a parabola -- a grade 2 polynomial.
    // d = x * (t - x)
    // d = tx - x^2
    // x^2 - tx + d = 0
    // x^2 - tx + (t/2)^2 = (t/2)^2 - d
    // (x - t/2)^2 = (t/2)^2 - d
    // x - t/2 = sqrt((t/2)^2 - d)
    // x = sqrt(t^2/4 - d) + t/2
    // x = (t +- sqrt(t^2 - 4d)) / 2

    let (ts, ds) = inp.split_once('\n').unwrap();
    let ts = ts[5..].split_ascii_whitespace().filter_map(|s| s.parse().ok());
    let ds = ds[9..].split_ascii_whitespace().filter_map(|s| s.parse().ok());
    let races = ts.zip(ds).collect::<Vec<(i64, i64)>>();
    let n: u64 = races
        .into_iter()
        .map(|(t, record_d)| {
            let d = record_d + 1;
            let (t, d) = (t as f64, d as f64);
            let sqrt = (t * t - 4.0 * d).sqrt();
            let x1 = ((t - sqrt) / 2.0).ceil() as i64;
            let x2 = ((t + sqrt) / 2.0).floor() as i64;
            (x2 - x1 + 1) as u64
        })
        .product();
    println!("part 1: {}", n);
}

fn part2(inp: &str) {
    let mut inp = inp.lines();
    let ts = inp.next().unwrap();
    let ds = inp.next().unwrap();
    let t = ts[5..].bytes().filter(|c| c.is_ascii_digit()).map(|c| (c - b'0') as u64).fold(0, |t, x| 10 * t + x);
    let record_d = ds[9..].bytes().filter(|c| c.is_ascii_digit()).map(|c| (c - b'0') as u64).fold(0, |d, x| 10 * d + x);
    let d = record_d + 1;
    let (t, d) = (t as f64, d as f64);
    let sqrt = (t * t - 4.0 * d).sqrt();
    let x1 = ((t - sqrt) / 2.0).ceil() as i64;
    let x2 = ((t + sqrt) / 2.0).floor() as i64;
    let n = (x2 - x1 + 1) as u64;
    println!("part 2: {}", n);
}

const TEST: &str = r#"Time:      7  15   30
Distance:  9  40  200"#;
