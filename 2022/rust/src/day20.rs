use std::collections::VecDeque;

pub fn both_parts() {
    let xs0 = parse();
    let mut xs = xs0.clone();
    mix(&mut xs);
    println!("Part 1: {}", grove_coords(&xs));

    let decryption_key = 811589153;
    let mut xs = xs0;
    for (_, x) in &mut xs {
        *x *= decryption_key;
    }
    for _ in 0..10 {
        mix(&mut xs);
    }
    println!("Part 2: {}", grove_coords(&xs));
}

fn grove_coords(xs: &VecDeque<(usize, i64)>) -> i64 {
    let i0 = xs.iter().position(|&(_, x)| x == 0).unwrap();
    [1000, 2000, 3000].into_iter().map(|nth| xs[(i0 + nth) % xs.len()].1).sum::<i64>()
}

fn mix(xs: &mut VecDeque<(usize, i64)>) {
    for i in 0..xs.len() {
        while let Some((j, x)) = {
            xs.rotate_left(1);
            xs.back().cloned()
        } {
            if j == i {
                xs.pop_back();
                let k = if x < 0 {
                    xs.len() - (x.unsigned_abs() as usize % xs.len())
                } else {
                    x as usize % xs.len()
                };
                xs.insert(k, (j, x));
                break;
            }
        }
    }
}

fn parse() -> VecDeque<(usize, i64)> {
    // let inp = "1\n2\n-3\n3\n-2\n0\n4";
    let inp = std::fs::read_to_string("../inputs/day20.txt").unwrap();
    inp.lines().map(|l| l.parse().unwrap()).enumerate().collect()
}
