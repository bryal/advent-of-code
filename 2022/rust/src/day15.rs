use std::cmp::{max, min};
use std::collections::HashSet;
use std::ops::{Bound::*, RangeBounds, RangeInclusive};

pub fn part1() {
    let (sensors, beacons, yscan, _) = parse();
    let (covered, row_beacons) = scan_row(.., yscan, &sensors, &beacons);
    let known_unoccupied =
        covered.into_iter().map(|range| range.count()).sum::<usize>() - row_beacons.len();
    println!("Part 1: {}", known_unoccupied);
}

pub fn part2() {
    let (sensors, beacons, _, dim_max) = parse();
    for y in 0..=dim_max {
        let (covered, _row_beacons) = scan_row(0..=dim_max, y, &sensors, &beacons);
        let n_covered = covered.iter().map(|r| r.size_hint().0 as i64).sum::<i64>();
        if n_covered < dim_max + 1 {
            let mut prev = -1;
            let mut x_result = dim_max;
            for x in covered.into_iter().flatten() {
                if x > prev + 1 {
                    x_result = x - 1;
                }
                prev = x;
            }
            println!("Part 2: {}", x_result * 4000000 + y);
            return;
        }
    }
}

fn scan_row<R>(
    xscan: R,
    yscan: i64,
    sensors: &[Sensor],
    beacons: &HashSet<[i64; 2]>,
) -> (Vec<RangeInclusive<i64>>, Vec<i64>)
where
    R: RangeBounds<i64>,
{
    // Scanning left to right, entering and exiting a sensor's rhombus of influence.
    #[derive(PartialEq, Eq, PartialOrd, Ord)]
    enum Event {
        Enter,
        Exit,
    }
    use Event::*;

    let mut events = sensors
        .iter()
        .filter_map(|Sensor { pos: [x, y], rad }| {
            let dx = rad - (yscan - y).abs();
            if (yscan - y).abs() > *rad {
                None
            } else if let (Included(&x0), Included(&xn)) = (xscan.start_bound(), xscan.end_bound())
            {
                (x + dx >= x0 && x - dx <= xn)
                    .then_some([(max(x - dx, x0), Enter), (min(x + dx, xn), Exit)])
            } else {
                Some([(x - dx, Enter), (x + dx, Exit)])
            }
        })
        .flatten()
        .collect::<Vec<_>>();
    events.sort();
    let row_beacons = beacons.iter().filter_map(|&[x, y]| (y == yscan).then_some(x)).collect();

    let mut covered = vec![];
    let mut depth = 0;
    let mut start = i64::MIN;
    for (x, event) in events {
        match event {
            Enter => {
                if depth == 0 {
                    start = x;
                }
                depth += 1;
            }
            Exit => {
                depth -= 1;
                if depth == 0 {
                    covered.push(start..=x)
                }
            }
        }
    }
    (covered, row_beacons)
}

fn parse() -> (Vec<Sensor>, HashSet<[i64; 2]>, i64, i64) {
    let (inp, yscan, dim_max) =
        (std::fs::read_to_string("../inputs/day15.txt").unwrap(), 2000000, 4000000);
    //let (inp, yscan, dim_max) = (std::fs::read_to_string("example").unwrap(), 10, 20);

    let (mut sensors, mut beacons) = (vec![], HashSet::new());
    for l in inp.lines() {
        let mut it = l.split('=').skip(1).map(|s| {
            s.split_once(|c| c == ',' || c == ':').map(|(s, _)| s).unwrap_or(s).parse().unwrap()
        });
        let [sx, sy, bx, by]: [i64; 4] = std::array::from_fn(|_| it.next().unwrap());
        let (ps, pb) = ([sx, sy], [bx, by]);
        sensors.push(Sensor { pos: ps, rad: dist(ps, pb) });
        beacons.insert(pb);
    }
    (sensors, beacons, yscan, dim_max)
}

fn dist(p1: [i64; 2], p2: [i64; 2]) -> i64 {
    p1.into_iter().zip(p2).map(|(a1, a2)| (a1 - a2).abs()).sum()
}

#[derive(Debug)]
struct Sensor {
    pos: [i64; 2],
    rad: i64,
}
