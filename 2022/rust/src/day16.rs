use rayon::prelude::*;
use std::collections::{HashMap, HashSet, VecDeque};

pub fn part1() {
    let rooms = parse().unwrap();
    let rates = rates(&rooms);
    let jumps = jumps(&rooms);
    let valve0 = Valve::parse("AA");
    let remaining =
        rooms.iter().filter_map(|(v, r, _)| (*r > 0).then_some(*v)).collect::<Vec<Valve>>();
    println!("Part 1: {}", solve(&[(valve0, 30)], &remaining, &rates, &jumps))
}

pub fn part2() {
    let rooms = parse().unwrap();
    let rates = rates(&rooms);
    let jumps = jumps(&rooms);
    let valve0 = Valve::parse("AA");
    let remaining =
        rooms.iter().filter_map(|(v, r, _)| (*r > 0).then_some(*v)).collect::<Vec<Valve>>();
    println!("Part 2: {}", solve(&[(valve0, 26), (valve0, 26)], &remaining, &rates, &jumps))
}

fn solve(
    actors: &[(Valve, Minutes)],
    remaining: &[Valve],
    rates: &HashMap<Valve, Rate>,
    jumps: &HashMap<[Valve; 2], Minutes>,
) -> u16 {
    let go = |ri| {
        let next = remaining[ri];
        let (mut minutes, ai) = actors
            .iter()
            .enumerate()
            .map(|(ai, &(valve, minutes))| {
                let dist = jumps[&[valve, next]];
                (minutes.saturating_sub(dist), ai)
            })
            .max()
            .unwrap();
        if minutes == 0 {
            0
        } else {
            minutes -= 1;
            let mut actors = actors.to_vec();
            actors[ai] = (next, minutes);
            let mut remaining = remaining.to_vec();
            remaining.remove(ri);
            minutes * rates[&next] + solve(&actors, &remaining, rates, jumps)
        }
    };
    if remaining.len() > 13 {
        (0..remaining.len()).into_par_iter().map(go).max().unwrap_or(0)
    } else {
        (0..remaining.len()).map(go).max().unwrap_or(0)
    }
}

fn rates(rooms: &[(Valve, Rate, Vec<Valve>)]) -> HashMap<Valve, Rate> {
    rooms.iter().map(|(v, r, _)| (*v, *r)).collect()
}

fn jumps(rooms: &[(Valve, Rate, Vec<Valve>)]) -> HashMap<[Valve; 2], Minutes> {
    let tunnels = rooms.iter().map(|(v, _, ts)| (v, ts)).collect::<HashMap<_, _>>();
    rooms
        .iter()
        .flat_map(|(v1, _, _)| {
            let tunnels = &tunnels;
            rooms.iter().filter(|&&(_, rate, _)| rate > 0).filter_map(move |(v2, _, _)| {
                let mut seen = HashSet::new();
                let mut next = std::iter::once((v1, 0)).collect::<VecDeque<_>>();
                while let Some((v, dist)) = next.pop_front() {
                    if v == v2 {
                        return Some(([*v1, *v2], dist));
                    } else if seen.contains(v) {
                        continue;
                    }
                    seen.insert(v);
                    next.extend(tunnels[v].iter().map(|t| (t, dist + 1)))
                }
                None
            })
        })
        .collect()
}

fn parse() -> Option<Vec<(Valve, Rate, Vec<Valve>)>> {
    //let inp = std::fs::read_to_string("example").ok()?;
    let inp = std::fs::read_to_string("../inputs/day16.txt").ok()?;
    inp.lines()
        .map(|s| {
            let (valve, s) = s.split_once(' ')?.1.split_once(' ')?;
            let valve = Valve::parse(valve);
            let (rate, s) = s.split_once("rate=")?.1.split_once(';')?;
            let rate = rate.parse().ok()?;
            let tunnels = s
                .split_once("valves ")
                .or_else(|| s.split_once("valve "))?
                .1
                .split(", ")
                .map(Valve::parse)
                .collect();
            Some((valve, rate, tunnels))
        })
        .collect()
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct Valve(u16);
impl Valve {
    fn parse(s: &str) -> Self {
        let s = s.as_bytes();
        Valve((((s[0] - b'A') as u16) << 8) + (s[1] - b'A') as u16)
    }
}

impl std::fmt::Debug for Valve {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}{}",
            ((self.0 >> 8) as u8 + b'A') as char,
            ((self.0 & 0xFF) as u8 + b'A') as char
        )
    }
}

type Rate = u16;
type Minutes = u16;
