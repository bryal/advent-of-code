#![allow(dead_code)]

use std::collections::HashMap;

// TODO: Try (Mixed?) Integer Programming

pub fn part1() {
    let mut bps = parse();
    println!(
        "Part 1: {:?}",
        bps.iter_mut()
            .map(|bp| {
                println!("blueprint {}", bp.id);
                let o = bp.simulate([0, 0, 0], [1, 0, 0, 0], 24) as u64;
                let r = bp.id as u64 * o;
                println!("  cache: {}, opened: {}, result: {}", bp.cache.len(), o, r);
                r
            })
            .sum::<u64>()
    );
}

pub fn part2() {
    let mut bps = parse();
    println!(
        "Part 2: {:?}",
        bps.iter_mut()
            .take(3)
            .map(|bp| {
                println!("blueprint {}", bp.id);
                let o = bp.simulate([0, 0, 0], [1, 0, 0, 0], 32) as u64;
                println!("  cache: {}, opened: {}", bp.cache.len(), o);
                o
            })
            .product::<u64>()
    );
}

fn parse() -> Vec<Blueprint> {
    //let example = "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay.Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.";
    //let inp = EXAMPLE;
    let inp = std::fs::read_to_string("../inputs/day19.txt").unwrap();

    inp.lines().map(|l| parse_blueprint(l).unwrap()).collect::<Vec<_>>()
}

fn parse_blueprint(s: &str) -> Option<Blueprint> {
    let (id, s) = s.strip_prefix("Blueprint ")?.split_once(':')?;
    let (ore_ore, s) = s.split_once("costs ")?.1.split_once(' ')?;
    let (clay_ore, s) = s.split_once("costs ")?.1.split_once(' ')?;
    let (obsidian_ore, s) = s.split_once("costs ")?.1.split_once(' ')?;
    let (obsidian_clay, s) = s.split_once("and ")?.1.split_once(' ')?;
    let (geode_ore, s) = s.split_once("costs ")?.1.split_once(' ')?;
    let (geode_obsidian, _) = s.split_once("and ")?.1.split_once(' ')?;
    let id = id.parse().ok()?;
    let ore = ore_ore.parse().ok()?;
    let clay = clay_ore.parse().ok()?;
    let obsidian = (obsidian_ore.parse().ok()?, obsidian_clay.parse().ok()?);
    let geode = (geode_ore.parse().ok()?, geode_obsidian.parse().ok()?);
    Some(Blueprint::new(id, ore, clay, obsidian, geode))
}

#[derive(Debug)]
struct Blueprint {
    id: usize,
    costs: [[u8; 3]; 4],
    cache: HashMap<([u8; 3], [u8; 4], u8), u8>,
}

impl Blueprint {
    fn new(id: usize, ore: u8, clay: u8, obsidian: (u8, u8), geode: (u8, u8)) -> Self {
        let costs = [[ore, 0, 0], [clay, 0, 0], [obsidian.0, obsidian.1, 0], [geode.0, 0, geode.1]];
        Self { id, costs, cache: HashMap::new() }
    }

    fn simulate(&mut self, resources: [u8; 3], robots: [u8; 4], t_rem: u8) -> u8 {
        if let Some(&memoized) = self.cache.get(&(resources, robots, t_rem)) {
            memoized
        } else if t_rem == 1 {
            robots[3]
        } else {
            let mut alternatives = std::iter::once(Some((resources, robots)))
                .chain((0..4).map(|i| self.build_bot(i, resources, robots)));
            let alternatives: [_; 5] = std::array::from_fn(|_| alternatives.next().unwrap());
            let optimal = robots[3]
                + alternatives
                    .into_iter()
                    .flatten()
                    .map(|(resources, robots_new)| {
                        self.simulate(gather(resources, robots), robots_new, t_rem - 1)
                    })
                    .max()
                    .unwrap_or(0);
            self.cache.insert((resources, robots, t_rem), optimal);
            optimal
        }
    }

    fn build_bot(
        &self,
        i: usize,
        mut resources: [u8; 3],
        mut robots: [u8; 4],
    ) -> Option<([u8; 3], [u8; 4])> {
        let cost = self.costs[i];
        for (c, r) in cost.iter().zip(&mut resources) {
            if c > r {
                return None;
            }
            *r -= c
        }
        robots[i] += 1;
        Some((resources, robots))
    }
}

fn gather(resources: [u8; 3], robots: [u8; 4]) -> [u8; 3] {
    std::array::from_fn(|i| resources[i] + robots[i])
}
