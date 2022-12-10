#![allow(dead_code)]

use std::collections::HashSet;

pub fn part1() {
    let inp = std::fs::read_to_string("../inputs/day8.txt").unwrap();
    let grid = Grid::from_input(&inp);

    let mut visible = HashSet::new();
    let from_directions: [(std::ops::Range<usize>, Vec<usize>, bool); 4] = [
        (0..grid.height, (0..grid.width).collect(), true),
        (0..grid.height, (0..grid.width).rev().collect(), true),
        (0..grid.width, (0..grid.height).collect(), false),
        (0..grid.width, (0..grid.height).rev().collect(), false),
    ];
    for (zs, ws, is_swapped) in from_directions {
        let swap = |(z, w)| if is_swapped { (w, z) } else { (z, w) };
        for z in zs {
            let mut ws = ws.iter().cloned();
            let pos0 = swap((z, ws.next().unwrap()));
            let mut max_size = grid.get(pos0);
            visible.insert(pos0);
            for w in ws {
                let pos = swap((z, w));
                if grid.get(pos) > max_size {
                    visible.insert(pos);
                    max_size = grid.get(pos);
                }
            }
        }
    }
    println!("Part 1: {}", visible.len());
}

pub fn part2() {
    let inp = std::fs::read_to_string("../inputs/day8.txt").unwrap();
    let grid = Grid::from_input(&inp);

    let mut max_score = 0;
    for yout in 0..grid.height {
        for xout in 0..grid.width {
            let left = (0..xout)
                .rev()
                .position(|x| grid.get((x, yout)) >= grid.get((xout, yout)))
                .map(|n| n + 1)
                .unwrap_or(xout);
            let right = (xout + 1..grid.width)
                .position(|x| grid.get((x, yout)) >= grid.get((xout, yout)))
                .map(|n| n + 1)
                .unwrap_or(grid.width - xout - 1);
            let top = (0..yout)
                .rev()
                .position(|y| grid.get((xout, y)) >= grid.get((xout, yout)))
                .map(|n| n + 1)
                .unwrap_or(yout);
            let bottom = (yout + 1..grid.height)
                .position(|y| grid.get((xout, y)) >= grid.get((xout, yout)))
                .map(|n| n + 1)
                .unwrap_or(grid.height - yout - 1);
            let score = left * right * top * bottom;
            max_score = std::cmp::max(score, max_score);
        }
    }
    println!("Part 2: {}", max_score);
}

struct Grid {
    width: usize,
    height: usize,
    data: Vec<u8>,
}

impl Grid {
    fn from_input(inp: &str) -> Self {
        let width = inp.lines().next().unwrap().len();
        let data = inp
            .lines()
            .flat_map(|s| s.bytes().map(|b| b - b'0'))
            .collect::<Vec<_>>();
        let height = data.len() / width;
        Self {
            width,
            height,
            data,
        }
    }

    fn get(&self, (x, y): (usize, usize)) -> u8 {
        self.data[y * self.width + x]
    }
}
