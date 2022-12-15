#![allow(dead_code)]

pub fn part1() {
    let (heights, start, end) = parse();
    let mut dists: Mat<i32> = heights.map(|pos, _| if pos == end { 0 } else { -1 });
    while dists[start] == -1 {
        dists = next_generation(&dists, &heights);
    }
    println!("Part 1: {}", dists[start])
}

pub fn part2() {
    let (heights, _, end) = parse();
    let starts = heights
        .iter()
        .filter_map(|(p, &h)| if h == b'a' { Some(p) } else { None })
        .collect::<Vec<_>>();
    let mut dists: Mat<i32> = heights.map(|pos, _| if pos == end { 0 } else { -1 });
    let start = loop {
        if let Some(&start) = starts.iter().find(|&&start| dists[start] != -1) {
            break start;
        }
        dists = next_generation(&dists, &heights);
    };
    println!("Part 2: {}", dists[start])
}

// Instead of doing a BFS, use a sort of Cellular Automaton method of flooding the map, one
// generation at a time. This would be a more suitable method in an array programming language,
// which I intend to re-solve this problem in. Probably Futhark or Co-dnfs and run it on the
// GPU. Maybe BQN.
fn next_generation(dists: &Mat<i32>, heights: &Mat<u8>) -> Mat<i32> {
    let traversable = |p1: Vec2, p2: Vec2| {
        let h1 = heights[p1];
        let h2 = heights[p2];
        h1 >= h2 || h1 + 1 == h2
    };
    dists.map(|p1, &dist| {
        if dist == -1 {
            p1.neighs(heights.dims)
                .into_iter()
                .filter(|&p2| traversable(p1, p2) && dists[p2] >= 0)
                .map(|p2| dists[p2] + 1)
                .min()
                .unwrap_or(-1)
        } else {
            dist
        }
    })
}

fn parse() -> (Mat<u8>, Vec2, Vec2) {
    let inp = std::fs::read_to_string("../inputs/day12.txt").unwrap();
    //let inp = std::fs::read_to_string("example").unwrap();
    let w = inp.lines().next().unwrap().len();
    let h = inp.len() / w;
    let dims = Vec2 { x: w, y: h };
    let mut heights = Mat { dims, data: inp.lines().flat_map(|l| l.bytes()).collect() };
    let start = heights.iter().find(|(_, &c)| c == b'S').unwrap().0;
    let end = heights.iter().find(|(_, &c)| c == b'E').unwrap().0;
    heights[start] = b'a';
    heights[end] = b'z';
    (heights, start, end)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Vec2 {
    x: usize,
    y: usize,
}

impl Vec2 {
    fn neighs(self, dims: Vec2) -> Vec<Vec2> {
        [
            if self.x > 0 { Some(Vec2 { x: self.x - 1, ..self }) } else { None },
            if self.x < dims.x - 1 { Some(Vec2 { x: self.x + 1, ..self }) } else { None },
            if self.y > 0 { Some(Vec2 { y: self.y - 1, ..self }) } else { None },
            if self.y < dims.y - 1 { Some(Vec2 { y: self.y + 1, ..self }) } else { None },
        ]
        .into_iter()
        .flatten()
        .collect()
    }
}

struct Mat<T> {
    dims: Vec2,
    data: Vec<T>,
}

impl<T> Mat<T> {
    fn iter(&self) -> impl Iterator<Item = (Vec2, &T)> {
        self.data
            .iter()
            .enumerate()
            .map(|(i, v)| (Vec2 { x: i % self.dims.x, y: i / self.dims.x }, v))
    }

    fn map<U>(&self, mut f: impl FnMut(Vec2, &T) -> U) -> Mat<U> {
        Mat { data: self.iter().map(|(p, v)| f(p, v)).collect(), dims: self.dims }
    }

    fn get(&self, pos: Vec2) -> Option<&T> {
        let i = pos.y * self.dims.x + pos.x;
        self.data.get(i)
    }

    fn get_mut(&mut self, pos: Vec2) -> Option<&mut T> {
        let i = pos.y * self.dims.x + pos.x;
        self.data.get_mut(i)
    }
}

impl<T> std::ops::Index<Vec2> for Mat<T> {
    type Output = T;
    fn index(&self, index: Vec2) -> &T {
        self.get(index).unwrap()
    }
}
impl<T> std::ops::IndexMut<Vec2> for Mat<T> {
    fn index_mut(&mut self, index: Vec2) -> &mut T {
        self.get_mut(index).unwrap()
    }
}
