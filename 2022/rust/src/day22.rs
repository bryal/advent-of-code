use crate::linalg::*;

const N_SQUARES: usize = 6;

// const SIDE_LEN: usize = 4;
// static FILE: &str = "example";
const SIDE_LEN: usize = 50;
static FILE: &str = "../inputs/day22.txt";

pub fn day22() {
    let (squares, path) = parse();
    let flat_graph = plane_map(&squares);
    let cube_graph = cube_map(&squares);
    let mut flat_walker = flat_graph.walker_at_origin();
    let mut cube_walker = cube_graph.walker_at_origin();
    for path_instruction in path {
        flat_walker.follow(path_instruction);
        cube_walker.follow(path_instruction);
    }
    println!("Part 1: {}", flat_walker.password());
    println!("Part 2: {}", cube_walker.password());
}

type Path = Vec<PathInstr>;

#[derive(Debug, Clone, Copy)]
enum PathInstr {
    Move(u8),
    RotCw,
    RotCcw,
}
use PathInstr::*;

struct Walker<'g> {
    graph: &'g Graph,
    node: NodeIx,
    pos: Vec2, // Position within a square
    dir: Dir,
}

impl<'g> Walker<'g> {
    fn follow(&mut self, instr: PathInstr) {
        match instr {
            RotCw => self.dir = self.dir.rot_cw(),
            RotCcw => self.dir = self.dir.rot_ccw(),
            Move(m) => {
                for _ in 0..m {
                    let non_wrapped = self.pos.add(self.dir.to_vec());
                    let mut next = non_wrapped.map(|a| a.rem_euclid(SIDE_LEN as i64));
                    let (new_node, n_rots) = if next != non_wrapped {
                        self.graph.edges[self.node][self.dir as usize]
                    } else {
                        (self.node, 0)
                    };
                    let mut dir = self.dir;
                    for _ in 0..n_rots {
                        // Rotate around centre of square, not around (0,0)
                        let shift = Vec2 { x: SIDE_LEN as i64 / 2, y: SIDE_LEN as i64 / 2 };
                        next = next
                            .sub(shift)
                            .map(|a| if a >= 0 { a + 1 } else { a })
                            .extend(0)
                            .rot_around(Vec3::z_axis())
                            .trunc()
                            .map(|a| if a > 0 { a - 1 } else { a })
                            .add(shift);
                        dir = dir.rot_cw();
                    }
                    if self.graph.nodes[new_node].1.free(next) {
                        self.node = new_node;
                        self.pos = next;
                        self.dir = dir;
                    } else {
                        break;
                    }
                }
            }
        }
    }

    fn password(&self) -> i64 {
        let Vec2 { x, y } =
            vmap2(|a, b| a * SIDE_LEN as i64 + b, self.graph.nodes[self.node].0, self.pos);
        1000 * (y + 1) + 4 * (x + 1) + self.dir as i64
    }
}

fn cube_map(squares: &[(Vec2, Square); N_SQUARES]) -> Graph {
    // To determine a face's orientation in 3D, we need two direction vectors, which point to the
    // square's original (up, right) when lying on the plane.
    let poss = squares.map(|(pos, _)| pos);
    let mut orientations = [(Vec3 { x: 0, y: 1, z: 0 }, Vec3 { x: 1, y: 0, z: 0 }); 6];
    fold_neighs(0, 0, &mut orientations, &poss);

    // Cross product
    let normal = |(up, right): Orientation| Vec3 {
        x: right.y * up.z - right.z * up.y,
        y: right.z * up.x - right.x * up.z,
        z: right.x * up.y - right.y * up.x,
    };

    let edges = std::array::from_fn(|from| {
        let from_orient = orientations[from];
        [Right, Down, Left, Up].map(|dir| {
            // Depending on how we reach a certain face, the orientation may differ. The
            // normal will always be the same though.
            let to_expected_orient = orientation_past_fold(from_orient, dir);
            let to_normal = normal(to_expected_orient);
            let to =
                orientations.iter().position(|&to_orient| normal(to_orient) == to_normal).unwrap();
            let to_orient = orientations[to];
            let mut n_rots = 0;
            let mut orient = to_expected_orient;
            while orient != to_orient {
                n_rots += 1;
                orient = (orient.0.rot_around(to_normal), orient.1.rot_around(to_normal));
            }
            (to, n_rots)
        })
    });
    Graph { nodes: *squares, edges }
}

fn fold_neighs(prev: NodeIx, from: NodeIx, orientations: &mut [Orientation; 6], poss: &[Vec2; 6]) {
    for (dir, node) in [Right, Down, Left, Up].into_iter().flat_map(|dir| {
        poss.iter()
            .position(|&pos| pos == poss[from].add(dir.to_vec()))
            .map(|node| (dir, node))
            .filter(|&(_, node)| node != prev)
    }) {
        orientations[node] = orientation_past_fold(orientations[from], dir);
        fold_neighs(from, node, orientations, poss)
    }
}

fn orientation_past_fold((up, right): Orientation, dir: Dir) -> Orientation {
    match dir {
        Up => (up.rot_around(right.flip()), right),
        Down => (up.rot_around(right), right),
        Right => (up, right.rot_around(up)),
        Left => (up, right.rot_around(up.flip())),
    }
}

type Orientation = (Vec3, Vec3);

fn plane_map(squares: &[(Vec2, Square); N_SQUARES]) -> Graph {
    let edges = squares.map(|(here, _)| {
        [Right, Down, Left, Up].map(|dir| {
            let dirv = dir.to_vec();
            let mut there = here;
            let other = loop {
                there = vmap2(|a, b| (a + b).rem_euclid(N_SQUARES as i64), there, dirv);
                if let Some((i, _)) = squares.iter().enumerate().find(|(_, (pos, _))| *pos == there)
                {
                    break i;
                }
            };
            (other, 0) // No rotations to match orientation needed on the plane
        })
    });
    Graph { nodes: *squares, edges }
}

fn parse() -> ([(Vec2, Square); N_SQUARES], Path) {
    let inp = std::fs::read_to_string(FILE).unwrap();
    let (s_board, s_path) = inp.rsplit_once("\n\n").unwrap();
    let s_path = s_path.trim();
    let board_lines = s_board.lines().map(|l| l.as_bytes()).collect::<Vec<_>>();
    let squares = (0..4)
        .flat_map(|sq_y| (0..4).map(move |sq_x| (sq_x, sq_y)))
        .filter(|(sq_x, sq_y)| {
            board_lines
                .get(sq_y * SIDE_LEN)
                .and_then(|l| l.get(sq_x * SIDE_LEN).cloned())
                .unwrap_or(b' ')
                != b' '
        })
        .map(|(sq_x, sq_y)| {
            let square = std::array::from_fn(|yoffset| {
                let y = sq_y * SIDE_LEN + yoffset;
                (0..SIDE_LEN)
                    .map(|xoffset| {
                        let x = sq_x * SIDE_LEN + xoffset;
                        ((board_lines[y][x] == b'#') as u64) << xoffset
                    })
                    .sum()
            });
            (Vec2 { x: sq_x as i64, y: sq_y as i64 }, Square(square))
        })
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();
    let path = s_path
        .split_inclusive(|c: char| c.is_alphabetic())
        .flat_map(|s| {
            if let Some(i) = s.rfind(|c: char| c.is_alphabetic()) {
                vec![
                    Move(s[..i].parse::<u8>().unwrap()),
                    if &s[i..] == "R" { RotCw } else { RotCcw },
                ]
            } else {
                vec![Move(s.parse::<u8>().unwrap())]
            }
        })
        .collect();
    (squares, path)
}

#[derive(Debug)]
struct Graph {
    nodes: [Node; N_SQUARES],
    // For the 4 sides of each square, to which other square is it connected, and how many clockwise
    // rotations are needed to match the orientation.
    edges: [[(NodeIx, u8); 4]; N_SQUARES],
}

impl Graph {
    fn walker_at_origin(&self) -> Walker {
        Walker {
            graph: self,
            node: 0, // Because of how we parse the board, this is true
            pos: Vec2 { x: 0, y: 0 },
            dir: Right,
        }
    }
}

type NodeIx = usize;
type Node = (Vec2, Square);

#[derive(Debug, Clone, Copy)]
enum Dir {
    Right = 0,
    Down,
    Left,
    Up,
}
use Dir::*;

impl Dir {
    fn to_vec(self) -> Vec2 {
        match self {
            Right => Vec2 { x: 1, y: 0 },
            Down => Vec2 { x: 0, y: 1 },
            Left => Vec2 { x: -1, y: 0 },
            Up => Vec2 { x: 0, y: -1 },
        }
    }

    fn rot_cw(self) -> Self {
        Self::from_u8(self as u8 + 1)
    }

    fn rot_ccw(self) -> Self {
        Self::from_u8(self as u8 + 4 - 1)
    }

    fn from_u8(n: u8) -> Self {
        match n % 4 {
            0 => Right,
            1 => Down,
            2 => Left,
            _ => Up,
        }
    }
}

// Each of the first SIDE_LEN bits represent whether the tile is blocked or free.
#[derive(Clone, Copy)]
struct Square([u64; SIDE_LEN]);

impl Square {
    fn free(&self, pos: Vec2) -> bool {
        assert!(pos.x >= 0 && pos.y >= 0);
        ((self.0[pos.y as usize] >> pos.x as usize) & 1) == 0
    }
}

impl std::fmt::Debug for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        for row in self.0 {
            for col in 0..SIDE_LEN {
                write!(f, "{}", (row >> col) & 1)?
            }
            writeln!(f)?;
        }
        Ok(())
    }
}
