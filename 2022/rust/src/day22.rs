const N_SQUARES: usize = 6;

// const SIDE_LEN: usize = 4;
// static FILE: &str = "example";
const SIDE_LEN: usize = 50;
static FILE: &str = "../inputs/day22.txt";

pub fn day22() {
    let (squares, path) = parse();
    let graph = Graph::from_squares(&squares);
    let mut world_pos = graph.origin_world_pos();
    for instr in path {
        world_pos = world_pos.exec(instr);
        // println!(
        //     "node: {}, pos: {:?}, dir: {:?}",
        //     world_pos.node,
        //     (world_pos.pos.x, world_pos.pos.y),
        //     world_pos.dir
        // );
    }
    println!("Part 1: {}", world_pos.password());
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

type Path = Vec<PathInstr>;

#[derive(Debug)]
enum PathInstr {
    Move(u8),
    RotCw,
    RotCcw,
}
use PathInstr::*;

struct WorldPos<'g> {
    graph: &'g Graph,
    node: NodeIx,
    pos: Vec2, // Position within a square
    dir: Dir,
}

impl<'g> WorldPos<'g> {
    fn exec(&self, instr: PathInstr) -> Self {
        match instr {
            RotCw => Self { dir: self.dir.rot_cw(), ..*self },
            RotCcw => Self { dir: self.dir.rot_ccw(), ..*self },
            Move(m) => {
                let mut node = self.node;
                let mut pos = self.pos;
                for _ in 0..m {
                    let next = pos.add(self.dir.out_vec());
                    let wrapped = next.map(|a| a.rem_euclid(SIDE_LEN as i64));
                    let new_node = if next != wrapped {
                        self.graph.edges[node][self.dir as usize].0
                    } else {
                        node
                    };
                    if self.graph.nodes[new_node].1.free(wrapped) {
                        node = new_node;
                        pos = wrapped
                    }
                }
                Self { node, pos, ..*self }
            }
        }
    }

    fn password(&self) -> i64 {
        let Vec2 { x, y } =
            vmap2(|a, b| a * SIDE_LEN as i64 + b, self.graph.nodes[self.node].0, self.pos);
        1000 * (y + 1) + 4 * (x + 1) + self.dir as i64
    }
}

// struct Board {
//     squares: [Square; 6],
// }

#[derive(Debug)]
struct Graph {
    nodes: [Node; N_SQUARES],
    // For the 4 sides of each square, to which other square is it connected, and at which side of
    // that square.
    edges: [[(NodeIx, Dir); 4]; N_SQUARES],
}

impl Graph {
    fn from_squares(squares: &[(Vec2, Square); N_SQUARES]) -> Self {
        let nodes = squares.clone();
        let edges = squares.clone().map(|(here, _)| {
            [Right, Down, Left, Up].map(|dir| {
                let dirv = dir.out_vec();
                let mut there = here;
                let other = loop {
                    there = vmap2(|a, b| (a + b).rem_euclid(N_SQUARES as i64), there, dirv);
                    if let Some((i, _)) =
                        squares.iter().enumerate().find(|(_, (pos, _))| *pos == there)
                    {
                        break i;
                    }
                };
                (other, dir.flip())
            })
        });
        Graph { nodes, edges }
    }

    fn origin_world_pos(&self) -> WorldPos {
        WorldPos {
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
    fn out_vec(self) -> Vec2 {
        match self {
            Right => Vec2 { x: 1, y: 0 },
            Down => Vec2 { x: 0, y: 1 },
            Left => Vec2 { x: -1, y: 0 },
            Up => Vec2 { x: 0, y: -1 },
        }
    }

    fn flip(self) -> Self {
        Self::from_u8(self as u8 + 2)
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
#[derive(Clone)]
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct Vec2 {
    x: i64,
    y: i64,
}

impl Vec2 {
    fn add(self, other: Vec2) -> Self {
        Self { x: self.x + other.x, y: self.y + other.y }
    }

    fn map<F>(self, mut f: F) -> Self
    where
        F: FnMut(i64) -> i64,
    {
        Self { x: f(self.x), y: f(self.y) }
    }
}

fn vmap2<F>(mut f: F, v: Vec2, u: Vec2) -> Vec2
where
    F: FnMut(i64, i64) -> i64,
{
    Vec2 { x: f(v.x, u.x), y: f(v.y, u.y) }
}
