#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Vec3 {
    pub x: i64,
    pub y: i64,
    pub z: i64,
}

impl Vec3 {
    pub fn z_axis() -> Self {
        Self { x: 0, y: 0, z: 1 }
    }

    pub fn flip(self) -> Self {
        Vec3 { x: -self.x, y: -self.y, z: -self.z }
    }

    pub fn rot_around(self, axis: Vec3) -> Vec3 {
        let Vec3 { x, y, z } = self;
        match axis {
            Vec3 { x: 1, .. } => Vec3 { x, y: -z, z: y },
            Vec3 { x: -1, .. } => Vec3 { x, y: z, z: -y },
            Vec3 { y: 1, .. } => Vec3 { x: z, y, z: -x },
            Vec3 { y: -1, .. } => Vec3 { x: -z, y, z: x },
            Vec3 { z: 1, .. } => Vec3 { x: -y, y: x, z },
            Vec3 { z: -1, .. } => Vec3 { x: y, y: -x, z },
            _ => unimplemented!(),
        }
    }

    pub fn trunc(self) -> Vec2 {
        Vec2 { x: self.x, y: self.y }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Vec2 {
    pub x: i64,
    pub y: i64,
}

impl Vec2 {
    pub fn add(self, other: Vec2) -> Self {
        Self { x: self.x + other.x, y: self.y + other.y }
    }

    pub fn sub(self, other: Vec2) -> Self {
        Self { x: self.x - other.x, y: self.y - other.y }
    }

    pub fn map<F>(self, mut f: F) -> Self
    where
        F: FnMut(i64) -> i64,
    {
        Self { x: f(self.x), y: f(self.y) }
    }

    pub fn extend(self, z: i64) -> Vec3 {
        Vec3 { x: self.x, y: self.y, z }
    }
}

pub fn vmap2<F>(mut f: F, v: Vec2, u: Vec2) -> Vec2
where
    F: FnMut(i64, i64) -> i64,
{
    Vec2 { x: f(v.x, u.x), y: f(v.y, u.y) }
}
