use std::{io, cmp::Ordering, f64::consts::PI};

use scanner::UnsafeScanner;

#[derive(Clone, Copy, Debug)]
struct Point { x: i32, y: i32 }

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    fn cmp_yx(&self, other: &Point) -> Ordering {
        if self.y == other.y { return self.x.cmp(&other.x); }
        self.y.cmp(&other.y)
    }

    fn cmp_ccw(&self, other: &Point, mark: &Point) -> Ordering {
        let cp = ccw(mark, self, other);
        match 0.cmp(&cp) {
            Ordering::Equal => self.cmp_yx(other),
            ord => ord,
        }
    }
}

/// Determines whether the vector b, from p1 to p3, is in the counter clockwise direction of
/// the vector a, from p1 to p2.
/// 
/// Returns the cross product of a and b.
fn ccw(p1: &Point, p2: &Point, p3: &Point) -> i32 {
    // Defines a to be a vector from p1 to p2.
    // Similar for b, but from p1 to p3.
    let a = (p2.x - p1.x, p2.y - p1.y);
    let b = (p3.x - p1.x, p3.y - p1.y);
    a.0 * b.1 - a.1 * b.0
}

/// Returns the l2 distance of the edge whose both ends are p1 and p2.
fn l2_dist(p1: &Point, p2: &Point) -> f64 {
    f64::from((p2.x - p1.x).pow(2) + (p2.y - p1.y).pow(2)).sqrt()
}

fn circumference(radius: u32) -> f64 {
    2. * f64::from(radius) * PI
}

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let (n, l) = (scan.token::<usize>(), scan.token::<u32>());

    let mut points = Vec::with_capacity(n);
    for _ in 0..n {
        let (x, y) = (scan.token::<i32>(), scan.token::<i32>());
        points.push(Point::new(x, y));
    }

    points.sort_unstable_by(|a, b| a.cmp_yx(b));
    let mark = points[0];
    points[1..].sort_unstable_by(|a, b| a.cmp_ccw(b, &mark));

    let mut hull = Vec::with_capacity(n);
    hull.push(points[0]);
    hull.push(points[1]);

    points.iter().take(n).skip(2).for_each(|cur| {
        while hull.len() >= 2 {
            let p2 = *hull.last().unwrap();
            hull.pop();

            let p1 = *hull.last().unwrap();
            if ccw(&p1, &p2, cur) > 0 {
                hull.push(p2);
                break;
            }
        }
        hull.push(*cur);
    });

    let mut res = circumference(l);

    if hull.len() == 2 {
        res += 2. * l2_dist(&hull[0], &hull[1]);
    }
    else {
        for cur in 0..hull.len() {
            let (next, nnext) = ((cur + 1) % hull.len(), (cur + 2) % hull.len());
            res += l2_dist(&hull[cur], &hull[next]);
        }
    }

    println!("{}", res.round() as i32);
}

mod scanner {
    use std::{io, str};

    pub(crate) struct UnsafeScanner<R> {
        reader: R,
        buf_str: Vec<u8>,
        buf_iter: str::SplitAsciiWhitespace<'static>,
    }

    impl <R: io::BufRead> UnsafeScanner<R> {
        pub(crate) fn new(reader: R) -> Self {
            Self {
                reader,
                buf_str: vec![],
                buf_iter: "".split_ascii_whitespace(),
            }
        }

        pub(crate) fn token<T: str::FromStr>(&mut self) -> T {
            loop {
                if let Some(token) = self.buf_iter.next() {
                    return token.parse().ok().expect("Failed parse");
                }
                self.buf_str.clear();
                self.reader
                    .read_until(b'\n', &mut self.buf_str)
                    .expect("Failed read");
                self.buf_iter = unsafe {
                    let slice = str::from_utf8_unchecked(&self.buf_str);
                    std::mem::transmute(slice.split_ascii_whitespace())
                }
            }
        }
    }
}
