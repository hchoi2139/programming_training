use std::{io, cmp::Ordering};
use io::Write;

use scanner::UnsafeScanner;

#[derive(Clone, Copy, Debug)]
struct Point { x: i64, y: i64 }

impl Point {
    fn new(x: i64, y: i64) -> Self {
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
fn ccw(p1: &Point, p2: &Point, p3: &Point) -> i64 {
    // Defines a to be a vector from p1 to p2.
    // Similar for b, but from p1 to p3.
    let a = (p2.x - p1.x, p2.y - p1.y);
    let b = (p3.x - p1.x, p3.y - p1.y);
    a.0 * b.1 - a.1 * b.0
}

/// Determines whether the vector b, from p3 to p4, is in the counter clockwise direction of
/// the vector a, from p1 to p2.
/// 
/// Returns the cross product of a and b.
fn cccw(
    p1: &Point,
    p2: &Point,
    p3: &Point,
    p4: &Point,
) -> i64 {
    let pp = Point::new(p4.x - p3.x + p1.x, p4.y - p3.y + p1.y);
    ccw(p1, p2, &pp)
}

fn l2_squared_dist(p1: &Point, p2: &Point) -> i64 {
    let diff = (p2.x - p1.x, p2.y - p1.y);
    diff.0 * diff.0 + diff.1 * diff.1
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let t = scan.token::<u32>();

    for _ in 0..t {
        let n = scan.token::<usize>();

        let mut points: Vec<Point> = Vec::with_capacity(n);
        (0..n).for_each(|_| {
            let (x, y) = (scan.token::<i64>(), scan.token::<i64>());
            points.push(Point::new(x, y));
        });

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

        let (mut j, n_hull) = (1, hull.len());
        let (mut p1, mut p2, mut ans) = (Point::new(0, 0), Point::new(0, 0), 0);
        for i in 0..n_hull {
            while cccw(&hull[i], &hull[(i + 1) % n_hull], &hull[j % n_hull], &hull[(j + 1) % n_hull]) > 0 {
                j += 1;
            }
            if ans < l2_squared_dist(&hull[i], &hull[j % n_hull]) {
                ans = l2_squared_dist(&hull[i], &hull[j % n_hull]);
                p1 = hull[i];
                p2 = hull[j % n_hull];
            }
        }

        writeln!(out, "{} {} {} {}", p1.x, p1.y, p2.x, p2.y).unwrap();
    }
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
