use std::io::{self, Write};

use algorithm::convex_hull::{Point, find_convex_hull, check_intersect};
use scanner::UnsafeScanner;


fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let t = scan.token::<usize>();

    for _ in 0..t {
        let (n, m) = (scan.token::<usize>(), scan.token::<usize>());

        let mut blacks = Vec::with_capacity(n);
        for _ in 0..n {
            let (x, y) = (scan.token::<i64>(), scan.token::<i64>()); 
            blacks.push(Point::new(x, y));
        }

        let mut whites = Vec::with_capacity(m);
        for _ in 0..m {
            let (x, y) = (scan.token::<i64>(), scan.token::<i64>());
            whites.push(Point::new(x, y));
        }

        let black_hull = find_convex_hull(blacks);
        let white_hull = find_convex_hull(whites);

        let mut can_separate = true;

        if black_hull.len() > 2 {
            for white in white_hull.iter() {
                if white.is_inner_of(&black_hull) { can_separate = false; }
            }
        }
        
        if white_hull.len() > 2 {
            for black in black_hull.iter() {
                if black.is_inner_of(&white_hull) { can_separate = false; }
            }
        }
        
        for i in 0..black_hull.len() {
            for j in 0..white_hull.len() {
                if check_intersect(
                    &black_hull[i],
                    &black_hull[(i + 1) % black_hull.len()],
                    &white_hull[j],
                    &white_hull[(j + 1) % white_hull.len()]
                ) {
                    can_separate = false;
                }
            }
        }

        if can_separate { writeln!(out, "YES").unwrap(); } else { writeln!(out, "NO").unwrap(); }
    }
}

mod algorithm {
    pub(crate) mod convex_hull {
        use std::cmp::{Ordering, self};

        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
        pub(crate) struct Point { x: i64, y: i64 }

        impl Point {
            pub(crate) fn new(x: i64, y: i64) -> Self {
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

            pub(crate) fn is_inner_of(&self, hull: &Vec<Point>) -> bool {
                let init_dir = ccw(self, &hull[0], &hull[1]);
                for i in 1..hull.len() {
                    if init_dir * ccw(self, &hull[i], &hull[(i + 1) % hull.len()]) <= 0 {
                        return false;
                    }
                }
                true
            }
        }

        /// Determines whether the vector b, from p1 to p3, is in the counter clockwise direction of
        /// the vector a, from p1 to p2.
        /// 
        /// Returns the cross product of a and b.
        pub(crate) fn ccw(p1: &Point, p2: &Point, p3: &Point) -> i64 {
            // Defines a to be a vector from p1 to p2.
            // Similar for b, but from p1 to p3.
            let a = (p2.x - p1.x, p2.y - p1.y);
            let b = (p3.x - p1.x, p3.y - p1.y);
            a.0 * b.1 - a.1 * b.0
        }

        /// Returns true if the line segment ab, whose endpoints are a and b, intersects
        /// with the line segment cd, whose endpoints are c and d.
        pub(crate) fn check_intersect(a: &Point, b: &Point, c: &Point, d: &Point) -> bool {
            let (abc, abd, cda, cdb) = (
                ccw(a, b, c),
                ccw(a, b, d),
                ccw(c, d, a),
                ccw(c, d, b),
            );
            let (on_ab, on_cd) = (abc * abd, cda * cdb);
            if on_ab <= 0 && on_cd <= 0 {
                if on_ab == 0 && on_cd == 0 {
                    return cmp::min(a, b) <= cmp::max(c, d) && 
                           cmp::min(c, d) <= cmp::max(a, b)
                }
                return true;
            }
            false
        }

        pub(crate) fn find_convex_hull(mut points: Vec<Point>) -> Vec<Point> {
            if points.len() <= 2 { return points; }

            points.sort_unstable_by(|a, b| a.cmp_yx(b));
            let mark = points[0];
            points[1..].sort_unstable_by(|a, b| a.cmp_ccw(b, &mark));

            let mut hull = Vec::with_capacity(points.len());
            hull.push(points[0]);
            hull.push(points[1]);

            points.iter().take(points.len()).skip(2).for_each(|cur| {
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

            hull
        }
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
