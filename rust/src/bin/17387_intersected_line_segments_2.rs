use std::{io, cmp::{Ordering, self}};

use scanner::UnsafeScanner;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Point { x: i64, y: i64 }

impl Point {
    fn new(x: i64, y: i64) -> Self {
        Self { x, y }
    }
}

struct Line { p1: Point, p2: Point }

impl Line {
    fn new(p1: Point, p2: Point) -> Self {
        Self { p1, p2 }
    }
}

fn ccw(p1: &Point, p2: &Point, p3: &Point) -> i32 {
    // Defines a to be a vector from p1 to p2.
    // Similar for b, but from p1 to p3.
    let a = (p2.x - p1.x, p2.y - p1.y);
    let b = (p3.x - p1.x, p3.y - p1.y);

    let cp = a.0 * b.1 - a.1 * b.0;
    match cp.cmp(&0) {
        Ordering::Less  => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let mut ls = Vec::with_capacity(2);
    (0..2).for_each(|_| {
        let p1 = Point::new(scan.token::<i64>(), scan.token::<i64>());
        let p2 = Point::new(scan.token::<i64>(), scan.token::<i64>());
        ls.push(Line::new(cmp::min(p1, p2), cmp::max(p1, p2)));
    });

    let on_l1 = ccw(&ls[0].p1, &ls[0].p2, &ls[1].p1) * ccw(&ls[0].p1, &ls[0].p2, &ls[1].p2);
    let on_l2 = ccw(&ls[1].p1, &ls[1].p2, &ls[0].p1) * ccw(&ls[1].p1, &ls[1].p2, &ls[0].p2);

    if on_l1 <= 0 && on_l2 <= 0 {
        if on_l1 == 0 && on_l2 == 0 {
            if ls[0].p1 <= ls[1].p2 && ls[1].p1 <= ls[0].p2 { println!("1"); }
            else { println!("0"); }
            return;
        }
        println!("1");
    }
    else { println!("0"); }
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
