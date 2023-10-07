use std::{io, cmp::Ordering};

use scanner::UnsafeScanner;

fn ccw(x1: i64, y1: i64, x2: i64, y2: i64, x3: i64, y3: i64) -> i64 {
    // Define a to be a vector from (x1, y1) to (x2, y2).
    // b is similarly defined but from (x1, y1) to (x3, y3).
    let a = (x2 - x1, y2 - y1);
    let b = (x3 - x1, y3 - y1);
    
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

    let (x1, y1, x2, y2) = (
        scan.token::<i64>(),
        scan.token::<i64>(),
        scan.token::<i64>(),
        scan.token::<i64>()
    );
    let (x3, y3, x4, y4) = (
        scan.token::<i64>(),
        scan.token::<i64>(),
        scan.token::<i64>(),
        scan.token::<i64>()
    );

    let abc = ccw(x1, y1, x2, y2, x3, y3);
    let abd = ccw(x1, y1, x2, y2, x4, y4);
    let cda = ccw(x3, y3, x4, y4, x1, y1);
    let cdb = ccw(x3, y3, x4, y4, x2, y2);
    
    if abc * abd <= 0 && cda * cdb <= 0 { println!("1"); }
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
