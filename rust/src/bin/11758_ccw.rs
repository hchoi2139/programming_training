use std::{io, cmp::Ordering};

use scanner::UnsafeScanner;

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let mut points = Vec::with_capacity(3);
    (0..3).for_each(|_| points.push((scan.token::<i32>(), scan.token::<i32>())));

    // Define a to be a vector from points[0] to points[1], b from points[0] to points[2].
    let a = (points[1].0 - points[0].0, points[1].1 - points[0].1);
    let b = (points[2].0 - points[0].0, points[2].1 - points[0].1);

    let cross_prod = a.0 * b.1 - a.1 * b.0;
    match cross_prod.cmp(&0) {
        Ordering::Less  => println!("-1"),
        Ordering::Equal => println!("0"),
        Ordering::Greater => println!("1"),
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
