use crate::scanner::UnsafeScanner;
use std::{cmp, io};

const INF: i32 = 1_000_000_000 + 1;

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let n = scan.token::<usize>();

    let mut lines = Vec::with_capacity(n);
    for _ in 0..n {
        let (x, y) = (scan.token::<i32>(), scan.token::<i32>());
        lines.push((x, y));
    }
    lines.sort_unstable();

    let (mut tot_len, mut l, mut r) = (0, -INF, -INF);
    for &p in lines.iter() {
        if r < p.0 {
            tot_len += r - l;
            (l, r) = (p.0, p.1);
        }
        else {
            r = cmp::max(r, p.1);
        }
    }
    tot_len += r - l;

    println!("{tot_len}");
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