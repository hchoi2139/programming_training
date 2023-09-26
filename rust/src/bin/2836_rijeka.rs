use crate::scanner::UnsafeScanner;
use std::{cmp, io};

const INF: u32 = 1_000_000_001;

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let (n, for_tot_len) = (scan.token::<usize>(), scan.token::<u32>());

    let mut backwards = Vec::with_capacity(n);
    for _ in 0..n {
        let (from, to) = (scan.token::<u32>(), scan.token::<u32>());
        if from > to { backwards.push((from, to)); }
    }
    backwards.sort_unstable_by(|a, b| b.cmp(a));

    let (mut back_tot_len, mut l, mut r) = (0, INF, INF);
    for &(from, to) in backwards.iter() {
        if from < l {
            back_tot_len += r - l;
            (l, r) = (to, from);
        }
        else {
            l = cmp::min(l, to)
        }
    }
    back_tot_len += r - l;

    println!("{}", for_tot_len + 2 * back_tot_len);
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