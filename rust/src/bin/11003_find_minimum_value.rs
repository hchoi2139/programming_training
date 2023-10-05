use std::collections::VecDeque;
use std::io;
use io::Write;
use crate::scanner::UnsafeScanner;

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (n, l) = (scan.token::<u32>(), scan.token::<u32>());

    let mut cand: VecDeque<(i32, u32)> = VecDeque::with_capacity(n as usize);

    // Preprocessing before sliding window, A_0 ~ A_(L - 1).
    for i in 0..l {
        let a = scan.token::<i32>();
        while let Some(p) = cand.pop_back() {
            if p.0 <= a {
                cand.push_back(p);
                break;
            }
        }
        cand.push_back((a, i));
        write!(out, "{} ", cand.front().unwrap().0).unwrap();
    }

    // Sliding window, each step investigates A_(i + 1) ~ A_(i + L), i = 0..(N - L - 1).
    for i in 0..(n - l) {
        let a = scan.token::<i32>();
        if let Some(p) = cand.front() {
            if p.1 == i { cand.pop_front(); }
        }
        while let Some(p) = cand.pop_back() {
            if p.0 <= a {
                cand.push_back(p);
                break;
            }
        }
        cand.push_back((a, i + l));
        write!(out, "{} ", cand.front().unwrap().0).unwrap();
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
