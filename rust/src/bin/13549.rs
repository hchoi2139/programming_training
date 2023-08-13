use io::Write;
use std::{collections::VecDeque, io, str};

const MAX: usize = 100_000;

pub struct UnsafeScanner<R> {
    reader: R,
    buf_str: Vec<u8>,
    buf_iter: str::SplitAsciiWhitespace<'static>,
}

impl<R: io::BufRead> UnsafeScanner<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            buf_str: vec![],
            buf_iter: "".split_ascii_whitespace(),
        }
    }

    pub fn token<T: str::FromStr>(&mut self) -> T {
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

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (n, k) = (scan.token::<usize>(), scan.token::<usize>());

    let mut dist = vec![-1; MAX + 1];
    let mut queue = VecDeque::new();
    queue.push_back(n);
    dist[n] = 0;

    while let Some(t) = queue.pop_front() {
        if t == k {
            break;
        }

        if t * 2 <= MAX && dist[t * 2] < 0 {
            queue.push_front(t * 2);
            dist[t * 2] = dist[t];
        }

        if t > 0 && dist[t - 1] < 0 {
            queue.push_back(t - 1);
            dist[t - 1] = dist[t] + 1;
        }

        if t < MAX && dist[t + 1] < 0 {
            queue.push_back(t + 1);
            dist[t + 1] = dist[t] + 1;
        }
    }

    writeln!(out, "{}", dist[k]).unwrap();
}