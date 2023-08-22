use std::io::Write;
use std::{io, str};

const MAX: usize = 1_000_000;

struct UnsafeScanner<R> {
    reader: R,
    buf_str: Vec<u8>,
    buf_iter: str::SplitAsciiWhitespace<'static>,
}

impl <R: io::BufRead> UnsafeScanner<R> {
    fn new(reader: R) -> Self {
        Self {
            reader,
            buf_str: vec![],
            buf_iter: "".split_ascii_whitespace(),
        }
    }

    fn token<T: str::FromStr>(&mut self) -> T {
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

fn find_parent(parent: &mut [usize], i: usize) -> usize {
    if parent[i] == i {
        return i;
    }
    parent[i] = find_parent(parent, parent[i]);
    parent[i]
}

fn union_parent(parent: &mut [usize], i: usize, j: usize) {
    let pi = find_parent(parent, i);
    let pj = find_parent(parent, j);
    parent[pj] = pi;
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (n, m) = (scan.token::<usize>(), scan.token::<usize>());

    let mut parent = [0; MAX + 1];
    for i in 1..=n {
        parent[i] = i;
    }

    for _ in 0..m {
        let (op, a, b) = (
            scan.token::<u8>(),
            scan.token::<usize>(),
            scan.token::<usize>(),
        );
        match op {
            0 => union_parent(&mut parent, a, b),
            _ => {
                if find_parent(&mut parent, a) == find_parent(&mut parent, b) {
                    writeln!(out, "YES").unwrap();
                } else {
                    writeln!(out, "NO").unwrap();
                }
            },
        }
    }
}