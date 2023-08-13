use io::Write;
use std::{io, str, cmp};

const INF: i64 = 400 * 400 * 10_000 + 1;

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

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (v, e) = (scan.token::<usize>(), scan.token::<usize>());

    let mut adj = vec![vec![INF; v + 1]; v + 1];
    for _ in 0..e {
        let (a, b, c) = (
            scan.token::<usize>(),
            scan.token::<usize>(),
            scan.token::<i64>(),
        );
        adj[a][b] = c;
    }

    for k in 1..=v {
        for i in 1..=v {
            for j in 1..=v {
                adj[i][j] = cmp::min(adj[i][j], adj[i][k] + adj[k][j]);
            }
        }
    }

    let mut res = INF;
    for i in 1..=v {
        res = cmp::min(res, adj[i][i]);
    }

    writeln!(out, "{}", if res == INF { -1 } else { res }).unwrap();
}