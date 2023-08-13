use io::Write;
use std::{io, str};

const INF: i64 = 500 * 6_000 * 10_000 + 1;

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

    let (n, m) = (scan.token::<usize>(), scan.token::<usize>());

    let mut graph = vec![vec![]; n + 1];
    for _ in 0..m {
        let (a, b, c) = (
            scan.token::<usize>(),
            scan.token::<usize>(),
            scan.token::<i64>(),
        );
        graph[a].push((b, c));
    }

    let mut dist = vec![INF; n + 1];

    let mut has_neg_cycle = false;

    dist[1] = 0;
    for i in 1..=n {
        for j in 1..=n {
            for &(next_node, add_cost) in graph[j].iter() {
                if dist[j] != INF && dist[next_node] > dist[j] + add_cost {
                    dist[next_node] = dist[j] + add_cost;
                    if i == n {
                        has_neg_cycle = true;
                        break;
                    }
                }
            }
        }
    }

    if has_neg_cycle {
        writeln!(out, "-1").unwrap();
    } else {
        for i in 2..=n {
            if dist[i] == INF {
                writeln!(out, "-1").unwrap();
            } else {
                writeln!(out, "{}", dist[i]).unwrap();
            }
        }
    }
}