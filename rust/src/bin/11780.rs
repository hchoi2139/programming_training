use std::io::Write;
use std::{io, str, cmp};

const INF: i64 = 100_000 * 100_000 + 1;

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

fn find_route(route: &mut Vec<usize>, via: &Vec<Vec<usize>>, start: usize, end: usize) {
    if via[start][end] == 0 {
        route.push(start);
        route.push(end);
        return;
    }
    let via_city = via[start][end];
    find_route(route, via, start, via_city);
    route.pop();
    find_route(route, via, via_city, end);
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (n, m) = (scan.token::<usize>(), scan.token::<usize>());

    let mut adj = vec![vec![INF; n + 1]; n + 1];
    let mut via = vec![vec![0; n + 1]; n + 1];
    for _ in 0..m {
        let (a, b, c) = (
            scan.token::<usize>(), 
            scan.token::<usize>(), 
            scan.token::<i64>()
        );
        adj[a][b] = cmp::min(adj[a][b], c);
    }
    for i in 1..=n {
        adj[i][i] = 0;
    }

    for k in 1..=n {
        for i in 1..=n {
            for j in 1..=n {
                if adj[i][k] + adj[k][j] < adj[i][j] {
                    adj[i][j] = adj[i][k] + adj[k][j];
                    via[i][j] = k;
                }
            }
        }
    }

    for i in 1..=n {
        for j in 1..=n {
            if adj[i][j] == INF {
                adj[i][j] = 0;
                write!(out, "0 ").unwrap();
            } else {
                write!(out, "{} ", adj[i][j]).unwrap();
            }
        }
        writeln!(out).unwrap();
    }

    for i in 1..=n {
        for j in 1..=n {
            if adj[i][j] == 0 {
                writeln!(out, "0").unwrap();
            } else {
                let mut route = vec![];
                find_route(&mut route, &via, i, j);

                write!(out, "{} ", route.len()).unwrap();
                for &e in route.iter() {
                    write!(out, "{} ", e).unwrap();
                }
                writeln!(out).unwrap();
            }
        }
    }
}