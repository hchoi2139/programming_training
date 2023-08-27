use std::io::Write;
use std::{io, str, cmp};

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

struct Solver {
    graph: Vec<Vec<usize>>,
    dp: Vec<Vec<i32>>,
    vis: Vec<bool>,
}

impl Solver {
    fn new(n: usize) -> Self {
        Self {
            graph: vec![vec![]; n + 1],
            dp: vec![vec![-1; 2]; n + 1],
            vis: vec![false; n + 1],
        }
    }

    fn solve(&mut self, node: usize) {
        // Init. The case `node` is not an early adapter.
        self.dp[node][0] = 0;
        // Init. The case `node` is early adapter.
        self.dp[node][1] = 1;

        self.vis[node] = true;

        for i in 0..self.graph[node].len() {
            let next = self.graph[node][i];
            if !self.vis[next] {
                self.solve(next);
                self.dp[node][0] += self.dp[next][1];
                self.dp[node][1] += cmp::min(self.dp[next][0], self.dp[next][1]);
            }
        }
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let n = scan.token::<usize>();

    let mut sv = Solver::new(n);

    for _ in 1..n {
        let (u, v) = (scan.token::<usize>(), scan.token::<usize>());
        sv.graph[u].push(v);
        sv.graph[v].push(u);
    }

    // We assume that the root node is 1.
    sv.solve(1);
    write!(out, "{}", cmp::min(sv.dp[1][0], sv.dp[1][1])).unwrap();
}