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

    fn token<T: str::FromStr>(&mut self) -> Option<T> {
        loop {
            if let Some(token) = self.buf_iter.next() {
                return token.parse().ok();
            }
            self.buf_str.clear();
            if self.reader
                .read_until(b'\n', &mut self.buf_str)
                .expect("Failed read")  == 0 {
                return None;
            }
            self.buf_iter = unsafe {
                let slice = str::from_utf8_unchecked(&self.buf_str);
                std::mem::transmute(slice.split_ascii_whitespace())
            }
        }
    }
}

struct Solver {
    graph: Vec<Vec<usize>>,
    weights: Vec<i32>,
    dp: Vec<Vec<i32>>,
    visited: Vec<bool>,
    route: Vec<usize>,
}

impl Solver {
    fn new(n: usize) -> Self {
        Self {
            graph: vec![vec![]; n + 1],
            weights: vec![-1; n + 1],
            dp: vec![vec![0; 2]; n + 1],
            visited: vec![false; n + 1],
            route: vec![],
        }
    }

    fn dfs(&mut self, node: usize) {
        // The case including `node` to independent set.
        self.dp[node][0] = self.weights[node];
        // The case excluding `node` to independent set.
        self.dp[node][1] = 0;

        self.visited[node] = true;

        for i in 0..self.graph[node].len() {
            let next = self.graph[node][i];
            if !self.visited[next] {
                self.dfs(next);
                self.dp[node][0] += self.dp[next][1];
                self.dp[node][1] += cmp::max(self.dp[next][0], self.dp[next][1]);
            }
        }
    }

    fn trace(&mut self, node: usize, parent: usize) {
        self.visited.fill(false);
        self.trace_route(node, parent);
        self.route.sort_unstable();
    }

    fn trace_route(&mut self, node: usize, parent: usize) {
        if self.dp[node][1] < self.dp[node][0] && !self.visited[parent] {
            self.route.push(node);
            self.visited[node] = true;
        }
        for i in 0..self.graph[node].len() {
            let next = self.graph[node][i];
            if next != parent {
                self.trace_route(next, node);
            }
        }
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let n = scan.token::<usize>().unwrap();

    let mut sv = Solver::new(n + 1);

    for i in 1..=n {
        sv.weights[i] = scan.token::<i32>().unwrap();
    }

    while let Some(a) = scan.token::<usize>() {
        let b = scan.token::<usize>().unwrap();
        sv.graph[a].push(b);
        sv.graph[b].push(a);
    }

    sv.dfs(1);
    writeln!(out, "{}", cmp::max(sv.dp[1][0], sv.dp[1][1])).unwrap();
    sv.trace(1, 0);
    for i in 0..sv.route.len() {
        write!(out, "{} ", sv.route[i]).unwrap();
    }
}