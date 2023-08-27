use std::io::Write;
use std::{io, str};

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

struct Solver<'a> {
    graph: &'a Vec<Vec<usize>>,
    size: Vec<u32>,
    visited: Vec<bool>,
}

impl<'a> Solver<'a> {
    fn new(n: usize, graph: &'a Vec<Vec<usize>>) -> Self {
        Self { 
            graph,
            size: vec![1; n + 1],
            visited: vec![false; n + 1],
        }
    }

    fn dfs(&mut self, node: usize, parent: usize) {
        self.visited[node] = true;
        for &next in &self.graph[node] {
            if !self.visited[next] {
                self.dfs(next, node);
            }
        }
        if parent != 0 {
            self.size[parent] += self.size[node];
        }
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (n, r, q) = (
        scan.token::<usize>(),
        scan.token::<usize>(),
        scan.token::<usize>(),
    );

    let mut graph = vec![vec![]; n + 1];

    for _ in 0..(n - 1) {
        let (u, v) = (scan.token::<usize>(), scan.token::<usize>());
        graph[u].push(v);
        graph[v].push(u);
    }

    let mut solver = Solver::new(n, &graph);
    solver.dfs(r, 0);

    for _ in 0..q {
        writeln!(out, "{}", solver.size[scan.token::<usize>()]).unwrap();
    }
}