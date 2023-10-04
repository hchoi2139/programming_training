use std::io::Write;
use std::{io, str, collections::VecDeque};

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
    n: usize,
    log2_n: usize,
    tree: Vec<Vec<usize>>,
    parent: Vec<Vec<usize>>,
    depth: Vec<usize>,
}

impl Solver {
    fn new(n: usize, log2_n: usize) ->  Self {
        Self {
            n,
            log2_n,
            tree: vec![vec![]; n + 1],
            parent: vec![vec![0; log2_n]; n + 1],
            depth: vec![0; n + 1],
        }
    }

    fn add_edge_to_tree(&mut self, a: usize, b: usize) {
        self.tree[a].push(b);
        self.tree[b].push(a);
    }

    fn find_parent(&mut self) {
        let mut visited = vec![false; self.n + 1];
        visited[1] = true;

        let mut dq = VecDeque::new();
        dq.push_back(1);

        while let Some(cur) = dq.pop_front() {
            for &next in self.tree[cur].iter() {
                if !visited[next] {
                    visited[next] = true;
                    self.depth[next] = self.depth[cur] + 1;
                    self.parent[next][0] = cur;
                    for i in 0..(self.log2_n - 1) {
                        let nn = self.parent[self.parent[next][i]][i];
                        if nn == 0 {
                            break;
                        } else {
                            self.parent[next][i + 1] = nn;
                        }
                    }
                    dq.push_back(next);
                }
            }
        }
    }

    fn find_lca(&mut self, u: usize, v: usize) -> usize {
        let (mut d, mut s, dd, ds) = {
            let (du, dv) = (self.depth[u], self.depth[v]);
            if du > dv { (u, v, du, dv) } else { (v, u, dv, du) }
        };

        let mut dist = dd - ds;

        let mut i = 0;
        while dist != 0 {
            if dist % 2 == 1 {
                d = self.parent[d][i];
            }
            dist >>= 1;
            i += 1;
        }

        if d != s {
            for i in (0..self.log2_n).rev() {
                if self.parent[d][i] != self.parent[s][i] {
                    d = self.parent[d][i];
                    s = self.parent[s][i];
                }
            }
            d = self.parent[d][0];
        }

        d
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let n = scan.token::<u32>();
    let log2_n = f64::from(n).log2().ceil() as usize;
    let n = n as usize;

    let mut solver = Solver::new(n, log2_n);

    for _ in 1..n {
        let (a, b) = (scan.token::<usize>(), scan.token::<usize>());
        solver.add_edge_to_tree(a, b);
    }

    // Set the root node as 1 and find parents of each node.
    solver.find_parent();

    let m = scan.token::<usize>();

    for _ in 0..m {
        let (u, v) = (scan.token::<usize>(), scan.token::<usize>());
        writeln!(out, "{}", solver.find_lca(u, v)).unwrap();
    }
}