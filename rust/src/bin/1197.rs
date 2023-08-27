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

struct Edge {
    cost: i32,
    v1: usize,
    v2: usize,
}

struct UnionFind {
    v:      usize,
    graph:  Vec<Edge>,
    parent: Vec<usize>,
}

impl UnionFind {
    fn new(v: usize) -> Self {
        Self {
            v,
            graph:  vec![],
            parent: (0..=v).collect(),
        }
    }

    fn find(&mut self, x: usize) -> usize {
        let px = self.parent[x];
        if x == px { return x; }
        let root = self.find(px);
        self.parent[x] = root;
        root
    }

    fn union(&mut self, x: usize, y: usize) -> bool {
        let (px, py) = (self.find(x), self.find(y));
        if px != py {
            self.parent[py] = px;
            return true;
        }
        false
    }

    fn mst_cost(&mut self) -> i32 {
        self.graph.sort_unstable_by_key(|k| k.cost);
        let mut n_added = 0;
        let mut i = 0;
        let mut ans = 0;
        while n_added < self.v - 1 {
            let &Edge { cost, v1, v2 } = &self.graph[i];
            let (rv1, rv2) = (self.find(v1), self.find(v2));
            if rv1 != rv2 {
                self.union(v1, v2);
                ans += cost;
                n_added += 1;
            }
            i += 1;
        }
        ans
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (v, e) = (scan.token::<usize>(), scan.token::<usize>());

    let mut uf = UnionFind::new(v);

    for _ in 0..e {
        let (a, b, c) = (
            scan.token::<usize>(),
            scan.token::<usize>(),
            scan.token::<i32>(),
        );
        uf.graph.push(Edge { cost: c, v1: a, v2: b });
    }
    writeln!(out, "{}", uf.mst_cost()).unwrap();
}