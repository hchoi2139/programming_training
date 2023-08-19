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
    tree: &'a Vec<Vec<(usize, i32)>>,
    visited: Vec<bool>,
    max_node: usize,
    max_dist: i32,
}

impl<'a> Solver<'a> {
    fn new(v: usize, tree: &'a Vec<Vec<(usize, i32)>>) -> Self {
        Self { 
            tree, 
            visited: vec![false; v + 1], 
            max_node: 0, 
            max_dist: 0 
        }
    }

    fn find_max_node_n_dist(
        &mut self, 
        node: usize, 
        dist: i32,
    ) -> (usize, i32) {
        self.visited.fill(false);
        self.max_node = 0;
        self.max_dist = 0;

        self.dfs(node, dist);

        (self.max_node, self.max_dist)
    }

    fn dfs(&mut self, node: usize, dist: i32) {
        if self.visited[node] {
            return;
        }

        if self.max_dist < dist {
            self.max_dist = dist;
            self.max_node = node;
        }

        self.visited[node] = true;
        
        for &(nn, d) in self.tree[node].iter() {
            self.dfs(nn, dist + d);
        }
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let v = scan.token::<usize>();

    let mut tree = vec![vec![]; v + 1];
    
    for _ in 0..v {
        let i = scan.token::<usize>();
        loop {
            let j = scan.token::<i32>();
            if j == -1 {
                break;
            }
            let dist = scan.token::<i32>();
            tree[i].push((j as usize, dist));
            tree[j as usize].push((i, dist));
        }
    }

    let mut solver = Solver::new(v, &tree);
    let (max_node, _) = solver.find_max_node_n_dist(1, 0);
    let (_, ans) = solver.find_max_node_n_dist(max_node, 0);
    writeln!(out, "{ans}").unwrap();
}