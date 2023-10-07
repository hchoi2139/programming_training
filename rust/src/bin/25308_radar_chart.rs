use std::io;

use scanner::UnsafeScanner;

struct Solver {
    n: usize,
    n_convex_polygons: u32,
    visited: Vec<bool>,
    perm: Vec<u32>,
}

impl Solver {
    fn new(n: usize) -> Self {
        Self {
            n,
            n_convex_polygons: 0,
            visited: vec![false; n],
            perm: vec![0; n],
        }
    }

    fn is_convex(&self) -> bool {
        let n = self.n;
        for i in 0..8 {
            let (a, b, c) = (i, (i + 1) % n, (i + 2) % n);
            let (a, b, c) = (self.perm[a], self.perm[b], self.perm[c]);
            if f64::from(b * (a + c)) < (2.0f64).sqrt() * f64::from(a * c) { return false; }
        }
        true
    }

    fn dfs(&mut self, cnt: usize, stats: &Vec<u32>) {
        if cnt == self.n {
            self.n_convex_polygons += if self.is_convex() { 1 } else { 0 };
            return;
        }
        for i in 0..8 {
            if self.visited[i] { continue; }
            self.visited[i] = true;
            self.perm[cnt] = stats[i];
            self.dfs(cnt + 1, stats);
            self.visited[i] = false;
        }
    }
}

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let mut stats = vec![0; 8];
    stats.iter_mut().for_each(|v| *v = scan.token::<u32>());

    let mut solver = Solver::new(8);
    solver.dfs(0, &stats);
    println!("{}", solver.n_convex_polygons);
}

mod scanner {
    use std::{io, str};

    pub(crate) struct UnsafeScanner<R> {
        reader: R,
        buf_str: Vec<u8>,
        buf_iter: str::SplitAsciiWhitespace<'static>,
    }

    impl <R: io::BufRead> UnsafeScanner<R> {
        pub(crate) fn new(reader: R) -> Self {
            Self {
                reader,
                buf_str: vec![],
                buf_iter: "".split_ascii_whitespace(),
            }
        }

        pub(crate) fn token<T: str::FromStr>(&mut self) -> T {
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
}
