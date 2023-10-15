use std::io;

use algorithm::matching::BipartiteMatching;
use scanner::UnsafeScanner;

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let (n, m) = (scan.token::<usize>(), scan.token::<usize>());

    let mut tasks = vec![Vec::with_capacity(m); n + 1];
    tasks.iter_mut().take(n + 1).skip(1).for_each(|t| {
        let mi = scan.token::<usize>();
        for _ in 0..mi {
            t.push(scan.token::<usize>());
        }
    });

    let mut bm = BipartiteMatching::new(n, m, tasks);
    println!("{}", bm.bipartite_match());
}

mod algorithm {
    pub(crate) mod matching {
        pub(crate) struct BipartiteMatching {
            n: usize,   // Number of elements of the left group.
            m: usize,   // Number of elements of the right group.
            graph: Vec<Vec<usize>>,     // Bipartite relationship of left to right group.
                                        // graph[i] should have a list of connected elements of the
                                        // right group corresponding to i of left group.
            left: Vec<usize>,   // Left group of bipartite graph.
                                // left[i]: An element of right group that is matched to i-th
                                // element of left group.
            right: Vec<usize>,  // Right group of bipartite graph.
                                // right[i]: An element of left group that is matched to i-th
                                // element of right group.
            visited: Vec<bool>, // visited[i]: In an ongoing dfs, was i-th node of right group
                                // considered to be matched?
        }

        impl BipartiteMatching {
            pub(crate) fn new(
                n: usize,
                m: usize,
                graph: Vec<Vec<usize>>,
            ) -> Self {
                Self {
                    n,
                    m,
                    graph,
                    left: vec![0; n + 1],
                    right: vec![0; m + 1],
                    visited: vec![false; n + 1],
                }
            }

            pub(crate) fn bipartite_match(&mut self) -> i32 {
                let mut ans = 0;

                (1..=self.n).for_each(|i| {
                    self.visited.fill(false);
                    if self.dfs(i) { ans += 1; }
                });

                ans
            }

            fn dfs(&mut self, x: usize) -> bool {
                if self.visited[x] { return false; }
                self.visited[x] = true;

                for i in 0..self.graph[x].len() {
                    let e = self.graph[x][i];
                    if self.right[e] == 0 || self.dfs(self.right[e]) {
                        self.right[e] = x;
                        return true;
                    }
                }
                
                false
            }
        }
    }
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
