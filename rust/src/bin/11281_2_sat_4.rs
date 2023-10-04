use crate::algorithm::scc::SCC;
use crate::scanner::UnsafeScanner;
use std::io::{self, Write};

#[inline]
fn not(idx: usize, n: usize) -> usize {
    if idx > n { idx - n } else { idx + n }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (n, m) = (scan.token::<usize>(), scan.token::<u32>());

    let mut graph = vec![vec![]; 2 * n + 1];
    for _ in 0..m {
        let (from, to) = (scan.token::<i32>(), scan.token::<i32>());
        let (from, to) = (
            if from < 0 { not((-from) as usize, n) } else { from as usize },
            if to < 0 { not((-to) as usize, n) } else { to as usize },
        );
        graph[not(from, n)].push(to);
        graph[not(to, n)].push(from);
    }

    let mut scc = SCC::new(2 * n);
    scc.tarjan(&graph);

    for i in 1..=n {
        if scc.index[i] == scc.index[not(i, n)] {
            writeln!(out, "0").unwrap();
            return;
        }
    }

    writeln!(out, "1").unwrap();

    let mut t_order = Vec::with_capacity(2 * n);
    for i in 1..=(2 * n) {
        t_order.push((scc.index[i], i));
    }
    t_order.sort_unstable_by(|a, b| b.cmp(a));

    let mut ans = vec![-1; n + 1];
    for &(_, cur) in t_order.iter() {
        let (idx, has_neg) = if cur > n { (cur - n, true) } else { (cur, false) };
        if ans[idx] == -1 {
            ans[idx] = if has_neg { 1 } else { 0 };
        }
    }

    for i in 1..=n {
        write!(out, "{} ", ans[i]).unwrap();
    }
}

mod algorithm {
    pub mod scc {
        use std::cmp;

        pub(crate) struct SCC {
            // Given info.
            num_vs: usize,

            // Elements needed during strong_connect.
            v_id: usize,
            v_stack: Vec<usize>,
            v_index: Vec<usize>,
            v_low: Vec<usize>,
            v_on_stack: Vec<bool>,

            // Info to be found.
            pub(crate) num_scc: usize,        // Also used as scc id during recursion.
            pub(crate) scc: Vec<Vec<usize>>,
            pub(crate) index: Vec<usize>,
            pub(crate) graph: Vec<Vec<usize>>,
            pub(crate) in_degree: Vec<u32>,
        }

        impl SCC {
            pub(crate) fn new(num_vs: usize) -> Self {
                Self {
                    num_vs,

                    v_id: 1,
                    v_stack: Vec::with_capacity(num_vs),
                    v_index: vec![0; num_vs + 1],
                    v_low: vec![0; num_vs + 1],
                    v_on_stack: vec![false; num_vs + 1],

                    num_scc: 0,
                    scc: vec![vec![]; num_vs + 1],
                    index: vec![0; num_vs + 1],
                    graph: vec![vec![]; num_vs + 1],
                    in_degree: vec![0; num_vs + 1],
                }
            }

            pub(crate) fn tarjan(&mut self, graph: &Vec<Vec<usize>>) {
                for i in 1..=self.num_vs {
                    if self.v_index[i] == 0 {
                        self.strong_connect(i, graph);
                    }
                }
                self.build_scc_graph(graph);
                self.resize_scc_members();
            }

            fn resize_scc_members(&mut self) {
                let size_scc = self.num_scc + 1;
                self.scc.truncate(size_scc);
                self.graph.truncate(size_scc);
                self.in_degree.truncate(size_scc);
            }

            fn build_scc_graph(&mut self, graph: &Vec<Vec<usize>>) {
                for from in 1..=self.num_vs {
                    for &to in graph[from].iter() {
                        let (from_scc, to_scc) = (self.index[from], self.index[to]);
                        if from_scc != to_scc {
                            self.graph[from_scc].push(to_scc);
                            self.in_degree[to_scc] += 1;
                        }
                    }
                }
            }

            fn strong_connect(&mut self, cur: usize, graph: &Vec<Vec<usize>>) {
                self.v_index[cur] = self.v_id;
                self.v_low[cur] = self.v_id;
                self.v_id += 1;

                self.v_stack.push(cur);
                self.v_on_stack[cur] = true;

                for &next in graph[cur].iter() {
                    if self.v_index[next] == 0 {
                        // Successor next has not yet been visited; recurse on it.
                        self.strong_connect(next, graph);
                        self.v_low[cur] = cmp::min(self.v_low[cur], self.v_low[next]);
                    }
                    else if self.v_on_stack[next] {
                        // Successor next is in the current scc.
                        self.v_low[cur] = cmp::min(self.v_low[cur], self.v_index[next]);
                    }
                }

                // If cur is in the root node, pop the stack until cur is popped,
                // group them as a single scc group.
                if self.v_low[cur] == self.v_index[cur] {
                    self.num_scc += 1;
                    while let Some(node) = self.v_stack.pop() {
                        self.v_on_stack[node] = false;
                        self.scc[self.num_scc].push(node);
                        self.index[node] = self.num_scc;
                        if cur == node { break; }
                    }
                }
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