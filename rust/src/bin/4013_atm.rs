use crate::algorithm::scc::SCC;
use crate::algorithm::sort::TopologySort;
use crate::scanner::UnsafeScanner;
use std::{cmp, io};

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let (n, m) = (scan.token::<usize>(), scan.token::<u32>());

    let mut graph = vec![vec![]; n + 1];
    for _ in 0..m {
        let (from, to) = (scan.token::<usize>(), scan.token::<usize>());
        graph[from].push(to);
    }

    let mut cash = vec![0; n + 1];
    for i in 1..=n {
        cash[i] = scan.token::<u32>();
    }

    let (s, p) = (scan.token::<usize>(), scan.token::<usize>());

    let mut scc = SCC::new(n);
    scc.tarjan(&graph, &cash);

    let mut scc_restaurants = vec![0; p];
    for i in 0..p {
        scc_restaurants[i] = scc.index[scan.token::<usize>()];
    }

    let mut sorter = TopologySort::new(scc.num_scc, &scc.cash, &scc.graph, &mut scc.in_degree);
    sorter.topology_sort_with_dp(scc.index[s]);

    let mut ans = 0;
    for &node in scc_restaurants.iter() {
        ans = cmp::max(ans, sorter.dp[node]);
    }

    println!("{ans}");
}

mod algorithm {
    pub mod sort {
        use std::cmp;
        use std::collections::VecDeque;

        pub(crate) struct TopologySort<'a> {
            // Given info.
            num_vs: usize,
            cash: &'a Vec<u32>,
            graph: &'a Vec<Vec<usize>>,
            in_degree: &'a mut Vec<u32>,

            // Info to be found.
            pub(crate) dp: Vec<u32>,
        }

        impl<'a> TopologySort<'a> {
            pub(crate) fn new(
                num_vs: usize,
                cash: &'a Vec<u32>,
                graph: &'a Vec<Vec<usize>>,
                in_degree: &'a mut Vec<u32>
            ) -> Self {
                Self {
                    num_vs,
                    cash,
                    graph,
                    in_degree,
                    dp: vec![0; num_vs + 1],
                }
            }

            pub(crate) fn topology_sort_with_dp(&mut self, start: usize) {
                let mut is_reachable = vec![false; self.num_vs + 1];
                is_reachable[start] = true;

                let mut queue = VecDeque::with_capacity(self.num_vs);
                for i in 1..=self.num_vs {
                    if self.in_degree[i] == 0 {
                        queue.push_back(i);
                    }
                }

                self.dp[start] = self.cash[start];

                while let Some(cur) = queue.pop_front() {
                    for &next in self.graph[cur].iter() {
                        if is_reachable[cur] {
                            self.dp[next] = cmp::max(self.dp[next], self.dp[cur] + self.cash[next]);
                            is_reachable[next] = true;
                        }
                        self.in_degree[next] -= 1;
                        if self.in_degree[next] == 0 {
                            queue.push_back(next);
                        }
                    }
                }
            }
        }
    }

    pub mod scc {
        use std::cmp;

        pub(crate) struct SCC {
            // Given info.
            num_vs: usize,

            // Elements needed during strong_connect.
            v_id: usize,
            v_stack: Vec<usize>,
            v_index: Vec<usize>,
            v_low_link: Vec<usize>,
            v_on_stack: Vec<bool>,

            // Info to be found.
            pub(crate) num_scc: usize,        // Also used as scc id during recursion.
            pub(crate) scc: Vec<Vec<usize>>,
            pub(crate) cash: Vec<u32>,
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
                    v_low_link: vec![0; num_vs + 1],
                    v_on_stack: vec![false; num_vs + 1],

                    num_scc: 0,
                    scc: vec![vec![]; num_vs + 1],
                    cash: vec![0; num_vs + 1],
                    index: vec![0; num_vs + 1],
                    graph: vec![vec![]; num_vs + 1],
                    in_degree: vec![0; num_vs + 1],
                }
            }

            pub(crate) fn tarjan(&mut self, graph: &Vec<Vec<usize>>, cash: &Vec<u32>) {
                for i in 1..=self.num_vs {
                    if self.v_index[i] == 0 {
                        self.strong_connect(i, graph, cash);
                    }
                }
                self.build_scc_graph(graph);
                self.resize_scc_members();
            }

            fn resize_scc_members(&mut self) {
                let size_scc = self.num_scc + 1;
                self.scc.truncate(size_scc);
                self.cash.truncate(size_scc);
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

            fn strong_connect(&mut self, cur: usize, graph: &Vec<Vec<usize>>, cash: &Vec<u32>) {
                self.v_index[cur] = self.v_id;
                self.v_low_link[cur] = self.v_id;
                self.v_id += 1;

                self.v_stack.push(cur);
                self.v_on_stack[cur] = true;

                for &next in graph[cur].iter() {
                    if self.v_index[next] == 0 {
                        // Successor next has not yet been visited; recurse on it.
                        self.strong_connect(next, graph, cash);
                        self.v_low_link[cur] = cmp::min(self.v_low_link[cur], self.v_low_link[next]);
                    }
                    else if self.v_on_stack[next] {
                        // Successor next is in the current scc.
                        self.v_low_link[cur] = cmp::min(self.v_low_link[cur], self.v_index[next]);
                    }
                }

                // If cur is in the root node, pop the stack until cur is popped,
                // group them as a single scc group.
                if self.v_low_link[cur] == self.v_index[cur] {
                    self.num_scc += 1;
                    while let Some(node) = self.v_stack.pop() {
                        self.v_on_stack[node] = false;
                        self.scc[self.num_scc].push(node);
                        self.index[node] = self.num_scc;
                        self.cash[self.num_scc] += cash[node];
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