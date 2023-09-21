use std::io::Write;
use std::{io, cmp, str};

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

struct TarjanUtil<'a> {
    graph: &'a Vec<Vec<usize>>,
    id: usize,
    stack: Vec<usize>,
    index: Vec<usize>,
    low_link: Vec<usize>,
    on_stack: Vec<bool>,
    num_scc: usize,
    scc: Vec<Vec<usize>>,
    gid: usize,
    g_index: Vec<usize>,
}

impl<'a> TarjanUtil<'a> {
    fn new(v: usize, graph: &'a Vec<Vec<usize>>) -> Self {
        Self {
            graph,
            id: 1,
            stack: vec![],
            index: vec![0; v + 1],
            low_link: vec![0; v + 1],
            on_stack: vec![false; v + 1],
            num_scc: 0,
            scc: vec![],
            gid: 0,
            g_index: vec![0; v + 1],
        }
    }
}

fn strong_connect(util: &mut TarjanUtil, cur: usize) {
    util.index[cur] = util.id;
    util.low_link[cur] = util.id;
    util.id += 1;
    util.stack.push(cur);
    util.on_stack[cur] = true;

    for &next in util.graph[cur].iter() {
        if util.index[next] == 0 {
            // Successor next has not yet been visited; recurse on it.
            strong_connect(util, next);
            util.low_link[cur] = cmp::min(util.low_link[cur], util.low_link[next]);
        }
        else if util.on_stack[next] {
            // Successor next is in the current scc.
            util.low_link[cur] = cmp::min(util.low_link[cur], util.index[next]);
        }
    }

    // If cur is the root node, pop the stack and generate scc of it.
    if util.low_link[cur] == util.index[cur] {
        let mut cur_group = vec![];
        while let Some(node) = util.stack.pop() {
            util.on_stack[node] = false;
            cur_group.push(node);
            util.g_index[node] = util.gid;
            if cur == node { break; }
        }
        cur_group.sort_unstable();
        util.scc.push(cur_group);
        util.num_scc += 1;
        util.gid += 1;
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let t = scan.token::<usize>();

    for _ in 0..t {
        let (n, m) = (scan.token::<usize>(), scan.token::<usize>());

        let mut graph = vec![vec![]; n + 1];

        for _ in 0..m {
            let (x, y) = (scan.token::<usize>(), scan.token::<usize>());
            graph[x + 1].push(y + 1);
        }

        let mut util = TarjanUtil::new(n, &mut graph);

        for i in 1..=n {
            if util.index[i] == 0 {
                strong_connect(&mut util, i);
            }
        }

        let mut in_degree = vec![0; util.num_scc];

        for cur in 1..=n {
            for &next in util.graph[cur].iter() {
                if util.g_index[cur] != util.g_index[next] {
                    in_degree[util.g_index[next]] += 1;
                }
            }
        }

        let mut num_zid = 0;
        let mut idx_zid = 0;

        for i in 0..util.num_scc {
            if in_degree[i] == 0 {
                num_zid += 1;
                idx_zid = i;
            }
        }

        if num_zid == 1 {
            for &node in util.scc[idx_zid].iter() {
                writeln!(out, "{}", node - 1).unwrap();
            }
        } else {
            writeln!(out, "Confused").unwrap();
        }

        writeln!(out).unwrap();
    }
}