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
            if cur == node { break; }
        }
        cur_group.sort_unstable();
        util.scc.push(cur_group);
        util.num_scc += 1;
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (v, e) = (scan.token::<usize>(), scan.token::<usize>());

    let mut graph = vec![vec![]; v + 1];

    for _ in 0..e {
        let (a, b) = (scan.token::<usize>(), scan.token::<usize>());
        graph[a].push(b);
    }

    let mut util = TarjanUtil::new(v, &mut graph);

    for i in 1..=v {
        if util.index[i] == 0 {
            strong_connect(&mut util, i);
        }
    }
    util.scc.sort_unstable();

    writeln!(out, "{}", util.num_scc).unwrap();
    for i in 0..util.num_scc {
        for &comp in util.scc[i].iter() {
            write!(out, "{} ", comp).unwrap();
        }
        write!(out, "-1").unwrap();
        writeln!(out).unwrap();
    }
}