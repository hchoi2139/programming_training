use std::io::Write;
use std::{io, str};

const MOD: i64 = 1_000_000_007;

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

struct SegmentTree {
    n: usize,
    arr: Vec<i64>,
    tree: Vec<i64>,
}

impl SegmentTree {
    fn new(n: usize, arr: Vec<i64>) -> Self {
        let height = f64::from(n as u32).log2().ceil() as u32;
        Self {
            n,
            arr,
            tree: vec![0; 1 << (height + 1)],
        }
    }

    fn init(&mut self) -> i64 {
        self.build(1, 0, self.n - 1)
    }

    fn build(&mut self, node: usize, s: usize, e: usize) -> i64 {
        if s == e {
            self.tree[node] = self.arr[s];
            return self.tree[node]
        }
        let m = (s + e) / 2;
        let (lt, rt) = (2 * node, 2 * node + 1);
        self.tree[node] = (self.build(lt, s, m) * self.build(rt, m + 1, e)) % MOD;
        self.tree[node]
    }

    fn product(&mut self, l: usize, r: usize) -> i64 {
        self.prod_dfs(1, l - 1, r - 1, 0, self.n - 1)
    }

    fn prod_dfs(&mut self, node: usize, l: usize, r: usize, s: usize, e: usize) -> i64 {
        if l > e || r < s { return 1; }
        if l <= s && e <= r { return self.tree[node]; }
        let m = (s + e) / 2;
        let (lt, rt) = (2 * node, 2 * node + 1);
        (self.prod_dfs(lt, l, r, s, m) * self.prod_dfs(rt, l, r, m + 1, e)) % MOD
    }

    fn update(&mut self, idx: usize, new_val: i64) {
        let idx = idx - 1;
        self.arr[idx] = new_val;
        self.update_dfs(idx, 1, 0, self.n - 1);
    }

    fn update_dfs(
        &mut self,
        idx: usize,
        node: usize,
        s: usize,
        e: usize
    ) -> i64 {
        if idx < s || e < idx { return self.tree[node]; }
        if s == e {
            self.tree[node] = self.arr[s];
            return self.tree[node];
        }
        let m = (s + e) / 2;
        let (lt, rt) = (2 * node, 2 * node + 1);
        self.tree[node] = (self.update_dfs(idx, lt, s, m) * self.update_dfs(idx, rt, m + 1, e)) % MOD;
        self.tree[node]
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (n, m, k) = (
        scan.token::<usize>(),
        scan.token::<usize>(),
        scan.token::<usize>()
    );

    let mut arr = vec![0; n];
    for i in 0..n {
        arr[i] = scan.token::<i64>();
    }

    let mut seg_tree = SegmentTree::new(n, arr);
    seg_tree.init();

    for _ in 0..(m + k) {
        let (a, b) = (scan.token::<u8>(), scan.token::<usize>());
        match a {
            1 => {
                let c = scan.token::<i64>();
                seg_tree.update(b, c);
            },
            2 => {
                let c = scan.token::<usize>();
                let prod = seg_tree.product(b, c);
                writeln!(out, "{prod}").unwrap();
            }
            _ => unreachable!(),
        }
    }
}