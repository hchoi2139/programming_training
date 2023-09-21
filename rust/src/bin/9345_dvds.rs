use std::io::Write;
use std::{cmp, io, str};

const INF: u32 = 100_000;

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
    arr: Vec<u32>,
    min_tree: Vec<u32>,
    max_tree: Vec<u32>,
}

impl SegmentTree {
    fn new(n: usize, arr: Vec<u32>) -> Self {
        let height = f64::from(n as u32).log2().ceil() as u32;
        Self {
            n,
            arr,
            min_tree: vec![0; 1 << (height + 1)],
            max_tree: vec![0; 1 << (height + 1)],
        }
    }

    fn init(&mut self) {
        self.build(1, 0, self.n - 1);
    }

    fn build(
        &mut self,
        node: usize,
        s: usize,
        e: usize
    ) {
        self.min_tree[node] = self.arr[s];
        self.max_tree[node] = self.arr[e];
        if s == e { return; }
        let (m, lt, rt) = ((s + e) / 2, 2 * node, 2 * node + 1);
        self.build(lt, s, m);
        self.build(rt, m + 1, e);
    }

    fn min_max_of_interval(&mut self, l: usize, r: usize) -> (u32, u32) {
        self.mmi_dfs(1, 0, self.n - 1, l, r)
    }

    fn mmi_dfs(
        &mut self,
        node: usize,
        s: usize,
        e: usize,
        l: usize,
        r: usize
    ) -> (u32, u32) {
        if l > e || r < s { return (INF, 0) }
        if l <= s && e <= r { return (self.min_tree[node], self.max_tree[node]); }
        let (m, lt, rt) = ((s + e) / 2, 2 * node, 2 * node + 1);
        let (l_min, l_max) = self.mmi_dfs(lt, s, m, l, r);
        let (r_min, r_max) = self.mmi_dfs(rt, m + 1, e, l, r);
        (cmp::min(l_min, r_min), cmp::max(l_max, r_max))
    }

    fn update(&mut self, idx: usize, new_val: u32) {
        self.arr[idx] = new_val;
        self.update_dfs(1, 0, self.n - 1, idx);
    }

    fn update_dfs(
        &mut self,
        node: usize,
        s: usize,
        e: usize,
        idx: usize,
    ) -> (u32, u32) {
        if idx < s || e < idx {  return (self.min_tree[node], self.max_tree[node]); }
        if s == e {
            self.min_tree[node] = self.arr[s];
            self.max_tree[node] = self.arr[s];
            return (self.min_tree[node], self.max_tree[node]);
        }
        let (m, lt, rt) = ((s + e) / 2, 2 * node, 2 * node + 1);
        let (l_min, l_max) = self.update_dfs(lt, s, m, idx);
        let (r_min, r_max) = self.update_dfs(rt, m + 1, e, idx);
        self.min_tree[node] = cmp::min(l_min, r_min);
        self.max_tree[node] = cmp::max(l_max, r_max);
        (self.min_tree[node], self.max_tree[node])
    }

    fn swap(&mut self, ia: usize, ib: usize) {
        let (va, vb) = (self.arr[ia], self.arr[ib]);
        self.update(ia, vb);
        self.update(ib, va);
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let t = scan.token::<u32>();

    for _ in 0..t {
        let (n, k) = (scan.token::<u32>(), scan.token::<u32>());

        let arr: Vec<u32> = (0..n).collect();
        let n = n as usize;

        let mut st = SegmentTree::new(n, arr);
        st.init();

        for _ in 0..k {
            let (q, a, b) = (
                scan.token::<u8>(),
                scan.token::<usize>(),
                scan.token::<usize>(),
            );

            match q {
                0 => st.swap(a, b),
                1 => {
                    let min_max_dvd = st.min_max_of_interval(a, b);
                    if min_max_dvd == (a as u32, b as u32) {
                        writeln!(out, "YES").unwrap();
                    } else {
                        writeln!(out, "NO").unwrap();
                    }
                },
                _ => unreachable!(),
            };
        }
    }
}