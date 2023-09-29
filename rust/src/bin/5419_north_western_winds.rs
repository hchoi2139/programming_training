use crate::data_structure::SegmentTree;
use crate::scanner::UnsafeScanner;
use std::cmp::Ordering;
use std::io::{self, Write};

const INF: i32 = 1_000_000_000 + 1;

fn cmp_y_rev<U: Ord, T: Ord>(p1: &(U, T), p2: &(U, T)) -> Ordering {
    p2.1.cmp(&p1.1)
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let t = scan.token::<u32>();

    for _ in 0..t {
        let n = scan.token::<u32>();

        let mut islands = Vec::with_capacity(n as usize);
        for _ in 0..n {
            let (x, y) = (scan.token::<i32>(), scan.token::<i32>());
            islands.push((x, y));
        }

        // y-coordinate compression.
        islands.sort_unstable_by(|a, b| cmp_y_rev(a, b));
        let (mut idx, mut prev) = (-1, -INF);
        for (_, y) in islands.iter_mut() {
            if prev != *y {
                idx += 1;
                prev = *y;
            }
            *y = idx;
        }

        islands.sort_unstable();

        let mut seg_tree = SegmentTree::new(n);

        let mut ans = 0;
        for &(_, y) in islands.iter() {
            ans += seg_tree.search(0, y as u32);
            seg_tree.insert(y as u32, 1);
        }

        writeln!(out, "{ans}").unwrap();
    }
}

mod data_structure {
    pub(crate) struct SegmentTree {
        n: u32,
        pub(crate) tree: Vec<u32>,
    }

    impl SegmentTree {
        pub(crate) fn new(n: u32) -> Self {
            let height = f64::from(n).log2().ceil() as u32;
            Self {
                n,
                tree: vec![0; 1 << (height + 1)],
            }
        }

        pub(crate) fn search(&self, l: u32, r: u32) -> u32 {
            self.search_helper(1, 0, self.n - 1, l, r)
        }

        fn search_helper(&self, node: usize, s: u32, e: u32, l: u32, r: u32) -> u32 {
            if e < l || r < s { return 0; }
            if l <= s && e <= r { return self.tree[node]; }
            let (m, lt, rt) = ((s + e) / 2, 2 * node, 2 * node + 1);
            self.search_helper(lt, s, m, l, r) + self.search_helper(rt, m + 1, e, l, r)
        }

        pub(crate) fn insert(&mut self, idx: u32, val: u32) {
            self.insert_helper(1, 0, self.n - 1, idx, val);
        }

        fn insert_helper(&mut self, node: usize, s: u32, e: u32, idx: u32, val: u32) {
            if idx < s || e < idx { return; }
            self.tree[node] += val;
            if s == e { return; }
            let (m, lt, rt) = ((s + e) / 2, 2 * node, 2 * node + 1);
            self.insert_helper(lt, s, m, idx, val);
            self.insert_helper(rt, m + 1, e, idx, val);
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