use crate::data_structure::SegmentTree;
use crate::scanner::UnsafeScanner;
use std::cmp::Ordering;
use std::io;

const INF: i32 = 1_000_000_000 + 1;
const MOD: u64 = 1_000_000_000 + 7;

fn cmp_pair<U: Ord, T: Ord>(p1: &(U, T), p2: &(U, T)) -> Ordering {
    if p1.1 == p2.1 { return p1.0.cmp(&p2.0); }
    p2.1.cmp(&p1.1)
}

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let n = scan.token::<u32>();

    let mut stars = Vec::with_capacity(n as usize);
    for _ in 0..n {
        let (x, y) = (scan.token::<i32>(), scan.token::<i32>());
        stars.push((x, y));
    }

    // x-coordinate compression.
    stars.sort_unstable();
    let (mut compressed_x, mut prev) = (0, -INF);
    for (x, _) in stars.iter_mut() {
        if prev != *x {
            compressed_x += 1;
            prev = *x;
        }
        *x = compressed_x;
    }

    // Sort stars by the order of cmp_pair.
    stars.sort_unstable_by(|a, b| cmp_pair(a, b));

    let x_coords_stars = compressed_x as u32;
    let mut seg_tree = SegmentTree::new(x_coords_stars + 2);

    let (mut ans, mut prev_x, mut prev_y) = (0, -INF, INF);
    let (mut left_stars_same_y, mut visited_stars_same_y) = (0, 0);
    for &(x, y) in stars.iter() {
        if prev_y != y {
            (prev_x, prev_y) = (x, y);
            (left_stars_same_y, visited_stars_same_y) = (0, 0);
        }
        else {
            if prev_x == x {
                left_stars_same_y -= 1;
            }
            else {
                prev_x = x;
                left_stars_same_y = visited_stars_same_y;
            }
        }
        let (tl_stars, tr_stars) = (
            seg_tree.search(0, x as u32 - 1) - left_stars_same_y,
            seg_tree.search(x as u32 + 1, x_coords_stars + 1),
        );
        ans = (ans + (tl_stars as u64) * (tr_stars as u64)) % MOD;
        seg_tree.insert(x as u32, 1);
        left_stars_same_y += 1;
        visited_stars_same_y += 1;
    }

    println!("{ans}");
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