use crate::data_structure::SegmentTree;
use crate::scanner::UnsafeScanner;
use std::cmp::Ordering;
use std::io;

struct VEdge {
    x: u32,
    y_bot: u32,
    y_top: u32,
    is_start: bool,
}

impl VEdge {
    fn new(
        x: u32,
        y_bot: u32,
        y_top: u32,
        is_start: bool
    ) -> Self {
        VEdge { x, y_bot, y_top, is_start }
    }

    fn cmp(&self, rhs: &VEdge) -> Ordering {
        self.x.cmp(&rhs.x)
    }
}

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let n = scan.token::<u32>();

    let mut vert_edges = Vec::with_capacity(2 * n as usize);
    let mut y_coords = Vec::with_capacity(2 * n as usize);
    for _ in 0..n {
        let (x1, x2, y1, y2) = (
            scan.token::<u32>(),
            scan.token::<u32>(),
            scan.token::<u32>(),
            scan.token::<u32>(),
        );
        vert_edges.push(VEdge::new(x1, y1, y2, true));
        vert_edges.push(VEdge::new(x2, y1, y2, false));
        y_coords.push(y1);
        y_coords.push(y2);
    }

    // y-coordinate compression.
    y_coords.sort_unstable();
    y_coords.dedup();
    let num_y_coords = y_coords.len();

    vert_edges.sort_unstable_by(|a, b| a.cmp(b));

    // Update seg_tree with first element of vert_edges.
    let mut seg_tree = SegmentTree::new(num_y_coords);
    seg_tree.insert(
        &y_coords,
        y_coords.binary_search(&vert_edges[0].y_bot).unwrap(),
        y_coords.binary_search(&vert_edges[0].y_top).unwrap() - 1,
    );

    let (mut ans, mut prev_x) = (0, vert_edges[0].x);
    for e in vert_edges.iter().skip(1) {
        let (width, height) = (u64::from(e.x - prev_x), u64::from(seg_tree.get_cur_height()));
        ans += width * height;
        prev_x = e.x;
        if e.is_start {
            seg_tree.insert(
                &y_coords,
                y_coords.binary_search(&e.y_bot).unwrap(),
                y_coords.binary_search(&e.y_top).unwrap() - 1,
            );
        }
        else {
            seg_tree.remove(
                &y_coords,
                y_coords.binary_search(&e.y_bot).unwrap(),
                y_coords.binary_search(&e.y_top).unwrap() - 1,
            )
        }
    }

    println!("{ans}");
}

mod data_structure {
    const ROOT_IDX: usize = 1;

    #[derive(Copy, Clone)]
    struct HCnt {
        height: u32,
        cnt: u32,
    }

    impl HCnt {
        fn zero() -> Self {
            Self { height: 0, cnt: 0 }
        }
    }


    pub(crate) struct SegmentTree {
        n: usize,
        tree: Vec<HCnt>,
    }

    impl SegmentTree {
        pub(crate) fn new(n: usize) -> Self {
            let height = f64::from(n as u32).log2().ceil() as u32;
            Self {
                n,
                tree: vec![HCnt::zero(); 1 << (height + 1)],
            }
        }

        pub(crate) fn get_cur_height(&self) -> u32 {
            self.tree[ROOT_IDX].height
        }

        pub(crate) fn insert(&mut self, heights: &Vec<u32>, l: usize, r: usize) {
            self.insert_helper(ROOT_IDX, 0, self.n - 1, l, r, true, heights);
        }

        pub(crate) fn remove(&mut self, heights: &Vec<u32>, l: usize, r: usize) {
            self.insert_helper(ROOT_IDX, 0, self.n - 1, l, r, false, heights);
        }

        fn insert_helper(
            &mut self,
            node: usize,
            s: usize,
            e: usize,
            l: usize,
            r: usize,
            is_insert: bool,
            heights: &Vec<u32>,
        ) {
            if r < s || e < l { return; }
            let (m, lt, rt) = ((s + e) / 2, 2 * node, 2 * node + 1);
            if l <= s && e <= r {
                if is_insert { self.tree[node].cnt += 1; }
                else { self.tree[node].cnt -= 1; }
            }
            else {
                self.insert_helper(lt, s, m, l, r, is_insert, heights);
                self.insert_helper(rt, m + 1, e, l, r, is_insert, heights);
            }
            if self.tree[node].cnt == 0 {
                if s == e { self.tree[node].height = 0; }
                else { self.tree[node].height = self.tree[lt].height + self.tree[rt].height; }
            }
            else { self.tree[node].height = heights[e + 1] - heights[s]; }
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