use crate::data_structure::SegmentTree;
use crate::scanner::UnsafeScanner;
use std::io::{self, Write};

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (n, k) = (scan.token::<u32>(), scan.token::<u32>());

    let mut seg_tree = SegmentTree::new(n);
    seg_tree.init();

    let mut index = 1;
    let mut n_ppl = n;

    write!(out, "<").unwrap();

    for i in 1..=n {
        index += k - 1;
        index %= n_ppl;
        if index == 0 { index = n_ppl; }

        let target = seg_tree.kth(index);
        seg_tree.remove(target);

        n_ppl -= 1;

        if i == n {
            write!(out, "{}>", target).unwrap();
        } else {
            write!(out, "{}, ", target).unwrap();
        }
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

        pub(crate) fn init(&mut self) {
            self.build(1, 1, self.n);
        }

        fn build(
            &mut self,
            node: usize,
            s: u32,
            e: u32,
        ) -> u32 {
            if s == e {
                self.tree[node] = 1;
                return 1;
            }
            let (m, lt, rt) = ((s + e) / 2, 2 * node, 2 * node + 1);
            self.tree[node] = self.build(lt, s, m) + self.build(rt, m + 1, e);
            self.tree[node]
        }

        pub(crate) fn kth(&self, k: u32) -> u32 {
            self.kth_helper(1, 1, self.n, k)
        }

        fn kth_helper(
            &self,
            node: usize,
            s: u32,
            e: u32,
            idx: u32,
        ) -> u32 {
            if s == e { return s; }
            let (m, lt, rt) = ((s + e) / 2, 2 * node, 2 * node + 1);
            if idx <= self.tree[lt] {
                self.kth_helper(lt, s, m, idx)
            } else {
                self.kth_helper(rt, m + 1, e, idx - self.tree[lt])
            }
        }

        pub(crate) fn remove(&mut self, num: u32) {
            self.remove_helper(1, 1, self.n, num);
        }

        fn remove_helper(
            &mut self,
            node: usize,
            s: u32,
            e: u32,
            num: u32
        ) {
            self.tree[node] -= 1;
            if s == e { return; }
            let (m, lt, rt) = ((s + e) / 2, 2 * node, 2 * node + 1);
            if num <= m {
                self.remove_helper(lt, s, m, num);
            } else {
                self.remove_helper(rt, m + 1, e, num);
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