use io::Write;
use std::{io, str, cmp::Ordering};

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

fn dfs(
    left: usize, 
    right: usize, 
    sum: i64, 
    items: &Vec<i64>,
    sums: &mut Vec<i64>
) {
    if left > right { 
        sums.push(sum);
        return;
    }

    dfs(left + 1, right, sum, items, sums);
    dfs(left + 1, right, sum + items[left], items, sums);
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (n, c) = (scan.token::<usize>(), scan.token::<i64>());

    let mut items = vec![];
    items.push(0);
    for _ in 0..n {
        items.push(scan.token::<i64>());
    }
    items.sort_unstable();

    let mut sums_lh = vec![];
    let mut sums_rh = vec![];
    dfs(1, n / 2, 0, &items, &mut sums_lh);
    dfs(n / 2 + 1, n, 0, &items, &mut sums_rh);

    sums_rh.sort_unstable(); 

    let mut ans = 0;
    for &part_sum in sums_lh.iter() {
        ans += sums_rh
            .binary_search_by(|e| match e.cmp(&(c - part_sum)) {
                Ordering::Equal => Ordering::Less,
                ord => ord,
            })
            .unwrap_err();
    }

    writeln!(out, "{}", ans).unwrap();
}