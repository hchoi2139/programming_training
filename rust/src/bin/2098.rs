use std::{io, str, cmp};

const INF: i32 = 16_000_001;

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

fn nbits(num: usize) -> usize {
    let mut cnt = 0;
    let mut n = num;
    while n != 0 {
        cnt += n & 1;
        n >>= 1;
    }
    cnt
}

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let n = scan.token::<usize>();

    let mut adj = [[INF; 16]; 16];
    let mut cost = vec![vec![INF; n]; 1 << n];

    let mut input;
    for i in 0..n {
        for j in 0..n {
            input = scan.token::<i32>();
            if input != 0 {
                adj[i][j] = input;
            }
        }
    }

    // Fix 0 as start node, wlog.

    // Initialise cost[0].
    for x in 1..n {
        cost[0][x] = adj[0][x];
    }

    let univ_nodes = (1 << n) - 1;

    let mut nodes_inc_order: Vec<usize> = (0b1..(1 << (n - 1))).collect();
    nodes_inc_order.sort_by_key(|&num| (nbits(num), num));

    for s in nodes_inc_order {
        let s = s << 1;
        let mut val_nodes = univ_nodes & !(s | 0b1);
        let mut x = 0;
        while val_nodes != 0 {
            if val_nodes & 0b1 != 0 {
                let mut tar_nodes = s;
                let mut y = 0;
                while tar_nodes != 0 {
                    if tar_nodes & 0b1 != 0 && adj[y][x] != 0 {
                        cost[s][x] = cmp::min(cost[s][x], cost[s & !(1 << y)][y] + adj[y][x]);
                    }
                    tar_nodes >>= 1;
                    y += 1;
                }
            }
            val_nodes >>= 1;
            x += 1;
        }
    }

    let mut ans = INF;
    for x in 1..n {
        ans = cmp::min(ans, cost[univ_nodes & !(0b1 | (1 << x))][x] + adj[x][0]);
    }

    println!("{ans}");
}