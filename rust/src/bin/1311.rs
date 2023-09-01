use std::{io, str, cmp};

const INF: usize = 200_001;

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
    let nn = 1 << n;

    // Given by the problem.
    let mut cost = [[0; 20]; 20];

    // dp[i]: min cost to reach the state of i, where i is bitset.
    let mut dp = vec![INF; nn];

    for i in 0..n {
        for j in 0..n {
            cost[i][j] = scan.token::<usize>();
        }
    }

    dp[0] = 0;
    for i in 0..nn {
        let x = nbits(i);
        for j in 0..n {
            if i & (1 << j) == 0 {
                dp[i | (1 << j)] = cmp::min(dp[i | (1 << j)], dp[i] + cost[x][j]);
            }
        }
    }

    println!("{}", dp[nn - 1]);
}