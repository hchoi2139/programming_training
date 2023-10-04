use std::{cmp, io};
use crate::scanner::UnsafeScanner;

const MOD: i32 = 9_901;

struct Solver {
    n: usize,
    m: usize,
    dp: Vec<Vec<i32>>,
}

impl Solver {
    fn new(n: usize, m: usize) -> Self {
        Self {
            n: cmp::max(n, m),
            m: cmp::min(n, m),
            dp: vec![vec![-1; 1 << m]; n * m],
        }
    }

    fn go(
        &mut self,
        cur: usize,
        status: usize,
    ) -> i32 {
        if cur >= self.n * self.m {
            return if status == 0 && cur == self.n * self.m { 1 } else { 0 }
        }

        if self.dp[cur][status] != -1 { return self.dp[cur][status]; }
        self.dp[cur][status] = 0;

        if status & 1 == 0 {
            // cur is empty.
            // Fill with 2 * 1 domino.
            // Cannot fill cur if fill is the (m - 1)-th of its row or (cur + 1) is occupied.
            if (cur % self.m != self.m - 1) && (status & 0b10 == 0) {
                self.dp[cur][status] += self.go(cur + 2, status >> 2);
            }
            // Fill with 1 * 2 domino.
            // Loop invariant assures the emptiness of (>cur) indexed spaces.
            self.dp[cur][status] += self.go(cur + 1, status >> 1 | 1 << (self.m - 1));
        }
        else {
            // cur is occupied.
            self.dp[cur][status] += self.go(cur + 1, status >> 1);
        }

        self.dp[cur][status] %= MOD;
        self.dp[cur][status]
    }
}

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let (n, m) = (scan.token::<usize>(), scan.token::<usize>());
    let mut solver = Solver::new(n, m);
    print!("{}", solver.go(0, 0));
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
