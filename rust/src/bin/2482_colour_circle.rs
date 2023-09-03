use std::{io, str};

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

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let (n, k) = (scan.token::<usize>(), scan.token::<usize>());

    if k == 1 {
        println!("{n}");
        return;
    }

    // dp[i][j]: No. of cases using j colours inspecting i rooms in colour circle.
    // dp[i][j] = dp[i - 2][j - 1] + dp[i - 1][j].
    let mut dp = vec![vec![0; k + 1]; n + 1];

    // Initialise dp.
    for i in 1..=n {
        dp[i][0] = 0;
        dp[i][1] = i;
    }

    const MOD: usize = 1_000_000_003;

    // Dynamic programming, ignoring the circular condition.
    for i in 2..=n {
        for j in 2..=k {
            dp[i][j] = (dp[i - 2][j - 1] + dp[i - 1][j]) % MOD;
        }
    }

    // Update dp[n][k] regarding the colour conflict of 1-st and n-th idx.
    dp[n][k] = (dp[n - 1][k] + dp[n - 3][k - 1]) % MOD;

    println!("{}", dp[n][k]);
}