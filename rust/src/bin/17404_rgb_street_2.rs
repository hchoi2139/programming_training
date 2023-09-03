use std::{io, str, cmp};

const INF: u32 = 1_000 * 1_000 + 1;

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

struct Colour {
    r: u32,
    g: u32,
    b: u32,
}

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let n = scan.token::<usize>();

    let mut cost = Vec::with_capacity(n);

    for _ in 0..n {
        cost.push(Colour {
            r: scan.token::<u32>(),
            g: scan.token::<u32>(),
            b: scan.token::<u32>(),
        });
    }

    let mut min_cost = INF;
    let mut dp = vec![vec![0; 3]; n];

    for c0 in 0..3 {
        // Set dp[0][c0] match the corresponding cost, else INF.
        dp[0][0] = INF;
        dp[0][1] = INF;
        dp[0][2] = INF;
        match c0 {
            0 => dp[0][0] = cost[0].r,
            1 => dp[0][1] = cost[0].g,
            2 => dp[0][2] = cost[0].b,
            _ => unreachable!(),
        };

        // Dynamic Programming.
        for i in 1..n {
            dp[i][0] = cost[i].r + cmp::min(dp[i - 1][1], dp[i - 1][2]);
            dp[i][1] = cost[i].g + cmp::min(dp[i - 1][0], dp[i - 1][2]);
            dp[i][2] = cost[i].b + cmp::min(dp[i - 1][0], dp[i - 1][1]);
        }

        // Extract min cost of painting for those c0 != cn.
        for cn in 0..3 {
            if c0 != cn {
                min_cost = cmp::min(min_cost, dp[n - 1][cn]);
            }
        }
    }

    println!("{min_cost}");
}