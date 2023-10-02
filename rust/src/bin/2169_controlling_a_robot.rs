use std::{cmp, io};
use crate::scanner::UnsafeScanner;

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let (n, m) = (scan.token::<usize>(), scan.token::<usize>());

    let mut values = vec![vec![0; m]; n];
    for i in 0..n {
        for j in 0..m {
            values[i][j] = scan.token::<i32>();
        }
    }

    let mut dp = vec![vec![0; m]; n];
    dp[0][0]=  values[0][0];
    for j in 1..m { dp[0][j] = dp[0][j-1] + values[0][j]; }

    for i in 1..n {
        let (mut left_to_right, mut right_to_left) = (vec![0; m], vec![0; m]);

        // Left to right.
        left_to_right[0] = dp[i-1][0] + values[i][0];
        for j in 1..m {
            left_to_right[j] = cmp::max(dp[i-1][j], left_to_right[j-1]) + values[i][j];
        }

        // Right to left.
        right_to_left[m-1] = dp[i-1][m-1] + values[i][m-1];
        for j in (0..(m-1)).rev() {
            right_to_left[j] = cmp::max(dp[i-1][j], right_to_left[j+1]) + values[i][j];
        }

        for j in 0..m {
            dp[i][j]= cmp::max(left_to_right[j], right_to_left[j]);
        }
    }

    println!("{}", dp[n-1][m-1]);
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
