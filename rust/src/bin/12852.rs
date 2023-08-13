use io::Write;
use std::{io, str, cmp};

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
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let mut n = scan.token::<usize>();

    // dp[i]: minimum n. of ops to reach 1 by given ops.
    let mut dp = vec![-1; n + 1];

    // Initialise dp[i] with the n. of cases by only using `subtract 1` op.
    for i in 1..=n {
        dp[i] = dp[i - 1] + 1;
    }

    for i in 2..=n {
        // Must cascade down the conditions to regard the case i % 6 == 0.
        if i % 3 == 0 {
            dp[i] = cmp::min(dp[i], dp[i / 3]);
        }

        if i % 2 == 0 {
            dp[i] = cmp::min(dp[i], dp[i / 2]);
        }

        dp[i] = cmp::min(dp[i], dp[i - 1]) + 1;
    }

    writeln!(out, "{}", dp[n]).unwrap();

    while n != 0 {
        write!(out, "{} ", n).unwrap();
        
        if n % 3 == 0 && dp[n] == dp[n / 3] + 1 {
            n /= 3;
        }
        else if n % 2 == 0 && dp[n] == dp[n / 2] + 1 {
            n /= 2;     
        }
        else {
            n -= 1;
        }
    }
}