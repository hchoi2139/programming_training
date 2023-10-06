use scanner::UnsafeScanner;
use std::{io, collections::VecDeque};

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let (n, k) = (scan.token::<usize>(), scan.token::<usize>());

    let mut eff = vec![0; n + 1];
    eff.iter_mut().take(n + 1).skip(1).for_each(|val| *val = scan.token::<u64>());

    let mut p_sum = vec![0; n + 1];
    (1..=n).for_each(|i| p_sum[i] = p_sum[i - 1] + eff[i]);

    let mut cand: VecDeque<usize> = VecDeque::with_capacity(k + 1);
    let mut dp: Vec<u64> = vec![0; n + 1];

    // Preprocessing for 1..k.
    for i in 1..=k {
        while let Some(back) = cand.pop_back() {
            if eff[back] <= eff[i] {
                cand.push_back(back);
                break;
            }
        }
        cand.push_back(i);
        dp[i] = p_sum[i];
    }

    // Sliding window, each step investigates i - k <= j <= i, i = (k + 1)..n.
    for i in (k + 1)..=n {
        if let Some(front) = cand.front() {
            if *front < i - k { cand.pop_front(); }
        }
        while let Some(back) = cand.pop_back() {
            if p_sum[back] - dp[back - 1] <= p_sum[i] - dp[i - 1] {
                cand.push_back(back);
                break;
            }
        }
        cand.push_back(i);
        let j = cand.front().unwrap();
        dp[i] = p_sum[i] - p_sum[*j] + dp[*j - 1];
    }

    println!("{}", dp[n]);
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
