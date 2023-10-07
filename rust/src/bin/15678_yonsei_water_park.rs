use scanner::UnsafeScanner;
use std::{io, cmp, collections::VecDeque};

const INF: i64 = 1_000_000_000 + 1;

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let (n, d) = (scan.token::<usize>(), scan.token::<usize>());

    let mut scores = vec![0; n + 1];
    scores.iter_mut()
          .take(n + 1)
          .skip(1)
          .for_each(|e| *e = scan.token::<i64>());

    let mut cand: VecDeque<usize> = VecDeque::with_capacity(d + 1);
    let mut dp: Vec<i64> = vec![0; n + 1];

    let mut ans = -INF;
    for i in 1..=n {
        while let Some(front) = cand.front() {
            if *front + d < i { cand.pop_front(); } else { break; }
        }
        let max_score_before_i = if let Some(front) = cand.front() { dp[*front] } else { -INF };
        dp[i] = scores[i] + cmp::max(0, max_score_before_i);
        ans = cmp::max(ans, dp[i]);
        while let Some(back) = cand.back() {
            if dp[*back] < dp[i] { cand.pop_back(); } else { break; }
        }
        cand.push_back(i);
    }

    println!("{ans}");
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
