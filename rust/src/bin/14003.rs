use io::Write;
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
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let n = scan.token::<usize>();

    let mut seq = vec![0; n + 1];
    let mut inc_pseq = vec![];
    let mut dp = vec![0; n + 1];
    let mut max_idx = 0;
    for i in 1..=n {
        seq[i] = scan.token::<i32>();

        if *inc_pseq.last().unwrap_or(&i32::MIN) < seq[i] {
            inc_pseq.push(seq[i]);
            dp[i] = inc_pseq.len();
        }
        else {
            let p_idx = inc_pseq.partition_point(|&x| x < seq[i]);
            inc_pseq[p_idx] = seq[i];
            dp[i] = p_idx + 1;
        }

        if dp[max_idx] < dp[i] {
            max_idx = i;
        }
    }

    writeln!(out, "{}", inc_pseq.len()).unwrap();

    let mut sol = vec![];
    sol.push(seq[max_idx]);
    for i in (1..max_idx).rev() {
        if seq[i] < seq[max_idx] && dp[i] + 1 == dp[max_idx] {
            sol.push(seq[i]);
            max_idx = i;
        }
    }

    while let Some(v) = sol.pop() {
        write!(out, "{} ", v).unwrap(); 
    }
    writeln!(out).unwrap();
}