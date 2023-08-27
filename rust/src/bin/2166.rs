use std::io::Write;
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
    
    let mut polygon = Vec::with_capacity(n);
    for _ in 0..n {
        polygon.push((scan.token::<i64>(), scan.token::<i64>()));
    }

    let mut ans: f64 = 0.;
    for i in 1..(n - 1) {
        let (x1, y1, x2, y2) = (
            polygon[0].0 - polygon[i].0,
            polygon[0].1 - polygon[i].1,
            polygon[0].0 - polygon[i + 1].0,
            polygon[0].1 - polygon[i + 1].1,
        );
        ans += (((x1 * y2 - x2 * y1) * 10) as f64).round() / 10.
    }

    writeln!(out, "{:.1}", ans.abs() / 2.).unwrap();
}