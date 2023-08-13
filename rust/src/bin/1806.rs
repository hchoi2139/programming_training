use io::Write;
use std::{io, str, cmp::{self, Ordering}};

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

    let (n, s) = (scan.token::<usize>(), scan.token::<i32>());

    let mut arr = vec![];
    for _ in 0..n {
        arr.push(scan.token::<i32>());
    }
    
    let mut start: usize= 0;
    let mut end: usize = 0;
    let mut sum = arr[0];
    let mut min_len = i32::MAX;
    while start <= end {
        match sum.cmp(&s) {
            Ordering::Less => {
                if end == n - 1 {
                    break;
                } else {
                    end += 1;
                    sum += arr[end];
                }
            },
            _ => {
                min_len = cmp::min(min_len, (end - start + 1) as i32);
                if min_len == 0 {
                    break;
                }
                sum -= arr[start];
                start += 1;
            }
        }
    }

    writeln!(out, "{}", if min_len == i32::MAX { 0 } else { min_len }).unwrap();
}