use io::Write;
use std::{io, str, cmp::Ordering};

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

    let mut arr = vec![];
    for _ in 0..n {
        arr.push(scan.token::<i32>());
    }

    arr.sort();

    let mut left = 0;
    let mut right = n - 1;
    let mut abs_char_val = i32::MAX;
    let mut res = vec![0; 2];
    while left < right {
        let sum = arr[left] + arr[right];
        let abs_sum = i32::abs(sum);

        if abs_char_val > abs_sum {
            abs_char_val = abs_sum;
            res[0] = arr[left];
            res[1] = arr[right];
        }

        match sum.cmp(&0) {
            Ordering::Less => left += 1,
            Ordering::Equal => break,
            Ordering::Greater => right -= 1, 
        }
    }

    writeln!(out, "{} {}", res[0], res[1]).unwrap();
}