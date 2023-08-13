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
    if n == 1 {
        writeln!(out, "0").unwrap();
        return;
    }

    let mut sieve = vec![true; n + 1];
    sieve[0] = false;
    sieve[1] = false;

    for i in 2..=(n as f64).sqrt() as usize {
        if sieve[i] {
            let mut j = i * i;
            while j <= n {
                sieve[j] = false;
                j += i;
            }
        }
    }

    let mut primes = vec![];
    for (i, &is_prime) in sieve.iter().enumerate() {
        if is_prime {
            primes.push(i);
        }
    }

    let (mut left, mut right) = (0, 0);
    let mut sum = primes[0];
    let mut num_cases = 0;
    loop {
        match sum.cmp(&n) {
            Ordering::Less => {
                right += 1;
                if right >= primes.len() {
                    break;
                }
                sum += primes[right];
            },
            Ordering::Equal => {
                num_cases += 1;
                right += 1;
                if right >= primes.len() {
                    break;
                }
                sum += primes[right];
            },
            Ordering::Greater => {
                sum -= primes[left];
                left += 1;
            }
        }
    }

    writeln!(out, "{}", num_cases).unwrap();
}