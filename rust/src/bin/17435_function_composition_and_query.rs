use std::io::Write;
use std::{io, str};

const MAX_N: f32 = 500_000.;

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

    let m = scan.token::<usize>();

    // Equivalent of f in the problem.
    let mut func = vec![0; m + 1];
    for i in 1..=m {
        func[i] = scan.token::<usize>();
    }

    let q = scan.token::<u32>();
    let log2_n = MAX_N.log2().ceil() as usize;

    // table[i][j]: f_k(j) where k == 2^i, i.e. k is the index of digit.
    //
    // Say that n is 11. Then n can be written as 1011(2). To get the value of f_11(n), we instead
    // look for f_8( f_2( f_1(n) ) ).
    let mut table = vec![vec![0; m + 1]; log2_n];

    // table[0][j] == f[j].
    for j in 1..=m {
        table[0][j] = func[j];
    }

    // f_2k(x) == f_k( f_k(x) ) by function composition.
    for i in 0..(log2_n - 1) {
        for j in 1..=m {
            table[i + 1][j] = table[i][table[i][j]];
        }
    }

    for _ in 0..q {
        let (mut n, mut x) = (scan.token::<usize>(), scan.token::<usize>());
        let mut i = 0;
        while n != 0 {
            if n % 2 == 1 {
                x = table[i][x];
            }
            n >>= 1;
            i += 1;
        }
        writeln!(out, "{x}").unwrap();
    }
}