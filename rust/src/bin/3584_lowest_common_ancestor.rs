use std::io::{BufWriter, Write};
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
    let mut out = BufWriter::new(stdout.lock());

    let t = scan.token::<usize>();


    for _ in 0..t {
        let n = scan.token::<usize>();

        let mut parent = vec![0; n + 1];
        let mut visited = vec![false; n + 1];

        for _ in 1..=(n - 1) {
            let (a, b) = (scan.token::<usize>(), scan.token::<usize>());
            parent[b] = a;
        }

        let (mut u, mut v) = (scan.token::<usize>(), scan.token::<usize>());

        visited[u] = true;

        while parent[u] != 0 {
            u = parent[u];
            visited[u] = true;
        }

        loop {
            if visited[v] {
                writeln!(out, "{}", v).unwrap();
                break;
            }
            v = parent[v];
        }
    }
}