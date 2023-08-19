use std::io::Write;
use std::{io, str, collections::VecDeque};

const UPPER_BOUND: usize = 9_999;
const MAX: usize = 10_000;

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

#[inline]
fn op_d(t: usize) -> usize {
    t * 2 % MAX
}

#[inline]
fn op_s(t: usize) -> usize {
    (t + UPPER_BOUND) % MAX
}

#[inline]
fn op_l(t: usize) -> usize {
    t / 1_000 + (t % 1_000) * 10
}

#[inline]
fn op_r(t: usize) -> usize {
    t / 10 + (t % 10) * 1000
}

fn bfs(a: usize, b: usize) -> String {
    let mut visited = [false; MAX];
    let mut prev = [MAX; MAX];
    let mut op = [""; MAX];

    let mut dq = VecDeque::new();
    dq.push_back(a);
    visited[a] = true;

    while let Some(n) = dq.pop_front() {
        if n == b {
            break;
        }

        let nd = op_d(n);
        if !visited[nd] {
            visited[nd] = true;
            prev[nd] = n;
            op[nd] = "D";
            dq.push_back(nd);
        }

        let ns = op_s(n);
        if !visited[ns] {
            visited[ns] = true;
            prev[ns] = n;
            op[ns] = "S";
            dq.push_back(ns);
        }

        let nl = op_l(n);
        if !visited[nl] {
            visited[nl] = true;
            prev[nl] = n;
            op[nl] = "L";
            dq.push_back(nl);
        }

        let nr = op_r(n);
        if !visited[nr] {
            visited[nr] = true;
            prev[nr] = n;
            op[nr] = "R";
            dq.push_back(nr);
        }
    }

    let mut rev_res = VecDeque::new();
    let mut cur = b;
    while cur != a {
        rev_res.push_back(op[cur]);
        cur = prev[cur];
    }
    let mut res = String::new();
    while let Some(o) = rev_res.pop_back() {
        res += o;
    }

    res
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let t = scan.token::<usize>();

    for _ in 0..t {
        let (a, b) = (scan.token::<usize>(), scan.token::<usize>());
        writeln!(out, "{}", bfs(a, b)).unwrap();
    }
}