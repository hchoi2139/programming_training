use std::io::Write;
use std::{io, str};
use std::collections::VecDeque;

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

struct Sorter {
    n: usize,
    graph: Vec<Vec<usize>>,
    in_degree: Vec<usize>,
    sorted: Vec<usize>,
}

impl Sorter {
    fn new(n: usize) -> Self {
        Self {
            n,
            graph: vec![vec![]; n + 1],
            in_degree: vec![0; n + 1],
            sorted: Vec::with_capacity(n),
        }
    }

    fn topological_sort(&mut self) {
        let mut queue = VecDeque::new();

        for i in 1..=self.n {
            if self.in_degree[i] == 0 {
                queue.push_back(i);
            }
        }

        while let Some(cur) = queue.pop_front() {
            self.sorted.push(cur);
            for &next in self.graph[cur].iter() {
                self.in_degree[next] -= 1;
                if self.in_degree[next] == 0 {
                    queue.push_back(next);
                }
            }
        }
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (n, m) = (scan.token::<usize>(), scan.token::<usize>());

    let mut sorter = Sorter::new(n);

    for _ in 0..m {
        let (a, b) = (scan.token::<usize>(), scan.token::<usize>());
        sorter.graph[a].push(b);
        sorter.in_degree[b] += 1;
    }

    sorter.topological_sort();

    for i in 0..n {
        write!(out, "{} ", sorter.sorted[i]).unwrap();
    }
}