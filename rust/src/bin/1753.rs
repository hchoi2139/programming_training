use io::Write;
use std::{collections::BinaryHeap, io, str, vec};

pub struct UnsafeScanner<R> {
    reader: R,
    buf_str: Vec<u8>,
    buf_iter: str::SplitAsciiWhitespace<'static>,
}

impl<R: io::BufRead> UnsafeScanner<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            buf_str: vec![],
            buf_iter: "".split_ascii_whitespace(),
        }
    }

    pub fn token<T: str::FromStr>(&mut self) -> T {
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

    let v = scan.token::<usize>();
    let e = scan.token::<usize>();
    let k = scan.token::<usize>();

    let mut dist = vec![i32::MAX; v + 1];
    let mut graph = vec![vec![]; v + 1];
    
    for _ in 0..e {
        let (u, v, w) = (
            scan.token::<usize>(),
            scan.token::<usize>(),
            scan.token::<i32>(),
        );
        graph[u].push((v, w));
    }

    dist[k] = 0;

    let mut pq = BinaryHeap::new();
    pq.push((0, k));

    while let Some((mut cost, vertex)) = pq.pop() {
        cost *= -1;

        for info in graph[vertex].iter() {
            let (next_vertex, add_cost) = *info;
            let next_cost = cost + add_cost;

            if dist[next_vertex] > next_cost {
                dist[next_vertex] = next_cost;
                pq.push((-next_cost, next_vertex));
            }
        }
    }

    for i in 1..=v {
        if dist[i] == i32::MAX {
            writeln!(out, "INF").unwrap();
        } else {
            writeln!(out, "{}", dist[i]).unwrap();
        }
    }
}