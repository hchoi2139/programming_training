use io::Write;
use std::{collections::BinaryHeap, io, str, cmp};

const MAX: i32 = 200_000 * 800;

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

fn shortest_dist(
    dist: &mut Vec<i32>,
    graph: &Vec<Vec<(usize, i32)>>,
    from: usize,
    to: usize,
) -> i32 {
    dist.fill(MAX + 1);
    dist[from] = 0;

    let mut pq = BinaryHeap::new();
    pq.push((0, from));

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

    dist[to]
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (n, e) = (scan.token::<usize>(), scan.token::<usize>());

    let mut dist = vec![MAX + 1; n + 1];
    let mut graph = vec![vec![]; n + 1];

    for _ in 0..e {
        let (a, b, c) = (
            scan.token::<usize>(),
            scan.token::<usize>(),
            scan.token::<i32>(),
        );
        graph[a].push((b, c));
        graph[b].push((a, c));
    }

    let (v1, v2) = (scan.token::<usize>(), scan.token::<usize>());

    let cost1 = shortest_dist(&mut dist, &graph, 1, v1)
        + shortest_dist(&mut dist, &graph, v1, v2)
        + shortest_dist(&mut dist, &graph, v2, n);
    let cost2 = shortest_dist(&mut dist, &graph, 1, v2)
        + shortest_dist(&mut dist, &graph, v2, v1)
        + shortest_dist(&mut dist, &graph, v1, n);

    let cost = cmp::min(cost1, cost2);
    
    if cost <= MAX {
        writeln!(out, "{cost}").unwrap();
    } else {
        writeln!(out, "-1").unwrap();
    }
}