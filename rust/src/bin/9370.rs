use io::Write;
use std::{collections::BinaryHeap, io, str, vec};

const INF: i32 = 50_000 * 1_000 + 1;

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

fn dijkstra(from: usize, dist: &mut Vec<i32>, graph: &Vec<Vec<(usize, i32)>>) {
    dist[from] = 0;

    let mut pq = BinaryHeap::new();
    pq.push((0, from));

    while let Some((mut cost, vertex)) = pq.pop() {
        cost *= -1;

        for &(next_vertex, add_cost) in graph[vertex].iter() {
            let next_cost = cost + add_cost;

            if dist[next_vertex] > next_cost {
                dist[next_vertex] = next_cost;
                pq.push((-next_cost, next_vertex));
            } 
        }
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let t = scan.token::<usize>();

    for _ in 0..t {
        let (n, m, t) = (scan.token::<usize>(), scan.token::<usize>(), scan.token::<usize>());
        let (s, g, h) = (scan.token::<usize>(), scan.token::<usize>(), scan.token::<usize>());

        let mut graph = vec![vec![]; n + 1];
        let mut scent = -1;
        for _ in 0..m {
            let (a, b, d) = (scan.token::<usize>(), scan.token::<usize>(), scan.token::<i32>());
            graph[a].push((b, d));
            graph[b].push((a, d));

            if a == g && b == h || a == h && b == g {
                scent = d;
            }
        }

        let mut dest = vec![];
        for _ in 0..t {
            let cand = scan.token::<usize>();

            let mut from_s = vec![INF; n + 1];
            let mut from_g = vec![INF; n + 1];
            let mut from_h = vec![INF; n + 1];
            dijkstra(s, &mut from_s, &graph);
            dijkstra(g, &mut from_g, &graph);
            dijkstra(h, &mut from_h, &graph);

            if from_s[g] + scent + from_h[cand] == from_s[cand] || 
               from_s[h] + scent + from_g[cand] == from_s[cand] 
            {
                dest.push(cand);
            }
        }

        dest.sort();

        let res = dest.iter()
            .map(|&d| d.to_string())
            .collect::<Vec<String>>()
            .join(" ");

        writeln!(out, "{}", res).unwrap();
    }
}