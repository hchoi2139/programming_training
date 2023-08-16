use io::Write;
use std::{io, str, cmp};

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

struct Coord {
    r: i32,
    c: i32,
}

#[inline(always)]
fn l1_dist(c1: &Coord, c2: &Coord) -> i32 {
    (c1.r - c2.r).abs() + (c1.c - c2.c).abs()
}

struct Solver<'a> {
    n: i32,
    w: usize,
    route: Vec<i32>,
    dp: Vec<Vec<i32>>,
    locs: &'a Vec<Coord>,
}

impl<'a> Solver<'a> {
    fn new(locs: &'a Vec<Coord>, n: i32, w: usize) -> Self {
        let dp = vec![vec![-1; w + 1]; w + 1];
        let route = vec![0; w + 1];
        Self { n, w, route, dp, locs, }
    }

    fn find_min_travel_dist(
        &mut self,
        police1: usize,
        police2: usize,
    ) -> i32 {
        if police1 == self.w || police2 == self.w {
            return 0;
        }

        if self.dp[police1][police2] != -1 {
            return self.dp[police1][police2];
        }

        let cur_inc = cmp::max(police1, police2);
        let next_inc = cur_inc + 1;

        // The case police1 is inspecting the next incident.
        let mut dist1;
        if police1 == 0 {
            dist1 = l1_dist(&Coord { r: 1, c: 1 }, &self.locs[next_inc]);
        } else {
            dist1 = l1_dist(&self.locs[police1], &self.locs[next_inc]);
        }
        dist1 += self.find_min_travel_dist(next_inc, police2);

        // The case police2 is inspecting the next incident.
        let mut dist2;
        if police2 == 0 {
            dist2 = l1_dist(&Coord { r: self.n, c: self.n }, &self.locs[next_inc]);
        } else {
            dist2 = l1_dist(&self.locs[police2], &self.locs[next_inc]);
        }
        dist2 += self.find_min_travel_dist(police1, next_inc);

        self.dp[police1][police2] = cmp::min(dist1, dist2);

        self.dp[police1][police2]
    }

    fn find_trace(
        &mut self,
        police1: usize,
        police2: usize,
    ) {
        if police1 == self.w || police2 == self.w {
            return;
        }
        
        let cur_inc = cmp::max(police1, police2);
        let next_inc = cur_inc + 1;

        // The case police1 is inspecting the next incident.
        let mut dist1;
        if police1 == 0 {
            dist1 = l1_dist(&Coord { r: 1, c: 1 }, &self.locs[next_inc]);
        } else {
            dist1 = l1_dist(&self.locs[police1], &self.locs[next_inc]);
        }
        dist1 += self.dp[next_inc][police2];

        // The case police2 is inspecting the next incident.
        let mut dist2;
        if police2 == 0 {
            dist2 = l1_dist(&Coord { r: self.n, c: self.n }, &self.locs[next_inc]);
        } else {
            dist2 = l1_dist(&self.locs[police2], &self.locs[next_inc]);
        }
        dist2 += self.dp[police1][next_inc];

        if dist1 < dist2 {
            self.route[cur_inc] = 1;
            self.find_trace(next_inc, police2);
        } else {
            self.route[cur_inc] = 2;
            self.find_trace(police1, next_inc);
        }
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let (n, w) = (scan.token::<i32>(), scan.token::<usize>());

    let mut locs: Vec<Coord> = vec![];
    locs.push(Coord { r: 0, c: 0 });
    for _ in 1..=w {
        locs.push(Coord {
            r: scan.token::<i32>(),
            c: scan.token::<i32>(),
        });
    }

    let mut solver = Solver::new(&locs, n, w);
    let min_travel_dist = solver.find_min_travel_dist(0, 0);
    writeln!(out, "{min_travel_dist}").unwrap();

    solver.find_trace(0, 0);
    for i in 0..w {
        writeln!(out, "{}", solver.route[i]).unwrap();
    }
}