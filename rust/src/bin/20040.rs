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

fn find(parent: &mut Vec<usize>, i: usize) -> usize {
    if parent[i] == i { return i; }
    let pi = find(parent, parent[i]);
    parent[i] = pi;
    pi
}

fn main() {
    let stdin = io::stdin();
    let mut scan = UnsafeScanner::new(stdin.lock());

    let (n, m) = (scan.token::<usize>(), scan.token::<usize>());
    let mut parent: Vec<usize> = (0..n).collect();
    let mut has_cycle = false;

    for i in 1..=m {
        let (a, b) = (scan.token::<usize>(), scan.token::<usize>());
        let (pa, pb) = (find(&mut parent, a), find(&mut parent, b));
        if pa == pb {
            has_cycle = true;
            println!("{i}");
            break;
        } else {
            parent[pb] = pa;
        }
    }

    if !has_cycle {
        println!("0");
    }
}