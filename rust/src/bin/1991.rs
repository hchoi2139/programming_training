use std::io::Write;
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

#[inline]
fn to_alpha_idx(c: char) -> usize {
    (c as usize) - ('A' as usize)
}

#[inline]
fn to_char(n: usize) -> char {
    (n as u8 + b'A') as char
}

fn preorder(node: usize, tree: &Vec<(usize, usize)>, cache: &mut String) {
    if node == 26 {
        return;
    }
    cache.push(to_char(node));
    preorder(tree[node].0, tree, cache);
    preorder(tree[node].1, tree, cache);
}

fn inorder(node: usize, tree: &Vec<(usize, usize)>, cache: &mut String) {
    if node == 26 {
        return;
    }
    inorder(tree[node].0, tree, cache);
    cache.push(to_char(node));
    inorder(tree[node].1, tree, cache);
}

fn postorder(node: usize, tree: &Vec<(usize, usize)>, cache: &mut String) {
    if node == 26 {
        return;
    }
    postorder(tree[node].0, tree, cache);
    postorder(tree[node].1, tree, cache);
    cache.push(to_char(node));
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let n = scan.token::<usize>();

    let mut tree = vec![(26, 26); 26];
    for _ in 0..n {
        let (p, l, r) = (
            scan.token::<char>(),
            scan.token::<char>(),
            scan.token::<char>(),
        );
        let (up, ul, ur) = (
            to_alpha_idx(p),
            if l == '.' { 26 } else { to_alpha_idx(l) },
            if r == '.' { 26 } else { to_alpha_idx(r) },
        );
        tree[up] = (ul, ur);
    }

    let mut cache = String::new();
    preorder(0, &tree, &mut cache);
    writeln!(out, "{cache}").unwrap();

    cache.clear();
    inorder(0, &tree, &mut cache);
    writeln!(out, "{cache}").unwrap();

    cache.clear();
    postorder(0, &tree, &mut cache);
    writeln!(out, "{cache}").unwrap();
}