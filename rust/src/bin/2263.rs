use std::io::Write;
use std::{io, str};

const MAX: usize = 100_000 + 1;

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

struct Solver<'a> {
    preorder: &'a mut Vec<usize>,
    postorder: &'a [usize; MAX],
    idx_inorder: &'a [usize; MAX],
}

impl<'a> Solver<'a> {
    fn new(
        preorder: &'a mut Vec<usize>,
        postorder: &'a [usize; MAX],
        idx_inorder: &'a [usize; MAX],
    ) -> Self {
        Self { preorder, postorder, idx_inorder }
    }

    fn induce_preorder(
        &mut self,
        in_s: usize,
        in_e: usize,
        post_s: usize,
        post_e: usize,
    ) {
        if in_s > in_e || post_s > post_e {
            return;
        }
        
        let root = self.postorder[post_e];

        self.preorder.push(root);

        let idx_root = self.idx_inorder[root];
        let len_left = idx_root - in_s;
        let len_right = in_e - idx_root;

        self.induce_preorder(in_s, idx_root - 1, post_s, post_s + len_left - 1);
        self.induce_preorder(idx_root + 1, in_e, post_e - len_right, post_e - 1);
    }
}

fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut scan = UnsafeScanner::new(stdin.lock());
    let mut out = io::BufWriter::new(stdout.lock());

    let n = scan.token::<usize>();

    let mut preorder = vec![];
    let mut postorder = [0; MAX];

    let mut idx_inorder = [0; MAX];

    for i in 1..=n {
        idx_inorder[scan.token::<usize>()] = i;
    }

    for i in 1..=n {
        postorder[i] = scan.token::<usize>();
    }

    let mut solver = Solver::new(
        &mut preorder, 
        &postorder,
        &idx_inorder,
    );

    solver.induce_preorder(1, n, 1, n);
    
    for i in 0..n {
        write!(out, "{} ", preorder[i]).unwrap();
    }
}