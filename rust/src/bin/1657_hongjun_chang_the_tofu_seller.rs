use std::{cmp, io};

const INF: i32 = 1_000_000_000;

struct Solver {
    n: usize,
    m: usize,
    tofu: Vec<Vec<usize>>,
    price: Vec<Vec<i32>>,
    dp: Vec<Vec<i32>>,
}

impl Solver {
    fn new(n: usize, m: usize, tofu: Vec<Vec<usize>>, price: Vec<Vec<i32>>) -> Self {
        Self {
            n,
            m,
            tofu,
            price,
            dp: vec![vec![-1; 1 << m]; n * m],
        }
    }

    fn idx_to_tofu_price(&self, i1: usize, i2: usize) -> i32 {
        let (r1, c1) = (i1 / self.m, i1 % self.m);
        let (r2, c2) = (i2 / self.m, i2 % self.m);
        if r1 >= self.n || r2 >= self.n { -INF }
        else { self.price[self.tofu[r1][c1]][self.tofu[r2][c2]] }
    }

    fn go(
        &mut self,
        cur: usize,
        stat: usize,
    ) -> i32 {
        if cur >= self.n * self.m {
            return if cur == self.n * self.m && stat == 0 { 0 } else { -INF }
        }

        if self.dp[cur][stat] != -1 { return self.dp[cur][stat]; }
        self.dp[cur][stat] = 0;

        // Skip cur, regardless of the state of cur -- either occupied or empty.
        self.dp[cur][stat] = cmp::max(self.dp[cur][stat], self.go(cur + 1, stat >> 1));

        // The case cur is empty.
        if stat & 1 == 0 {
            // Fill cur with 2 * 1 domino.
            // Cannot fill cur if fill is (m - 1)-th of it row or (cur + 1) is occupied.
            if (cur % self.m != self.m - 1) && (stat & 0b10 == 0) {
                self.dp[cur][stat] = cmp::max(
                    self.dp[cur][stat],
                    self.go(cur + 2, stat >> 2) + self.idx_to_tofu_price(cur, cur + 1),
                );
            }
            // Fill cur with 1 * 2 domino.
            // Assumption of the function assures the emptiness of (>cur) indexed spaces.
            self.dp[cur][stat] = cmp::max(
                self.dp[cur][stat],
                self.go(cur + 1, stat >> 1 | 1 << (self.m - 1)) + self.idx_to_tofu_price(cur, cur + self.m),
            );
        }

        self.dp[cur][stat]
    }
}

fn grade_to_idx(grade: char) -> usize {
    match grade {
        'A' => 0,
        'B' => 1,
        'C' => 2,
        'D' => 3,
        'F' => 4,
        _   => unreachable!(),
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Failed reading a line.");
    let input: Vec<usize> = input
        .split_ascii_whitespace()
        .map(|c| c.parse::<usize>().expect("Failed parsing to usize."))
        .collect();
    let (n, m) = (input[0], input[1]);

    let mut tofu: Vec<Vec<usize>> = Vec::with_capacity(n + 1);
    for _ in 0..n {
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Failed reading a line.");
        tofu.push(input
            .trim_end()
            .chars()
            .map(|c| grade_to_idx(c))
            .collect()
        );
    }

    let price = vec![
        vec![10, 8, 7, 5, 1],
        vec![8, 6, 4, 3, 1],
        vec![7, 4, 3, 2, 1],
        vec![5, 3, 2, 2, 1],
        vec![1, 1, 1, 1, 0]
    ];

    let mut solver = Solver::new(n, m, tofu, price);
    println!("{}", solver.go(0, 0));
}
