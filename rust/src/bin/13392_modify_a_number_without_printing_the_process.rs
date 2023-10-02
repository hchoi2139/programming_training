use std::cmp;

const INF: u32 = 1_000_000;

#[allow(unused_macros)]
macro_rules! read {
    ($out:ident as $type:ty) => {
        let mut inner = String::new();
        std::io::stdin().read_line(&mut inner).expect("A String");
        let $out = inner.trim().parse::<$type>().expect("Parsable");
    };
}

#[allow(unused_macros)]
macro_rules! read_string {
    ($out:ident) => {
        let mut inner = String::new();
        std::io::stdin().read_line(&mut inner).expect("A String");
        let $out = inner.trim_end().to_string();
    };
}

fn main() {
    read!(n as usize);

    read_string!(cur);
    let cur: Vec<u32> = cur
        .chars()
        .map(|c| c.to_digit(10).unwrap())
        .collect();

    read_string!(goal);
    let goal: Vec<u32> = goal
        .chars()
        .map(|c| c.to_digit(10).unwrap())
        .collect();

    let mut dp = vec![vec![INF; 10]; n + 1];
    for j in 0..10 { dp[0][j] = j as u32; }

    for i in 1..=n {
        for j in 0..10 {
            let ls = (20 + goal[i - 1] - cur[i - 1] - (j as u32)) % 10;
            let rs = 10 - ls;
            let nl = (j + ls as usize) % 10;
            dp[i][j] = cmp::min(dp[i][j], dp[i - 1][j] + rs);
            dp[i][nl] = cmp::min(dp[i][nl], dp[i - 1][j] + ls);
        }
    }

    let mut ans = INF;
    for j in 0..10 {
        ans = cmp::min(ans, dp[n][j]);
    }

    println!("{ans}");
}
