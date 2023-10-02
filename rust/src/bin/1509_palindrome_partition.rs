use std::{cmp, io};

const INF: u32 = 10_000;

fn main() {
    let mut given = String::new();
    io::stdin().read_line(&mut given).expect("Failed reading.");
    let given = given.trim_end();
    let g_len = given.len();

    let mut is_palindrome = vec![vec![false; g_len]; g_len];
    // String of len 1 is always palindrome.
    for i in 0..g_len {
        is_palindrome[i][i] = true;
    }

    // String of len 2 is palindrome if both chars are the same.
    for i in 1..g_len {
        if given[i-1..i] == given[i..i+1] { is_palindrome[i-1][i] = true; }
    }

    // String of shape "c_s (rest) c_e" is palindrome if c_s == c_e and rest is palindrome.
    for len in 3..=g_len {
        for s in 0..=(g_len - len) {
            let e = s + len - 1;
            if given[s..s+1] == given[e..e+1] && is_palindrome[s+1][e-1] {
                is_palindrome[s][e] = true;
            }
        }
    }

    let mut dp = vec![INF; g_len + 1];
    dp[0] = 0;
    for e in 1..=g_len {
        for s in 1..=e {
            if is_palindrome[s-1][e-1] { dp[e] = cmp::min(dp[e], dp[s - 1] + 1); }
        }
    }

    println!("{}", dp[g_len]);
}