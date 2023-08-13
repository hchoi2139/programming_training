use std::{io, cmp};

fn read_string() -> String {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
}

fn main() {
    let (s1, s2) = (read_string(), read_string());
    let (len1, len2) = (s1.len(), s2.len());

    let mut dp = vec![vec![0; len2+1]; len1+1];
    for i in 1..=len1 {
        for j in 1..=len2 {
            if s1[i-1..i] == s2[j-1..j] {
                dp[i][j] = dp[i-1][j-1] + 1;
            } else {
                dp[i][j] = cmp::max(dp[i-1][j], dp[i][j-1]);
            }
        }
    }
    
    let mut lcs = String::new();
    let (mut r, mut c) = (len1, len2);
    while dp[r][c] != 0 {
        if dp[r][c] == dp[r-1][c] {
            r -= 1;
        }
        else if dp[r][c] == dp[r][c-1] {
            c -= 1;
        }
        else {
            lcs.push_str(&s1[r-1..r]);
            r -= 1;
            c -= 1;
        }
    }

    println!("{}", dp[len1][len2]);
    println!("{}", lcs.chars().rev().collect::<String>());
}