use std::io;

fn read_usize() -> usize {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().parse().unwrap()
}

fn read_string() -> String {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
}

fn compute_rem(int: &str, k: usize) -> usize {
    let mut res = 0;
    for chr in int.chars() {
        res *= 10;
        res += chr.to_digit(10).unwrap() as usize;
        res %= k;
    }
    res
}

fn compute_gcd(a: u64, b: u64) -> u64 {
    let mut a = a;
    let mut b = b;
    while b != 0 {
        let tmp = b;
        b = a % b;
        a = tmp;
    }
    a
}

fn main() {
    let n = read_usize();
    
    let mut arr = vec![];
    let mut len = vec![0; n];
    for i in 0..n {
        let input = read_string();
        len[i] = input.chars().count();
        arr.push(input);
    }

    let k = read_usize();

    let rem: Vec<usize> = arr
        .iter()
        .map(|i| compute_rem(i, k))
        .collect();

    let mut tens_rem = vec![0; 51];
    tens_rem[0] = 1 % k;
    for i in 1..51 {
        tens_rem[i] = (tens_rem[i - 1] * 10) % k;
    }

    let mut dp = vec![vec![0; k]; 1 << n];
    dp[0][0] = 1;
    for cur in 0..(1 << n) {
        for i in 0..n {
            if cur & (1 << i) == 0 {
                let next = cur | (1 << i);
                for j in 0..k {
                   let nk = ((tens_rem[len[i]] * j) % k + rem[i]) % k;
                    dp[next][nk] += dp[cur][j];
                }
            }
        }
    }

    let num_vals = dp[(1 << n) - 1][0];
    let num_all = (1..=(n as u64)).fold(1, |acc, x| acc * x);
    let gcd = compute_gcd(num_vals, num_all);

    println!("{}/{}", num_vals / gcd, num_all / gcd);
}