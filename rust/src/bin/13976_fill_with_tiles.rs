use std::ops::Mul;

const MOD: i64 = 1_000_000_007;

macro_rules! read {
    ($out:ident as $type:ty) => {
        let mut inner = String::new();
        std::io::stdin().read_line(&mut inner).expect("A String");
        let $out = inner.trim().parse::<$type>().expect("Parsable");
    };
}

struct Mat2by1 {
    a1: i64,
    a2: i64,
}

impl Mat2by1 {
    fn new(
        a1: i64,
        a2: i64,
    ) -> Self {
        Self { a1, a2 }
    }
}

#[derive(Clone, Copy)]
struct Mat2by2 {
    a11: i64,
    a12: i64,
    a21: i64,
    a22: i64,
}

impl Mul for Mat2by2 {
    type Output = Mat2by2;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::Output {
            a11: (self.a11 * rhs.a11 % MOD + self.a12 * rhs.a21 % MOD + MOD) % MOD,
            a12: (self.a11 * rhs.a12 % MOD + self.a12 * rhs.a22 % MOD + MOD) % MOD,
            a21: (self.a21 * rhs.a11 % MOD + self.a22 * rhs.a21 % MOD + MOD) % MOD,
            a22: (self.a21 * rhs.a12 % MOD + self.a22 * rhs.a22 % MOD + MOD) % MOD,
        }
    }
}

impl Mat2by2 {
    fn new(
        a11: i64,
        a12: i64,
        a21: i64,
        a22: i64,
    ) -> Self {
        Self { a11, a12, a21, a22 }
    }

    fn dot(&self, rhs: &Mat2by1) -> Mat2by1 {
        Mat2by1 {
            a1: (self.a11 * rhs.a1 % MOD + self.a12 * rhs.a2 % MOD + MOD) % MOD,
            a2: (self.a21 * rhs.a1 % MOD + self.a22 * rhs.a2 % MOD + MOD) % MOD,
        }
    }

    fn pow(self, mut exp: u64) -> Self {
        let mut ans = Self::new(1, 0, 0, 1);
        let mut base = self;
        while exp != 0 {
            if exp % 2 != 0 {
                ans = base * ans;
            }
            base = base * base;
            exp /= 2;
        }
        ans
    }
}

fn main() {
    read!(n as u64);

    if n % 2 != 0 { println!("0"); }
    else {
        let mut base = Mat2by2::new(4, -1, 1, 0);
        let mut ans = Mat2by1::new(3, 1);
        base = base.pow(n / 2 - 1);
        ans = base.dot(&ans);
        println!("{}", ans.a1);
    }
}