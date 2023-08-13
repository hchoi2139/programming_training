#include <bits/stdc++.h>

using namespace std;

typedef long long ll;
typedef vector<vector<ll>> matrix;

const ll P {1000000007};

ll n {0};

matrix operator*(const matrix& a, const matrix& b)
{
  matrix c = {{0, 0}, {0, 0}};

  for (int i = 0; i < 2; i++)
  {
    for (int j = 0; j < 2; j++)
    {
      for (int k = 0; k < 2; k++)
      {
        c[i][j] += a[i][k] * b[k][j];
      }
      c[i][j] %= P;
    }
  }
  return c;
}

matrix pow(const matrix& m, const ll a)
{
  if (a == 1)
    return m;
  
  matrix h = pow(m, a / 2);
  h = h * h;

  if (a % 2 == 1)
    h = h * m;
  
  return h;
}

int main()
{
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> n;
  
  matrix m = {{1, 1}, {1, 0}};
  cout << pow(m, n + 1)[1][1] % P << '\n';
  return 0;
}