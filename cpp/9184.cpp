#include <iostream>

#define endl '\n'
#define N 21

using namespace std;

int dp[N][N][N];

int w(int a, int b, int c)
{
  if (a <= 0 || b <= 0 || c <= 0)
    return 1;

  if (a > 20 || b > 20 || c > 20)
    return w(20, 20, 20);

  if (dp[a][b][c])
    return dp[a][b][c];

  if (a < b && b < c) 
  {
    dp[a][b][c] =  w(a, b, c - 1) + w(a, b - 1, c - 1) - w(a, b - 1, c);
  } 
  else 
  {
    dp[a][b][c] = w(a - 1, b, c)
                + w(a - 1, b - 1, c)
                + w(a - 1, b, c - 1)
                - w(a - 1, b - 1, c - 1);
  }

  return dp[a][b][c];
}

int main()
{
  ios::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  int a, b, c;
  while (true)
  {
    cin >> a >> b >> c;
    if (a == -1 && b == -1 && c == -1)
      break;

    cout << "w(" << a << ", " << b << ", " << c << ") = " << w(a, b, c) << endl;
  }

  return 0;
}