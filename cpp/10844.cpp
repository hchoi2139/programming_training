#include <iostream>

#define MAX 100
#define R 1000000000
#define ll long long

using namespace std;

// dp[i][j] indicates the number of i-digit stair number that ends with j.
ll dp[MAX + 1][10];

int main()
{
  int n;
  cin >> n;

  for (int i = 1; i <= 9; i++)
  {
    dp[1][i] = 1;
  }

  for (int i = 2; i <= n; i++)
  {
    dp[i][0] = dp[i - 1][1];
    dp[i][9] = dp[i - 1][8];
    for (int j = 1; j <= 8; j++)
    {
      dp[i][j] = (dp[i - 1][j - 1] + dp[i - 1][j + 1]) % R;
    }
  }

  int res = 0;
  for (int j = 0; j < 10; j++)
  {
    res = (res + dp[n][j]) % R;
  }
  cout << res << endl;
}