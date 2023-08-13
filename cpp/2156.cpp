#include <iostream>
#include <algorithm>

#define MAX 10000
#define endl '\n'

using namespace std;

int wine[MAX + 1];
int dp[MAX + 1];
int n;

int main()
{
  cin >> n;
  for (int i = 1; i <= n; i++)
  {
    cin >> wine[i];
  }

  dp[1] = wine[1];
  dp[2] = wine[1] + wine[2];
  dp[3] = max({wine[1] + wine[3], wine[2] + wine[3], wine[1] + wine[2]});
  for (int i = 4; i <= n; i++)
  {
    dp[i] = max(
      { dp[i - 3] + wine[i - 1] + wine[i],
        dp[i - 2] + wine[i],
        dp[i - 1] });
  }

  cout << dp[n] << endl;
  return 0;
}