#include <iostream>
#include <algorithm>

#define endl '\n'
#define N 1000

using namespace std;

int n;
int seq[N];
int dp[N];  // dp[i]: length of longest increasing subsequence which ends at i-th index.

int main()
{
  cin >> n;
  for (int i = 0; i < n; i++)
  {
    cin >> seq[i];
  }

  for (int i = 0; i < n; i++)
  {
    dp[i] = 1;
    for (int j = 0; j < i; j++)
    {
      if (seq[i] > seq[j])
        dp[i] = max(dp[i], 1 + dp[j]);
    }
  }

  int res = 0;
  for (int i = 0; i < n; i++)
  {
    res = max(res, dp[i]);
  }
  cout << res << endl;
  return 0;
}