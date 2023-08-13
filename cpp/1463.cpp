#include <iostream> 
#include <vector>
#include <algorithm>

#define endl '\n'

using namespace std;

int main()
{
  int n;
  cin >> n;

  vector<int> dp(n + 1);
  dp[1] = 0;
  for (int i = 2; i <= n; i++)
  {
    dp[i] = 1 + dp[i - 1];
    if (i % 3 == 0) dp[i] = min(dp[i], 1 + dp[i / 3]);
    if (i % 2 == 0) dp[i] = min(dp[i], 1 + dp[i / 2]);
  }

  cout << dp[n] << endl;
  return 0;
}