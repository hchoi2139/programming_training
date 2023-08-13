#include <iostream>
#include <algorithm>

#define MAX 1000
#define endl '\n'

using namespace std;

int n;
int dp[MAX][3];
int cost[3];

int main()
{
  cin >> n;
  for (int i = 0; i < n; i++) {
    cin >> cost[0] >> cost[1] >> cost[2];
    dp[i][0] = min(dp[i - 1][1], dp[i - 1][2]) + cost[0];
    dp[i][1] = min(dp[i - 1][0], dp[i - 1][2]) + cost[1];
    dp[i][2] = min(dp[i - 1][0], dp[i - 1][1]) + cost[2];
  }
  cout << min({dp[n - 1][0], dp[n - 1][1], dp[n - 1][2]}) << endl;

  return 0;
}