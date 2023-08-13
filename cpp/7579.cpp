#include <iostream>

using namespace std;

const int APP_MAX {100};
const int COST_MAX {100};

int n, m, costSum = 0;
int mem[APP_MAX + 1];
int cost[APP_MAX + 1];
// dp[i][j]: max memory achievable by using j cost, A[1] to A[i].
int dp[APP_MAX + 1][APP_MAX * COST_MAX + 1];

int main()
{
  cin >> n >> m;

  for (int i = 1; i <= n; i++)
  {
    cin >> mem[i];
  }

  for (int i = 1; i <= n; i++)
  {
    cin >> cost[i];
    costSum += cost[i];
  }

  for (int i = 1; i <= n; i++)
  {
    for (int j = 0; j <= costSum; j++)
    {
      if (j >= cost[i])
        dp[i][j] = max(dp[i][j], dp[i - 1][j - cost[i]] + mem[i]);
      
      dp[i][j] = max(dp[i][j], dp[i - 1][j]);
    }
  }

  for (int j = 1; j <= costSum; j++)
  {
    if (dp[n][j] >= m) {
      cout << j << '\n';
      break;
    } 
  }
}