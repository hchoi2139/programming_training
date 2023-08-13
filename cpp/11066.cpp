#include <bits/stdc++.h>

using namespace std;

const int MAX_SIZE {500};

int T, K;

// fileSize[i]: file size of i-th C.
vector<int> fileSize(MAX_SIZE+1);

// sum[i] = fileSize[1] + ... + fileSize[i].
vector<int> sum(MAX_SIZE+1);

// dp[i][j]: min cost of merging i-th to j-th file.
// dp is upper-triangular; hence, i <= j is a prerequisite.
vector<vector<int>> dp(MAX_SIZE+1, vector<int>(MAX_SIZE+1));

int main()
{
  // I/O does not use stdio buffer.
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> T;

  for (int i = 1; i <= T; i++)
  {
    cin >> K;
    cin >> fileSize[1];  // k >= 3 guaranteed.
    dp[1][1] = fileSize[1];
    sum[1] = fileSize[1];
    for (int j = 2; j <= K; j++)
    {
      cin >> fileSize[j];
      sum[j] = sum[j-1] + fileSize[j];
      dp[j][j] = fileSize[j];
      dp[j-1][j] = fileSize[j-1] + fileSize[j];
      for (int m = j-2; m >= 1; m--)
      {
        int partSum = sum[j] - sum[m-1];
        dp[m][j] = min(dp[m+1][j] + partSum, dp[m][j-1] + partSum);
        for (int n = m + 1; n < j - 1; n++)
        {
          dp[m][j] = min(dp[m][j], dp[m][n] + dp[n+1][j] + partSum);
        }
      }
    }
    cout << dp[1][K] << '\n';
  }
}