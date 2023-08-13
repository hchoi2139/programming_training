#include <iostream>

#define endl '\n'
#define MAX 1024

using namespace std;

int n, m;
int x1, y1, x2, y2;
int dp[MAX + 1][MAX + 1];

int main()
{
  ios::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> n >> m;

  int in;
  for (int i = 1; i <= n; i++)
  {
    for (int j = 1; j <= n; j++)
    {
      cin >> in;
      dp[i][j] = dp[i - 1][j] + dp[i][j - 1] - dp[i - 1][j - 1] + in;
    } 
  }

  for (int i = 0; i < m; i++)
  {
    cin >> x1 >> y1 >> x2 >> y2;
    cout << dp[x2][y2] - dp[x1 - 1][y2] - dp[x2][y1 - 1]+ dp[x1 - 1][y1 - 1] << endl;
  }
}