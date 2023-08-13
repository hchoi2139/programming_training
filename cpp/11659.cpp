#include <iostream>

#define MAX 100000
#define ll long long
#define endl '\n'

using namespace std;

int n, m;
ll arr[MAX + 1];
ll dp[MAX + 1];  // dp[i] = arr[1] + arr[2] + ... + arr[i]

int main()
{
  ios::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> n >> m;
  for (int i = 1; i <= n; i++)
  {
    cin >> arr[i];
    dp[i] = dp[i - 1] + arr[i];
  }

  int i, j;
  for (int cnt = 0; cnt < m; cnt++)
  {
    cin >> i >> j;
    cout << dp[j] - dp[i - 1] << endl; 
  }

  return 0;
}