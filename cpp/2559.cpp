#include <iostream>
#include <algorithm>

#define endl '\n'
#define MAX 100000

using namespace std;

int n, k;
int arr[MAX];
int dp[MAX];

int main()
{
  ios::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> n >> k;
  for (int i = 0; i < n; i++)
  {
    cin >> arr[i];
  }

  for (int i = 0; i < k; i++)
  {
    dp[0] += arr[i];
  }

  int res = dp[0];
  for (int i = 1; i < n - k + 1; i++)
  {
    dp[i] = dp[i - 1] + arr[i + k - 1] - arr[i - 1];
    res = max(res, dp[i]);
  }

  cout << res << endl;
  return 0;
}