#include <iostream>

#define endl '\n'
#define N 100000

#define max(x, y) x > y ? x : y

using namespace std;

int n;
int arr[N];
int dp[N];

int main()
{
  cin >> n;

  for (int i = 0; i < n; i++)
  {
    cin >> arr[i];
  }

  int res = arr[0];
  dp[0] = arr[0];
  for (int i = 0; i < n; i++)
  {
    dp[i] = max(dp[i - 1] + arr[i], arr[i]);
    res = max(dp[i], res);
  }

  cout << res << endl;

  return 0;
}