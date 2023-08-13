#include <iostream>

#define endl '\n'
#define N 1000000
#define ll long long

using namespace std;

ll dp[N + 1];
int n;

int main()
{
  ios::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> n;

  dp[0] = 0;
  dp[1] = 1;
  dp[2] = 2;

  for (int i = 3; i <= n; i++)
  {
    dp[i] = (dp[i - 2] + dp[i - 1]) % 15746;
  }

  cout << dp[n] << endl;

  return 0;
}