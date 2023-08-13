#include <iostream>
#include <vector>
#include <algorithm>

#define endl '\n'
#define MAX 100

using namespace std;

int n;
int dp[MAX];
vector<pair<int, int>> v;

int main()
{
  cin >> n;
  int a, b;
  for (int i = 0; i < n; i++)
  {
    cin >> a >> b;
    v.push_back(make_pair(a, b));
  }

  sort(v.begin(), v.end());

  for (int i = 0; i < n; i++)
  {
    dp[i] = 1;
    for (int j = 0; j < i; j++)
    {
      if (v[j].second < v[i].second)
        dp[i] = max(dp[i], 1 + dp[j]);
    }
  }

  int len = 0;
  for (int i = 0; i < n; i++)
  {
    len = max(len, dp[i]);
  }
  cout << n - len << endl;
  return 0;
}