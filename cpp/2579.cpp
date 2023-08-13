#include <iostream>
#include <algorithm>

#define endl '\n'
#define MAX 300

using namespace std;

int score[MAX + 1];
int dp[MAX + 1];

int main()
{
  int n;
  cin >> n;
  for (int i = 1; i <= n; i++)
  {
    cin >> score[i];
  }

  dp[1] = score[1];
  dp[2] = score[1] + score[2];
  dp[3] = max({score[1], score[2]}) + score[3];
  for (int i = 4; i <= n; i++)
  {
    dp[i] = max({dp[i - 2], dp[i - 3] + score[i - 1]}) + score[i];
  }

  cout << dp[n] << endl;

  return 0;
}