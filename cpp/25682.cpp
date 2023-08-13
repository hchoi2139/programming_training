#include <iostream>
#include <string>
#include <algorithm>
#include <climits>

#define endl '\n'
#define MAX 2000

using namespace std;

int n, m, k;
int wBoard[MAX+1][MAX+1];
int bBoard[MAX+1][MAX+1];
string s;

int main()
{
  ios::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> n >> m >> k;

  for (int i = 1; i <= n; i++)
  {
    cin >> s;
    for (int j = 1; j <= m; j++)
    {
      wBoard[i][j] = wBoard[i-1][j] + wBoard[i][j-1] - wBoard[i-1][j-1];
      bBoard[i][j] = bBoard[i-1][j] + bBoard[i][j-1] - bBoard[i-1][j-1];
      if ((i + j) % 2 == 0)
      {
        if (s[j - 1] == 'B')
          wBoard[i][j] += 1;
        else
          bBoard[i][j] += 1;
      }
      else
      {
        if (s[j - 1] == 'B')
          bBoard[i][j] += 1;
        else
          wBoard[i][j] += 1;
      }
    }
  }

  int res = INT_MAX;
  for (int i = 1; i <= n - k + 1; i++)
  {
    for (int j = 1; j <= m - k + 1; j++)
    {
      res = min({ 
        res, 
        wBoard[i+k-1][j+k-1] - wBoard[i+k-1][j-1] - wBoard[i-1][j+k-1] + wBoard[i-1][j-1],
        bBoard[i+k-1][j+k-1] - bBoard[i+k-1][j-1] - bBoard[i-1][j+k-1] + bBoard[i-1][j-1]
      });
    }
  }

  cout << res << endl;
  return 0;
}