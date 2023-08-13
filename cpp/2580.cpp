#include <iostream>
#include <vector>
#include <set>

#define N 9
#define endl "\n"

using namespace std;

int board[N][N];
vector<pair<int, int>> zeros;

void printBoard()
{
  for (int i = 0; i < N; i++) 
  {
    for (int j = 0; j < N; j++)
    {
      cout << board[i][j] << ' ';
    }
    cout << endl;
  }
}

bool check(pair<int, int> p, int n)
{
  int r = p.first;
  int c = p.second;
  for (int i = 0; i < N; i++)
  {
    // Check whether n appears in the row.
    if (board[r][i] == n)
    {
      return true;
    }
    // Check whether n appears in the column.
    if (board[i][c] == n)
    {
      return true;
    }
  }
  // Check whether n appears in the 3*3 square.
  int tlr = r / 3 * 3;
  int tlc = c / 3 * 3;
  if (board[tlr][tlc] == n || board[tlr][tlc + 1] == n || board[tlr][tlc + 2] == n ||
      board[tlr + 1][tlc] == n || board[tlr + 1][tlc + 1] == n || board[tlr + 1][tlc + 2] == n ||
      board[tlr + 2][tlc] == n || board[tlr + 2][tlc + 1] == n || board[tlr + 2][tlc + 2] == n)
  {
    return true;
  }
  // Safe to deploy.
  return false;
}

bool dfs(int n)
{
  if (n == zeros.size())
  {
    printBoard();
    return true;
  }
  pair<int, int> p = zeros[n];
  for (int i = 1; i <= N; i++)
  {
    if (!check(p, i))
    {
      board[p.first][p.second] = i;
      if (dfs(n + 1))
      {
        return true;
      }
      board[p.first][p.second] = 0;
    }
  }
  return false;
}

int main()
{
  ios::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  for (int i = 0; i < N; i++) 
  {
    for (int j = 0; j < N; j++) 
    {
      cin >> board[i][j];
      if (board[i][j] == 0) 
      {
        zeros.push_back(make_pair(i, j));
      }
    }
  }

  dfs(0);

  return 0;
}