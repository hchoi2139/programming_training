#include <iostream>

using namespace std;

const int ROW_MAX {500};
const int COL_MAX {500};

int M, N;
int map[ROW_MAX+1][COL_MAX+1];
// dp[i][j]: num of routes from (i, j) to (M, N).
// dp[i][j] = -1 means dp[i][j] is not investigated.
int dp[ROW_MAX+1][COL_MAX+1];   

// {top, bottom, left, right} direction.
int dr[4] = {0, 0, -1, 1};
int dc[4] = {1, -1, 0, 0};

// Returns num of routes from (r, c) to (M, N).
int dfs(int r, int c)
{
  // Return when reached (M, N).
  if (r == M && c == N)
    return 1;

  // Return if investigated.
  if (dp[r][c] != -1)
    return dp[r][c];

  dp[r][c] = 0;
  int nr = 0, nc = 0;
  for (int i = 0; i < 4; i++) 
  {
    nr = r + dr[i];
    nc = c + dc[i];

    // Ensure (nr, nc) is in bound.
    if (1 <= nr && nr <= M && 1 <= nc && nc <= N)
    {
      // Update dp[r][c] if map[nr][nc] < map[r][c].
      if (map[nr][nc] < map[r][c])
      {
        dp[r][c] += dfs(nr, nc);
      }
    }
  }

  return dp[r][c];
}

int main()
{
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> M >> N;

  for (int i = 1; i <= M; i++)
  {
    for (int j = 1; j <= N; j++)
    {
      cin >> map[i][j];
      dp[i][j] = -1;
    }
  }
  
  cout << dfs(1, 1) << '\n';
}