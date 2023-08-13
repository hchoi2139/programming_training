#include <iostream>

#define endl '\n'
#define MAX 128

using namespace std;

int n, nWhite = 0, nBlack = 0;
int paper[MAX+1][MAX+1];
int sum[MAX+1][MAX+1];

// Returns true if the rectangle of upper-left corner paper[r1][c1] and 
// lower-right corner paper[r2][c2] is in same colour.
bool inSameColour(int r1, int c1, int r2, int c2)
{
  int partSum = sum[r2][c2] - sum[r1-1][c2] - sum[r2][c1-1] + sum[r1-1][c1-1];
  return partSum == 0 || partSum == (r2 - r1 + 1) * (c2 - c1 + 1);
}

void solve(int r1, int c1, int r2, int c2)
{
  if (inSameColour(r1, c1, r2, c2))
  {
    paper[r1][c1] == 0 ? nWhite += 1 : nBlack += 1;
    return;
  }
  int midR = (r1 + r2 - 1) / 2;
  int midC = (c1 + c2 - 1) / 2;
  solve(r1, c1, midR, midC);
  solve(r1, midC + 1, midR, c2);
  solve(midR + 1, c1, r2, midC);
  solve(midR + 1, midC + 1, r2, c2);
}

int main()
{
  cin >> n;
  for (int i = 1; i <= n; i++)
  {
    for (int j = 1; j <= n; j++)
    {
      cin >> paper[i][j];
      sum[i][j] = sum[i-1][j] + sum[i][j-1] - sum[i-1][j-1] + paper[i][j];
    }
  }

  solve(1, 1, n, n);
  cout << nWhite << endl;
  cout << nBlack << endl;
  return 0;
}