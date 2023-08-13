#include <bits/stdc++.h>

using namespace std;

int n, c;
vector<int> pos;

int getMaxInterval()
{
  int minDist = 1;
  int midDist = -1;
  int maxDist = pos.back() - pos.front();

  int res = 0;
  while (minDist <= maxDist)
  {
    midDist = (minDist + maxDist) / 2;

    int nRouter = 1;
    int prev = pos[0];
    for (int i = 0; i < n; i++)
    {
      if (pos[i] - prev >= midDist)
      {
        nRouter += 1;
        prev = pos[i];
      }
    }

    if (nRouter >= c)
    {
      res = max(res, midDist);
      minDist = midDist + 1;
    }
    else
      maxDist = midDist - 1;
  }

  return res;
}

int main()
{
  ios_base::sync_with_stdio(false);
  cin.tie(nullptr);
  cout.tie(nullptr);

  cin >> n >> c;
  int loc;
  for (int i = 0; i < n; i++)
  {
    cin >> loc;
    pos.push_back(loc);
  }

  sort(pos.begin(), pos.end());

  cout << getMaxInterval() << '\n';
  return 0;
}