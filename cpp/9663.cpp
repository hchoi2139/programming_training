#include <iostream>
#include <vector>
#include <cmath>

#define MAX 15

using namespace std;

int n, cnt = 0;

// list of locations of deployed queens.
vector<pair<int, int>> v;

bool can_deploy(pair<int, int> g) 
{
  for (pair<int, int> p: v) 
  {
    if (p.first == g.first || p.second == g.second
        || abs(p.first - g.first) == abs(p.second - g.second)) 
    {
      return false;    
    }
  }
  return true;
}

void dfs(int nRow) {
  if (nRow == n) {
    cnt += 1;
    return;
  }
  for (int i = 0; i < n; i++) {
    if (pair<int, int> p = make_pair(nRow, i); can_deploy(p)) {
      v.push_back(p);
      dfs(nRow + 1);
      v.pop_back();
    }
  }
  return;
}

int main() {
  ios::sync_with_stdio(false);
  cin.tie(0);

  cin >> n;

  dfs(0);
  cout << cnt << endl;

  return 0;
}