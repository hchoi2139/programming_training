#include <iostream>
#include <algorithm>
#include <vector>
#include <map>
#include <limits.h>

using namespace std;

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  int n;
  cin >> n;

  vector<int> o, v;
  int e;
  for (int i = 0; i < n; i++) {
    cin >> e;
    o.push_back(e);
    v.push_back(e);
  }
  sort(v.begin(), v.end(), less<int>());

  map<int, int> num_to_rank;
  int rank = 0;
  int prev = INT_MIN;
  for (const int i : v) {
    if (prev != i) {
      num_to_rank.insert({i, rank});
      prev = i;
      rank += 1;
    }
  }

  for (int i = 0; i < n; i++) {
    cout << num_to_rank[o[i]] << ' ';
  }

  return 0;
}