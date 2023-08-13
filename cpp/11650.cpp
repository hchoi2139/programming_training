#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

bool compare(pair<int, int> p1, pair<int, int> p2) {
  return p1.first == p2.first ? p1.second < p2.second : p1.first < p2.first;
}

int main() {
  int n;
  cin >> n;

  vector<pair<int, int>> v;
  int a, b;
  for (int i = 0; i < n; i++) {
    cin >> a >> b;
    v.push_back(make_pair(a, b));
  }
  
  sort(v.begin(), v.end(), compare);

  for (int i = 0; i < n; i++) {
    cout << v[i].first << ' ' << v[i].second << "\n";
  }

  return 0; 
}