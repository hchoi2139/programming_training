#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

bool cmp(pair<int, string> p1, pair<int, string> p2) {
  return p1.first < p2.first;
}

int main() {
  int n;
  cin >> n;

  vector<pair<int, string>> ps;
  int age; 
  string name;
  for (int i = 0; i < n; i++) {
    cin >> age >> name;
    ps.push_back(make_pair(age, name));
  }

  stable_sort(ps.begin(), ps.end(), cmp);

  for (int i = 0; i < n; i++) {
    cout << ps[i].first << ' ' << ps[i].second << "\n";
  }

  return 0;
}