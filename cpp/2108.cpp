#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <climits>

using namespace std;

vector<int> v;
int n;

int range() {
  return v[n - 1] - v[0];
}

int mean() {
  int sum = 0;
  for (int i = 0; i < n; i++) {
    sum += v[i];
  }
  return round(static_cast<double>(sum) / n);
}

int median() {
  return v[(n - 1) / 2];
}

int mode_2() {
  int cnt[8001] = { 0 };
  int max_cnt = INT_MIN;
  for (int i = 0; i < n; i++) {
    cnt[v[i] + 4000] += 1; 
    max_cnt = max(max_cnt, cnt[v[i] + 4000]);
  }
  bool isSecond = false;
  int mode_2;
  for (int i = 0; i < 8001; i++) {
    if (cnt[i] == max_cnt) {
      if (isSecond) {
        return i - 4000;
      } else {
        mode_2 = i - 4000;
        isSecond = true;
      }
    }
  }
  return mode_2;
}

int main() {

  cin >> n;

  int t;
  for (int i = 0; i < n; i++) {
    cin >> t;
    v.push_back(t);
  }

  sort(v.begin(), v.end());
  
  cout << mean() << '\n';
  cout << median() << '\n';
  cout << mode_2() << '\n';
  cout << range() << '\n';

  return 0;
}