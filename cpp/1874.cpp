#include <iostream>
#include <stack>
#include <vector>

#define endl "\n"

using namespace std;

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);

  int n;
  cin >> n;

  vector<char> v;
  stack<int> s;

  int x, top = 1;
  for (int i = 0; i < n; i++) {
    cin >> x;

    while (top <= x) {
      s.push(top);
      v.push_back('+');
      top += 1;
    }

    if (s.top() == x) {
      s.pop();
      v.push_back('-');
    } else {
      cout << "NO" << endl;
      return 0;
    }
  }

  for (int i = 0; i < v.size(); i++) {
    cout << v[i] << endl;
  }

  return 0;
}