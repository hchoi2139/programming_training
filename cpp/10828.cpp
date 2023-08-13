#include <iostream>
#include <string>
#include <stack>

#define endl "\n"

using namespace std;

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);

  int n;
  cin >> n;

  stack<int> stack;
  string s;
  int num;
  for (int i = 0; i < n; i++) {
    cin >> s;
    if (s == "push") {
      cin >> num;
      stack.push(num);
    } else if (s == "pop") {
      if (stack.empty()) {
        cout << "-1" << endl;
      } else {
        cout << stack.top() << endl;
        stack.pop();
      }
    } else if (s == "size") {
      cout << stack.size() << endl;
    } else if (s == "empty") {
      if (stack.empty()) {
        cout << 1 << endl;
      } else {
        cout << 0 << endl;
      }
    } else if (s == "top") {
      if (stack.empty()) {
        cout << -1 << endl;
      } else {
        cout << stack.top() << endl;
      }
    }
  }

  return 0;
}