#include <iostream> 
#include <set>
#include <string>

using namespace std;

auto cmp = [](string s1, string s2) {
  if (s1.length() == s2.length()) {
    return s1 < s2;
  }
  return s1.length() < s2.length();
};

int main() {
  int n; 
  cin >> n;

  set<string, decltype(cmp)> ws(cmp);
  string s;
  for (int i = 0; i < n; i++) {
    cin >> s;
    ws.insert(s);
  }

  for (string w : ws) {
    cout << w << "\n";
  }

  return 0;
}