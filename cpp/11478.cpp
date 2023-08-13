#include <iostream>
#include <string>
#include <set>

using namespace std;

int main() {
  string s;
  cin >> s;

  set<string> ps;
  for (int i = 0; i < s.size(); i++) {
    string p = "";
    for (int j = i; j < s.size(); j++) {
      p.push_back(s[j]);
      ps.insert(p);
    }
  }

  cout << ps.size() << "\n";

  return 0;
}