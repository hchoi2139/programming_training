#include <iostream>
#include <string>

using namespace std;

int main() {
  int ts[] = { 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 10, 10, 10, 10 };
  string s;
  cin >> s;
  int total = 0;
  for (int i = 0; i < s.size(); i++) {
    total += ts[s[i] - 'A'];
  }
  cout << total;
  return 0;
}