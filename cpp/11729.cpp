#include <iostream>

#define endl "\n"

using namespace std;

void hanoi(int start, int by, int to, int n) {
  if (n == 1) {
    cout << start << ' ' << to << endl;
  } else {
    hanoi(start, to, by, n - 1);
    cout << start << ' ' << to << endl;
    hanoi(by, start, to, n - 1);
  }
}

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(0);

  int n;
  cin >> n;
  cout << (1 << n) - 1 << endl;
  hanoi(1, 2, 3, n);

  return 0;
}