#include <iostream>
#include <cmath>

#define endl "\n"

using namespace std;

void cantor(const int n) {
  int size = pow(3, n);

  if (size == 1) {
    cout << '-';
    return;
  }

  cantor(n - 1);
  for (int i = 0; i < size / 3; i++) {
    cout << ' ';
  }
  cantor(n - 1);
}

int main() {
  int n;

  while (cin >> n) {
    cantor(n);
    cout << endl;
  }

  return 0;
}