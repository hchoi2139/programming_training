#include <iostream>
#include <limits.h>

using namespace std;

int main() {
  int n;
  cin >> n;

  int min = INT_MAX, max = INT_MIN;
  int in;
  for (int i = 0; i < n; i++) {
    cin >> in;
    if (in < min)
      min = in;
    if (in > max)
      max = in;
  }
  cout << min * max << "\n";
  return 0;
}