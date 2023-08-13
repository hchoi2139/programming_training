#include <iostream>

using namespace std;

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  int t;
  cin >> t;

  int n, m;
  for (int i = 0; i < t; i++) {
    cin >> n >> m;
    int ans = 1, d = 1;
    for (int j = m; j > m - n; j--) {
      ans *= j;
      ans /= d;
      d += 1;
    }
    cout << ans << "\n";
  }

  return 0;
}