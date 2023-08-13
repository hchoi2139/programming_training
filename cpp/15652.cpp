#include <iostream>

#define endl '\n'
#define MAX 9

using namespace std;

int n, m;
int arr[MAX] = { 0, };

void dfs(int cnt, int prev) {
  if (cnt == m) {
    for (int i = 0; i < m; i++) {
      cout << arr[i] << ' ';
    }
    cout << endl;
  } else {
    for (int i = prev; i <= n; i++) {
      arr[cnt] = i;
      dfs(cnt + 1, i);
    }
  }
}

int main() {
  ios::sync_with_stdio(false);
  cin.tie(0);

  cin >> n >> m;
  dfs(0, 1);
  
  return 0;
}