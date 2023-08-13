#include <iostream>
#include <vector>

#define endl '\n'

using namespace std;

int main()
{
  int n, k;
  cin >> n >> k;

  vector<int> v(n);
  for (int i = 0; i < n; i++)
  {
    cin >> v[i]; 
  }

  int d, r, cnt = 0;
  for (int i = n - 1; i >= 0; i--)
  {
    d = k / v[i];
    r = k % v[i];
    cnt += d;
    k = r;
    if (k == 0)
      break;
  }

  cout << cnt << endl;
  return 0;
}