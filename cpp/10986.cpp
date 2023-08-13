#include <iostream>

#define endl '\n'
#define ll long long

using namespace std;

int n, m, mSum = 0;
// Setting to int array leads to overflow in line 30.
// Can use casting but slow.
ll cnt[1000];

int main()
{
  ios::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> n >> m;
  int tmp;
  for (int i = 0; i < n; i++)
  {
    cin >> tmp;
    mSum = (mSum + tmp) % m;
    cnt[mSum] += 1;
  }

  ll res = 0;
  for (int i = 0; i < m; i++)
  {
    res += cnt[i] * (cnt[i] - 1) / 2;
  }
  cout << res + cnt[0] << endl;
}