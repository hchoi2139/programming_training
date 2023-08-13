#include <bits/stdc++.h>

typedef long long ll;

const ll MAX {1000000000};

using namespace std;

ll n, k;

ll numsBelow(ll a)
{
  ll res {0};
  for (int i = 1; i <= n; i++)
  {
    res += min(a / i, n);
  }
  return res;
}

int main()
{
  ios_base::sync_with_stdio(false);
  cin.tie(nullptr);
  cout.tie(nullptr);

  cin >> n >> k;
  k = min(k, MAX);

  ll low {1}, mid {-1}, high {n * n};
  ll cnt {0};
  while (low <= high)
  {
    mid = (low + high) / 2;
    cnt = numsBelow(mid);
    if (cnt >= k)
      high = mid - 1;
    else
      low = mid + 1;
  }

  cout << low << '\n';
}