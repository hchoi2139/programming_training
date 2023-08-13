#include <iostream>
#include <vector>
#include <algorithm>

#define endl '\n'
#define ll long long

using namespace std;

int n;
ll s, e;
vector<pair<ll, ll>> v;

auto cmp = [](const pair<ll, ll>& p1, const pair<ll, ll>& p2)
{
  if (p1.second == p2.second)
    return p1.first < p2.first;

  return p1.second < p2.second;
};

int main()
{
  cin >> n;
  for (int i = 0; i < n; i++)
  {
    cin >> s >> e;
    v.push_back({s, e});
  }

  sort(v.begin(), v.end(), cmp);

  int prevEnd = -1, cnt = 0;
  for (int i = 0; i < n; i++)
  {
    if (prevEnd <= v[i].first)
    {
      cnt += 1;
      prevEnd = v[i].second;
    }
  }

  cout << cnt << endl;
  return 0;
}