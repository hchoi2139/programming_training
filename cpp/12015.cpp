#include <bits/stdc++.h>

using namespace std;

int n;
vector<int> v;

int main()
{
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> n;

  // Add first element.
  int t;
  cin >> t;
  v.push_back(t);

  for (int i = 1; i < n; i++)
  {
    cin >> t;
    
    if (v.back() < t)
      v.push_back(t);
    else
    {
      auto it = lower_bound(v.begin(), v.end(), t);
      *it = t;
    }
  }

  cout << v.size() << '\n';
}