#include <bits/stdc++.h>

using namespace std;

const int MAX {500000};

int n, m;
int arr[MAX];

int main()
{
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> n;
  for (int i = 0; i < n; i++)
  {
    cin >> arr[i];
  }

  sort(arr, arr + n);

  cin >> m;
  int t;
  for (int i = 0; i < m; i++)
  {
    cin >> t;
    cout << upper_bound(arr, arr + n, t) - lower_bound(arr, arr + n, t) << ' ';
  }
}