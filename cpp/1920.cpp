#include <bits/stdc++.h>

using namespace std;

const int MAX {100000};

int n, m;
int arr[MAX];

int binarySearch(int t)
{
  int s = 0;
  int e = n - 1;
  int m;

  while (s <= e)
  {
    m = (s + e) / 2;
    if (arr[m] == t)
      return 1;
    else if (arr[m] < t)
      s = m + 1;
    else
      e = m - 1;
  }
  return 0;
}

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
    cout << binarySearch(t) << '\n';
  }

  return 0;
}