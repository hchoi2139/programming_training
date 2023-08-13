#include <bits/stdc++.h>

using namespace std;

priority_queue<int> maxHeap;
int n, x;

int main()
{
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> n;
  for (int i = 0; i < n; i++)
  {
    cin >> x;

    if (x == 0)
    {
      if (maxHeap.empty())
        cout << 0 << '\n';
      else
      {
        cout << maxHeap.top() << '\n';
        maxHeap.pop();
      }
    }
    else
      maxHeap.push(x);
  }

  return 0;
}