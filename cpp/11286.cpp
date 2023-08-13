#include <bits/stdc++.h>

using namespace std;

auto cmp = [](int a, int b) {
  if (abs(a) == abs(b))
    return a > b;
  return abs(a) > abs(b);
};

priority_queue<int, vector<int>, decltype(cmp)> heap(cmp);
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
      if (heap.empty())
        cout << 0 << '\n';
      else
      {
        cout << heap.top() << '\n';
        heap.pop();
      }
    }
    else
      heap.push(x);
  }
  return 0;
}