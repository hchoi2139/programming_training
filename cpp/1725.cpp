#include <iostream>
#include <stack>

using namespace std;

inline constexpr int MAX = 100000;

int n, maxH, h[MAX + 2];
stack<int> s;

int main()
{
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> n;
  for (int i = 1; i <= n; i++)
  {
    cin >> h[i];
  }

  s.push(0);
  for (int i = 1; i <= n + 1; i++)
  {
    if (h[i - 1] > h[i])
    {
      while (!s.empty() && h[s.top()] > h[i])
      {
        int inspect = s.top();
        s.pop();
        maxH = max(maxH, h[inspect] * (i - s.top() - 1));
      }
    }
    s.push(i);
  }
  cout << maxH << endl;
}