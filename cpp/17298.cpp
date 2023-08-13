#include <iostream>
#include <stack>

using namespace std;

inline constexpr int MAX = 1000000;

int n = 0;
int arr[MAX] = {0,};
int nge[MAX] = {0,};
stack<int> stk;

int main()
{
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> n;
  for (int i = 0; i < n; i++)
  {
    cin >> arr[i];

    // The arr elements corresponding to the indices stored in stk, from top to
    // bottom, are in decreasing order.
    while (!stk.empty() && arr[stk.top()] < arr[i])
    {
      nge[stk.top()] = arr[i];
      stk.pop();
    }

    // Push i to stk for further investigation.
    stk.push(i);
  }

  // All remaining arr indices in stk has no nge.
  while (!stk.empty())
  {
    nge[stk.top()] = -1;
    stk.pop();
  }

  for (int i = 0; i < n; i++)
  {
    cout << nge[i] << ' ';
  }
  cout << endl;
}