#include <iostream>
#include <algorithm>

#define endl '\n'
#define ll long long
#define P_MAX 1000000000
#define N_MAX 100000

using namespace std;

int n;
ll dist[N_MAX];
ll price[N_MAX];

int main()
{
  cin >> n;
  for (int i = 0; i < n - 1; i++)
  {
    cin >> dist[i];
  }
  for (int i = 0; i < n; i++)
  {
    cin >> price[i];
  }

  ll minPrice = P_MAX;
  ll sum = 0;
  for (int i = 0; i < n - 1; i++)
  {
    minPrice = min(minPrice, price[i]);
    sum += minPrice * dist[i];
  }

  cout << sum << endl;
  return 0;
}