#include <iostream>

#define endl '\n'
#define P 1000000007
#define MAX_N 4000000
#define ll long long

using namespace std;

ll n, k;

ll pow(ll a, ll m)
{
  if (m == 0)
    return 1;
  
  ll hPow = pow(a, m / 2) % P;
  return m % 2 == 0 ? (hPow * hPow) % P : (((hPow * hPow) % P) * a) % P;
}

ll mult(int n, int k)
{
  ll acc = 1;
  for (int i = 0; i < k; i++)
  {
    acc = (acc * (n - i)) % P;
  }
  return acc;
}

ll fact(int k)
{
  return mult(k, k);
}

int main()
{
  cin >> n >> k;
  cout << mult(n, k) * pow(fact(k), P - 2) % P << endl;
  return 0;
}