#include <iostream>
#include <string>

#define endl '\n'
#define MAX 200000

using namespace std;

string s;
int q, l, r;
char a;
int sum[26][MAX];

inline int toIndex(char c)
{
  return c - 'a';
}

int main()
{
  ios::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  cin >> s;
  cin >> q;

  sum[toIndex(s[0])][0] = 1;
  for (int i = 1; i < s.size(); i++)
  {
    for (int j = 0; j < 26; j++)
    {
      sum[j][i] = sum[j][i - 1];
    }
    sum[toIndex(s[i])][i] += 1;
  }
  
  for (int i = 0; i < q; i++)
  {
    cin >> a >> l >> r;
    cout << sum[toIndex(a)][r] - (l == 0 ? 0 : sum[toIndex(a)][l - 1]) << endl;
  }

  return 0;
}