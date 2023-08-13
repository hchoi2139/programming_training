#include <iostream>
#include <vector>
#include <algorithm>

#define endl '\n'

using namespace std;

int n;
vector<int> v;

int main()
{
  cin >> n;
  int tmp;
  for (int i = 0; i < n; i++)
  {
    cin >> tmp;
    v.push_back(tmp);
  }
  
  sort(v.begin(), v.end(), less<int>());

  int sum = 0, acc = 0;
  for (int i = 0; i < n; i++)
  {
    sum += v[i];
    acc += sum;
  }

  cout << acc << endl;
  return 0;
}