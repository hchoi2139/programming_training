#include <iostream>

#define endl '\n'
#define MAX 100

using namespace std;

int N, M, K;
int a[MAX][MAX], b[MAX][MAX];

int main()
{
  cin >> N >> M;
  for (int i = 0; i < N; i++)
  {
    for (int j = 0; j < M; j++)
    {
      cin >> a[i][j];
    }
  }
  cin >> M >> K;
  for (int i = 0; i < M; i++)
  {
    for (int j = 0; j < K; j++)
    {
      cin >> b[i][j];
    }
  }

  int sum = 0;
  for (int i = 0; i < N; i++)
  {
    for (int j = 0; j < K; j++)
    {
      for (int k = 0; k < M; k++)
      {
        sum += a[i][k] * b[k][j];
      }
      cout << sum << ' ';
      sum = 0;
    }
    cout << endl;
  }

  return 0;
}