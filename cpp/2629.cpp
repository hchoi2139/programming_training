#include <iostream>

using namespace std;

int nWeights, nMarbles, wMarble;
int weights[30];     // starts at index 0.
bool dp[31][15001];  // dp[i][j]: can measure j-kg marble with i weights.

void lookupValidWeights(int n, int w)
{
  // Returns if investigated nWeights weights or already investigated.
  if (n > nWeights || dp[n][w])
    return;
  
  dp[n][w] = true;
  lookupValidWeights(n + 1, w + weights[n]);
  lookupValidWeights(n + 1, abs(w - weights[n]));
  lookupValidWeights(n + 1, w);
}

int main()
{
  cin >> nWeights;
  for (int i = 0; i < nWeights; i++)
  {
    cin >> weights[i];
  }

  lookupValidWeights(0, 0);

  cin >> nMarbles;
  for (int i = 0; i < nMarbles; i++)
  {
    cin >> wMarble;
    if (wMarble > 15000)
      cout << "N ";
    else if (dp[nWeights][wMarble])
      cout << "Y ";
    else
      cout << "N ";
  }
}