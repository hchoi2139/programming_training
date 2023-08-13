#include <iostream>
#include <string>

using namespace std;

int main()
{
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  cout.tie(NULL);

  string s, explode, inspect = "";
  cin >> s >> explode;

  int lenS = s.length();
  int lenE = explode.length();

  for (int i = 0; i < lenS; i++)
  {
    inspect += s[i];

    if (inspect.length() < lenE)
      continue;

    // inspect.length() >= lenE guaranteed.
    // Investiage and erase if last lenE elements of inspect matches explode.
    if (inspect.substr(inspect.length() - lenE) == explode)
      inspect.erase(inspect.end() - lenE, inspect.end());
  }

  if (inspect.empty())
    cout << "FRULA" << '\n';
  else
    cout << inspect << '\n';
}