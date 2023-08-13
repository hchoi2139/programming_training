#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <algorithm>

using namespace std;

map<string, int> word_2_freq;

bool cmp(string s1, string s2) {
  if (word_2_freq[s1] == word_2_freq[s2] && s1.size() == s2.size()) {
    return s1 < s2;
  }
  if (word_2_freq[s1] == word_2_freq[s2]) {
    return s1.size() > s2.size();
  }
  return word_2_freq[s1] > word_2_freq[s2];
}

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);

  int n, m;
  cin >> n >> m;

  vector<string> v;

  string s;
  for (int i = 0; i < n; i++) {
    cin >> s;
    if (s.size() < m) {
      continue;
    }
    if (!word_2_freq[s]) {
      word_2_freq[s] = 1;
      v.push_back(s);
    } else {
      word_2_freq[s]++;
    }
  }

  sort(v.begin(), v.end(), cmp);

  for (string s : v) {
    cout << s << "\n";
  }

  return 0;
}