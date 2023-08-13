#include <iostream>

int main() {
  int n;
  int arr[100];
  int v;
  int cnt = 0;
  
  scanf("%d", &n);

  for (int i = 0; i < n; i++) {
    scanf("%d", &arr[i]);
  }
  scanf("%d", &v);

  for (int i = 0; i < n; i++) {
    if (arr[i] == v)
      cnt += 1;
  }
  printf("%d\n", cnt);
  return 0;
}