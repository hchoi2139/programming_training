#include <stdio.h>

int main() {
  int n;
  scanf("%d", &n);

  int score, max = -1, total = 0;
  for (int i = 0; i < n; i++) {
    scanf("%d", &score);
    if (score > max) {
      max = score;
    }
    total += score;
  }
  printf("%f\n", 100.0 * total / max / n);
  return 0;
}