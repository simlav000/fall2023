#include <math.h>
#include <stdio.h>

double sequence(int n, double x) {
    double x_n = pow(2, n + 1) * (sqrt(1 + pow(2, -n) * x) - 1);
    double ln2 = log(2);
    double diff = x_n - ln2;
    printf("%d %3.6e %3.6e %3.6e \n", n, x_n, ln2, diff);
    if (n <= 60) sequence(n+1, x_n);
}

void main() {
    printf("n     x_n         ln(2)         diff\n");
    double x_n = sequence(1, 1.0);
}