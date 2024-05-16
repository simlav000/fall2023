#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#define PI 3.14159265358979323846

// function to integrate
double erf(double x) {
    return 2 * exp(-x * x) / sqrt(PI);
}

// Adaptive Simpson's method for numerical integration
double adapt_simpson(double (*func)(double), double a, double b, double delta, int *level, double level_max) {
    double I;
    double h = b - a;
    double c = (a + b) / 2;
    double I1 = h * (func(a) + 4 * func(c) + func(b)) / 6;
    (*level)++;
    double d = (a + c) / 2;
    double e = (c + b) / 2;
    double I2 = h * (func(a) + 4 * func(d) + 2 * func(c) + 4 * func(e) + func(b)) / 12;

    if (*level >= level_max) {
        return I = I2;
    } else {
        if (fabs(I2 - I1) <= 15 * delta) {
            I = I2 + (I2 - I1) / 15;
        } else {
            I = adapt_simpson(func, a, c, delta / 2, level, level_max) +
                adapt_simpson(func, c, b, delta / 2, level, level_max);     
        }
    }
    return I;
}

int main() {
    double a = 0;
    double b = 3;
    double delta = 1E-5;
    int level_max = 50;

    int level = 0; // Initialize level to 0

    double I = adapt_simpson(erf, a, b, delta, &level, level_max);

    printf("erf(3): %lf\niterations: %d\n", I, level);
    return 0;
}
