#include<stdio.h>
#include<math.h>
#define PI 3.14159265358979323846

// Global variable to keep track of function evaluations
int function_evaluations = 0;

double trapezoid_rule(double (*func)(double), double a, double b, double epsilon, int n, double previous_integral) {
    if (n == 1) {
        // first iteration starts at 1
        previous_integral = 0.5 * (b - a) * (func(a) + func(b));
    }

    double h = (b - a) / pow(2, n);

    double new_integral = 0;

    // summation
    for (int i = 1; i < pow(2, n); i++) {
        new_integral += func(a + i * h);
        function_evaluations++; // Increment the counter
    }
    new_integral *= h;
    new_integral += 0.5 * h * (func(a) + func(b));

    if (fabs(new_integral - previous_integral) <= epsilon) return new_integral;
    else {
        return trapezoid_rule(func, a, b, epsilon, n + 1, new_integral);
    }
}

// function to integrate
double erf(double x) {
    function_evaluations++; // Increment the counter
    return 2 * exp(-x * x) / sqrt(PI);
}

int main() {
    double a = 0;
    double b = 3;
    double delta = 1E-5;
    int level_max = 50;

    int level = 0; // Initialize level to 0
    function_evaluations = 0; // Reset the counter

    double result = trapezoid_rule(erf, a, b, delta, 1, 0);

    printf("erf(3): %lf\nfunction evaluations: %d\n", result, function_evaluations);
    return 0;
}
