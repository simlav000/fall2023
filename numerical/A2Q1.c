#include <math.h>
#include <stdio.h>

float binomial_distribution(float p, int k, int N) {
    float x = 1.0;
    for (int j = 1; j <= k; ++j) {
        x = x * (N + 1 - j);
        /*
        printf("%1.6e \n", x);
        this allowed me to determine that the value of x was overflowing
        */
    }
    
    for (int j = 1; j <= k; ++j) x = x / j;
    for (int j = 1; j <= k; ++j) x = x * p;

    float q = 1 - p;

    for (int j = k + 1; j <= N; ++j) x = x * q;

    return x;
}

float binomial_distribution_fixed(float p, int k, int N) {
    float x = 1.0;
    float B = pow(2, 100);
    float S = pow(0.5, 100);
    int counter = 0;

    for (int j = 1; j <= k; ++j) {
         x = x * (N + 1 - j);

        if (x > B) {
            x = x / B;
            counter++;
        }
    }

    for (int j = 1; j <= k; ++j) {
        x = x / j;

        if (x < S) {
            x = x * B;
            counter--;
        }    
    }

    for (int j = 1; j <= k; ++j) {
        x = x * p;
        
        if (x < S) {
            x = x * B;
            counter--;
        }
    }

    float q = 1 - p;

    for (int j = k + 1; j <= N; ++j) {
        x = x * q;

        if (x < S) {
            x = x * B;
            counter--;
        }
    } 
    
    return x * pow(B, counter);
}

void main() {
    float a = binomial_distribution(0.1, 200, 2000);
    float b = binomial_distribution_fixed(0.1, 200, 2000);
    printf("%1.6e \n", a); // inf
    printf("%1.6e \n", b); // 2.972153e-02
}