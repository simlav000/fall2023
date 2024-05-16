#include <stdio.h>
#include <math.h>

int main() /* approximating derivatives */
{
    int n; double x, h, approx, exact, error;
    x = 1.0;
    h = 1.0;
    n = 0;
    printf("    h       approx       exact        error \n");

    while (n < 20) {
        n++;
        h = h/10; 
        /* we wish to observe the effect of bringing h closer to x.
        We do this by making it an order of magnitude smaller per iteration*/
        approx = (sin(x + h) - sin(x)) / h; /* approximation */
        exact = cos(x);
        error = approx - exact;
        printf("%3.1e %3.6e %3.6e %3.6e \n", h, approx, exact, error);
    }
}

/*
    h       approx       exact       error 
1.0e-01 4.973638e-01 5.403023e-01 -4.293855e-02 
1.0e-02 5.360860e-01 5.403023e-01 -4.216325e-03 
1.0e-03 5.398815e-01 5.403023e-01 -4.208255e-04 
1.0e-04 5.402602e-01 5.403023e-01 -4.207445e-05 
1.0e-05 5.402981e-01 5.403023e-01 -4.207362e-06 
1.0e-06 5.403019e-01 5.403023e-01 -4.207468e-07 
1.0e-07 5.403023e-01 5.403023e-01 -4.182769e-08 
1.0e-08 5.403023e-01 5.403023e-01 -2.969885e-09 
1.0e-09 5.403024e-01 5.403023e-01 5.254127e-08 
1.0e-10 5.403022e-01 5.403023e-01 -5.848104e-08 
1.0e-11 5.403011e-01 5.403023e-01 -1.168704e-06 
1.0e-12 5.403455e-01 5.403023e-01 4.324022e-05 
1.0e-13 5.395684e-01 5.403023e-01 -7.339159e-04 
1.0e-14 5.440093e-01 5.403023e-01 3.706976e-03 
1.0e-15 5.551115e-01 5.403023e-01 1.480921e-02 
1.0e-16 0.000000e+00 5.403023e-01 -5.403023e-01 
1.0e-17 0.000000e+00 5.403023e-01 -5.403023e-01 
1.0e-18 0.000000e+00 5.403023e-01 -5.403023e-01 
1.0e-19 0.000000e+00 5.403023e-01 -5.403023e-01 
1.0e-20 0.000000e+00 5.403023e-01 -5.403023e-01 
*/

/* 
OBSERVATION:
    When h changes from 10^{-1} to 10^{-8}, the approximation gets BETTER,
    and the discretization error is reduced by ~10, so the error is O(h).
    When h reaches 10^{-9}, further reducing h results in a worsening 
    approximation. Eventually, when h = 10^{-16}, the approxiation becomes zero.
    Why?

EXPLANATION OF ACCURACY LOSS:
    if x = 1, and h < ε/2 ≈ 1.1 × 10^{-16} (double precision), then x + h has the same numerical value
    as x. See the following numberline:
    <---------------v----------v----------v--------------->
                    x       x + ε/2     x + ε <- next FPN number
                       x + h  <- note how x + h will get rounded back to x when it is less than x + ε/2
    this means that the numerator of the approximation, namely: f(x + h) - f(x) will cancel out, and so 
    the quantity "approx" has no digits of precision

    When h is a little bigger than ε/2, the values partially cancel. For example, suppose that the first
    10 digits of f(x + h) and f(x) are the same. Then, even though sin(x + h) and sin(x) are accurate to
    16 digits, the difference has only 6 accurate digits. This phenonmenon is caled numerical cancellation.

    In summary: when h is too small, big discretiation errors occur, while when h is too big, cancellation
    errors occur. For the function f(x) = sin(x) at x = 1, the best choice of h is about 10^{-8} (which 
    turns out to be sqrt(ε))
*/
