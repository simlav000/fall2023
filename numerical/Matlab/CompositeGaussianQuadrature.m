function [result, numEvaluations] = compositeGaussianQuadrature(f, a, b, n)
    % f: function to integrate
    % a, b: interval limits
    % n: number of subintervals

    % Calculate subinterval width
    h = (b - a) / n;

    % Initialize result and counter
    result = 0;
    numEvaluations = 0;

    for i = 0:n-1
        % Calculate subinterval limits
        xi = a + i * h;
        xi1 = a + (i + 1) * h;
        alpha = 0.5 * (xi1 + xi);
        beta = 0.5 * (xi1 - xi);

        % Calculate points for Gaussian quadrature on the subinterval
        xPlus = alpha + beta * (sqrt(3)/3);
        xMinus = alpha + beta * (-sqrt(3)/3);

        % Apply Gaussian quadrature on the subinterval
        result = result + beta * (f(xPlus) + f(xMinus));

        % Increment the counter for function evaluations
        numEvaluations = numEvaluations + 2;
    end
end
