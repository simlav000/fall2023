% Given data
x = [1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000];
y = [75.995, 91.972, 105.711, 123.203, 131.669, 150.697, 179.323, 203.212, 226.505, 249.633, 281.422];

% Create the Vandermonde matrix
n = length(x);
A = [ones(n, 1), x', x'.^2];

% Solve for the coefficients using the normal equations
coefficients = (A' * A) \ (A' * y');

% Calculate the fitted values
xFit = linspace(min(x), max(x), 1000);
yFit = coefficients(3) * xFit.^2 + coefficients(2) * xFit + coefficients(1);

% Evaluate the fit at x = 1975
xNew = 1975;
yNew = coefficients(3) * xNew^2 + coefficients(2) * xNew + coefficients(1);

% Display the coefficients
fprintf('Quadratic Coefficients: a = %.4f, b = %.4f, c = %.4f\n', coefficients(3), coefficients(2), coefficients(1));

% Generate the fitted quadratic curve
figure;
plot(x, y, 'o', 'DisplayName', 'Original Data');
hold on;
plot(xFit, yFit, 'r-', 'DisplayName', 'Fitted Quadratic Polynomial');
plot(xNew, yNew, 'ko', 'DisplayName', 'New Point at x = 1975');
legend;
xlabel('Year');
ylabel('Value');
title('Least Squares Quadratic Fit with Extra Point');
grid on;
