% Define the dataset
x = [1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000];
y = [75.995, 91.972, 105.711, 123.203, 131.669, 150.697, 179.323, 203.212, 226.505, 249.633, 281.422];

% Interpolation
x_interp = linspace(min(x), max(x), 1000);
y_interp = spline(x, y, x_interp);

% Plot the original data and the interpolated curve
plot(x, y, 'o', 'MarkerSize', 10, 'DisplayName', 'Original Data');
hold on;
plot(x_interp, y_interp, '-', 'LineWidth', 2, 'DisplayName', 'Interpolated Curve (Using spline)');
legend('show');
title('Cubic Spline Interpolation');
xlabel('Year');
ylabel('Population');
grid on;
hold off;