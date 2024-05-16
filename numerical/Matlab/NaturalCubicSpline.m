x = [1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000];
y = [75.995, 91.972, 105.711, 123.203, 131.669, 150.697, 179.323, 203.212, 226.505, 249.633, 281.422];

% following the algorithm given in the spline pdf:
n = length(x);
h = zeros(1, n - 1);
b = zeros(1, n - 1);

for i = 1 : n - 1
    h(i) = x(i+1) - x(i);
    b(i) = (y(i+1) - y(i)) / h(i);
end

% Forward elimination
u = zeros(1, n - 1);
v = zeros(1, n - 1);
u(1) = 2 * (h(1) + h(2));
v(1) = 6 * (b(2) - b(1));

for i = 2 : n - 1
    mult = h(i-1) / u(i - 1);
    u(i) = 2 * (h(i-1) + h(i)) - mult * h(i-1);
    v(i) = 6 * (b(i) - b(i-1)) - mult * v(i-1);
end

% Back substitution
z = zeros(1, n);
z(n) = 0;

for i = n-1 : -1 : 1
    z(i) = (v(i) - h(i)*z(i+1)) / u(i);
end
z(1) = 0;

% Evaluate S(x) for every point and plot
t = linspace(min(x), max(x), 1000);
S = zeros(size(t));

for i = 1 : n-1
    indices = find(t >= x(i) & t <= x(i+1));
    
    % in the pdf we name it h but I don't want to override the h array so i
    % name it l
    l = x(i+1) - x(i);
    A = y(i);
    B = (y(i+1) - y(i)) / h(i) - h(i) * (z(i+1) + 2 * z(i)) / 6;
    C = z(i) / 2;
    D = (z(i+1) - z(i)) / (6 * l);

    S(indices) = A + B*(t(indices) - x(i)) + C*(t(indices) - x(i)).^2 + D*(t(indices) - x(i)).^3;
end

% Define a function for the last piece
% In order to get the values of A, B, C and D, I simply removed the
% semicolons from the above for loop and selected the last values as these
% correspond to the final cubic spline peice. From that I can define a
% function of x which I can use to extrapolate
last_spine_piece = @(x) 249.6330 + 2.7081 * (x - 1990) + 0.0706 * (x - 1990).^2 - 0.0024 * (x - 1990).^3;

% Evaluate the last piece at x = 2010
extrapolated_point = last_spine_piece(2010);

% Plot the natural cubic spline
figure;
plot(x, y, 'o', t, S, '-');
hold on;

% Plot the extrapolated point
plot(2010, extrapolated_point, 'rx', 'MarkerSize', 10); 
title('Natural Cubic Spline with Extrapolation Point');
xlabel('Year');
ylabel('Population');
legend('Data Points', 'Natural Cubic Spline', 'Extrapolation Point');
grid on;
hold off;
