function X = solveLinearSystem(A, B)
    % Perform LU decomposition
    [L, U, P] = lu(A);

    % Get the dimensions of A and B
    n = size(A, 1);
    m = size(B, 2); % Number of columns in B

    % Initialize matrices to store the results
    C = zeros(n, m);
    X = zeros(n, m);

    % Solve for C in L*C = P*B
    for j = 1:m
        for i = 1:n
            C(i, j) = (P(i, :) * B(:, j) - L(i, 1:i-1) * C(1:i-1, j)) / L(i, i);
        end
    end

    % Solve for X in U*X = C
    for j = 1:m
        for i = n:-1:1
            X(i, j) = (C(i, j) - U(i, i+1:n) * X(i+1:n, j)) / U(i, i);
        end
    end
end

