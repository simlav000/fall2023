function A_inv = inverseUsingLU(A)
    % Perform LU decomposition with pivoting
    [L, U, P] = lu(A);
    
    % Get the size of the matrix
    n = size(A, 1);
    
    % Initialize the inverse matrix
    A_inv = zeros(n, n);
    
    % Solve for the inverse using forward and backward substitution
    for i = 1:n
        % Create the right-hand side vector with a 1 in the ith position
        b = zeros(n, 1);
        b(i) = 1;
        
        % Solve L(UX) = P*b for X using forward substitution
        Y = forwardSubstitution(L, P * b);
        
        % Solve UX = Y for X using backward substitution
        X = backwardSubstitution(U, Y);
        
        % Store the solution X as the ith column of the inverse matrix
        A_inv(:, i) = X;
    end
end

function X = forwardSubstitution(L, b)
    % Forward substitution to solve L*X = b
    n = length(b);
    X = zeros(n, 1);
    
    for i = 1:n
        X(i) = (b(i) - L(i, 1:i-1) * X(1:i-1)) / L(i, i);
    end
end

function X = backwardSubstitution(U, b)
    % Backward substitution to solve U*X = b
    n = length(b);
    X = zeros(n, 1);
    
    for i = n:-1:1
        X(i) = (b(i) - U(i, i+1:end) * X(i+1:end)) / U(i, i);
    end
end