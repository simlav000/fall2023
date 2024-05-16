A = hilb(10);

for repetition = 1:10
    X_t = randn(10, 4);
    B = A * X_t;
    X_c = solveLinearSystem(A, B);
    condition_number_times_epsilon = cond(A) * eps;
    epsilon = eps;
    
    fprintf('Repetition %d:\n', repetition);
    
    for j = 1:4
        error = norm(X_c(:,j) - X_t(:,j), 2) / norm(X_t(:,j), 2);
        relative_residual = norm(B(:,j) - A * X_c(:,j), 2) / (norm(A, 2) * norm(X_c(:,j), 2));
        
        fprintf('   Result %d:\n', j);
        fprintf('      Error: %e\n', error);
        fprintf('      Relative Residual: %e\n', relative_residual);
    end
    
    fprintf('   Condition Number Times Epsilon: %e\n', condition_number_times_epsilon);
    fprintf('   Epsilon: %e\n', epsilon);
    fprintf('\n');
end
        