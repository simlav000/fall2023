function [result, n_eval, num_subintervals] = trapezoid_rule(func, a, b, ep)
    % Set default values for n, I_prev, and n_eval
    n = 1;
    I_prev = 0.5 * (b - a) * (func(a) + func(b));
    n_eval = 2; % Two function evaluations for endpoints

    % Call the recursive trapezoid_rule function
    [result, n_eval, num_subintervals] = trapezoid_rule_rec(func, a, b, ep, n, I_prev, n_eval);
end

function [result, n_eval, num_subintervals] = trapezoid_rule_rec(func, a, b, ep, n, I_prev, n_eval)
    h = (b - a) / 2^n;

    I_new = 0;

    % summation
    for i = 1:(2^(n - 1))
        I_new = I_new + func(a + (2 * i - 1) * h);
        n_eval = n_eval + 1; % Increment function evaluations
    end
    I_new = I_new * h;
    I_new = I_new + 0.5 * I_prev;

    if abs(I_new - I_prev) <= ep
        result = I_new;
        num_subintervals = 2^n; % Number of subintervals created
    else
        [result, n_eval, num_subintervals] = trapezoid_rule_rec(func, a, b, ep, n + 1, I_new, n_eval);
    end
end
