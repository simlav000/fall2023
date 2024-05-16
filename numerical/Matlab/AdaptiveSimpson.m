function result = adapt_simpson(func, a, b, delta, level_max)
    f_a = func(a);
    f_b = func(b);
    f_c = func((a + b) / 2);
    n_eval = 3;
    level = 1;
    result = adapt_simpson_recursive(func, a, b, f_a, f_b, f_c, delta, level);

    function result = adapt_simpson_recursive(func, a, b, f_a, f_b, f_c, delta, level)
        h = b - a;
        c = (a + b) / 2;
        I1 = h * (f_a + 4 * f_c + f_b) / 6;
        level = level + 1;
        d = (a + c) / 2;
        f_d = func(d);
        e = (c + b) / 2;
        f_e = func(e);
        n_eval = n_eval + 2; % Increment for the two new function evaluations
        I2 = h * (f_a + 4 * f_d + 2 * f_c + 4 * f_e + f_b) / 12;

        if level >= level_max
            result = I2;
        else
            if abs(I2 - I1) <= 15 * delta
                result = I2 + (I2 - I1) / 15;
            else
                result_left = adapt_simpson_recursive(func, a, c, f_a, f_c, f_d, delta / 2, level);
                result_right = adapt_simpson_recursive(func, c, b, f_c, f_b, f_e, delta / 2, level);
                result = result_left + result_right;
            end
        end
    end
    disp(n_eval);
end
