function result = test(n)
    n = n + 1;
    result = inner(n);
    function result = inner(n)
        result = n + 1;
    end
end