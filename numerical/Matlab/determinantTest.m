function test = determinantTest()

test = 0;
for i = 1: 1000
    m = randi([1, 25]);
    % Create the identity matrix
    I = eye(m);

    % Randomly shuffle the rows of the identity matrix to generate a permutation matrix
    A = I(randperm(m), :);

    if determinant(A) ~= det(A)
        test = test + 1;
    end
end
