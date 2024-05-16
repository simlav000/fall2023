function det_A = determinant(A)
% lupp: LU factorization with partial pivoting
% 
% input:  A  
% output: L, U and P such that PA = LU
%
n = size(A,1);
P = eye(n);

for k = 1:n-1
   [maxval, maxindex] = max(abs(A(k:n,k)));
   q = maxindex + k - 1;
   if maxval == 0, error('A is singular'), end
   if q ~= k
       A([k,q],:) = A([q,k],:); 
       P([k,q],:) = P([q,k],:);
   end
   i = k+1:n;
   A(i,k) = A(i,k)/A(k,k);
   A(i,i) = A(i,i) - A(i,k)*A(k,i); 
end

U = triu(A);

% Idea:
% Given PA = LU, we can use determinant properties to obtain:
% det(PA) = det(LU)
% det(P) * det(A) = det(L) * det(U)
% det(A) = det(L) * det(U) / det(P)
% by definition, L is lower triangular with 1's as its diagonals
% and so det(L) = 1

det_L = 1; 
det_U = prod(diag(U));

% det(P) is +/- 1 depending on the number of swapped pairs of rows
% from the identity. We can count the number of swapped pairs by
% subtracting P from I and counting the number of rows whose entries sum to
% 1 or greater. If we have an even number of such rows, then we have an odd
% number of row swaps and thus the determinant is -1. Else, we have a
% determinant of 1.

row_swaps = 0;

for i = 1:n
    if P(i, i) == 0
        % Find the row with a '1' in the current column and swap it
        j = find(P(i+1:end, i), 1) + i;
        P([i, j], :) = P([j, i], :);
        row_swaps = row_swaps + 1;
    end
end

if mod(row_swaps, 2) == 0
    det_P = 1;  % Even number of row swaps
else
    det_P = -1;  % Odd number of row swaps
end


det_A = det_L * det_U / det_P;
end
