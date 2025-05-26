function C = batched_matmul(A, B, perm_order)
% BATCHED_MATMUL Multiplies slices of two arrays using matrix multiplication
%   C = batched_matmul(A, B, perm_order)
%   - A and B: same size arrays with shape [..., m, n, ...]
%   - perm_order: order to permute to bring matrix dimensions forward
%   - Output C has the same shape as A and B

    % Check that A and B are same size
    if ~isequal(size(A), size(B))
        error('Input arrays A and B must have the same size.');
    end

    original_size = size(A);
    inv_perm = zeros(1, numel(perm_order));
    inv_perm(perm_order) = 1:numel(perm_order);

    % Permute to align matrix dimensions for multiplication
    A_perm = permute(A, perm_order);
    B_perm = permute(B, perm_order);

    % Multiply slices
    C_perm = pagemtimes(A_perm, B_perm);

    % Permute back to original order
    C = permute(C_perm, inv_perm);

    % Ensure output shape matches
    assert(isequal(size(C), original_size), 'Output size mismatch.');
end