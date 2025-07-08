function A_matrix = e1_construct_A_matrix(optim, d_state_space,I, num_state_vars, len_state)
row_idx = [];
col_idx = [];
vals = [];

Y_total = zeros(len_state, 1);
base_idx = (1:len_state)';
strides = cumprod([1, repmat(I, 1, num_state_vars - 1)]);

for state_num = 1:num_state_vars
    stride = strides(state_num);
    block_size = I * stride;

    fwd_idx = base_idx + stride;
    bwd_idx = base_idx - stride;

    valid_fwd = mod(base_idx - 1, block_size) < block_size - stride;
    valid_bwd = mod(base_idx - 1, block_size) >= stride;


    % Compute components
    drift = optim.drift.overall(:,state_num)/ d_state_space(state_num);
    diffusion_sq = optim.diffusion.overall_sq(:,state_num) / (d_state_space(state_num)^2);
    X = -min(drift, 0) + .5*diffusion_sq;
    Y = -max(drift, 0) + min(drift, 0) - diffusion_sq;
    Z =  max(drift, 0) + .5*diffusion_sq;

    % Add reflected components to the diagonal where fwd/bwd is invalid
    Y = Y + (~valid_bwd) .* X + (~valid_fwd) .* Z;

    % Accumulate diagonal
    Y_total = Y_total + Y;

    % Off-diagonal entries
    row_idx = [row_idx; base_idx(valid_bwd)];
    col_idx = [col_idx; bwd_idx(valid_bwd)];
    vals    = [vals; X(valid_bwd)];

    row_idx = [row_idx; base_idx(valid_fwd)];
    col_idx = [col_idx; fwd_idx(valid_fwd)];
    vals    = [vals; Z(valid_fwd)];
end

% Add accumulated diagonal entries
row_idx = [row_idx; base_idx];
col_idx = [col_idx; base_idx];
vals    = [vals; Y_total];

% Assemble sparse matrix
A_matrix = sparse(row_idx, col_idx, vals, len_state, len_state);
if max(abs(sum(A_matrix,2)))>10^(-12)
    disp('Improper Transition Matrix')
end
end