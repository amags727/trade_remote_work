function [A_block,A_cell] = fh8_make_A_matrix(drift, d_state_space)

%setup 
[len_Sigma,num_state_vars, num_networks] = size(drift );
I = round(len_Sigma.^(1/num_state_vars));
A_cell = cell(num_networks, 1);
strides = cumprod([1, repmat(I, 1, num_state_vars - 1)]);

parfor network = 1:num_networks
    row_idx = [];
    col_idx = [];
    vals = [];

    Y_total = zeros(len_Sigma, 1);
    base_idx = (1:len_Sigma)';

    for state_num = 1:num_state_vars
        stride = strides(state_num);
        block_size = I * stride;

        fwd_idx = base_idx + stride;
        bwd_idx = base_idx - stride;

        valid_fwd = mod(base_idx - 1, block_size) < block_size - stride;
        valid_bwd = mod(base_idx - 1, block_size) >= stride;

        % Compute components
        X = -min(drift(:,state_num,network), 0) / d_state_space(state_num);
        Z =  max(drift(:,state_num,network), 0) / d_state_space(state_num);
        Y = -max(drift(:,state_num,network), 0) / d_state_space(state_num) + ...
            min(drift(:,state_num,network), 0) / d_state_space(state_num);

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
    A_cell{network} = sparse(row_idx, col_idx, vals, len_Sigma, len_Sigma);
    temp = A_cell{network};
    if max(abs(sum(temp,2)))>10^(-12)
        disp('Improper Transition Matrix')
        disp(network)
    end
end

% === Build sparse block-diagonal matrix manually ===
total_size = len_Sigma * num_networks;
all_row = [];
all_col = [];
all_val = [];

for n = 1:num_networks
    [i, j, v] = find(A_cell{n});
    offset = (n - 1) * len_Sigma;
    all_row = [all_row; i + offset];
    all_col = [all_col; j + offset];
    all_val = [all_val; v];
end

A_block = sparse(all_row, all_col, all_val, total_size, total_size);
end 
