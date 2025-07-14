function [dv_f, dv_b] = fh5_make_derivatives(v, I, num_state_vars, num_networks, d_state_space)
len_Sigma = size(v,1);

v_reshaped = reshape(v, [repmat(I, 1, num_state_vars), num_networks]);
    dv_f = zeros(len_Sigma, num_state_vars, num_networks); dv_b = dv_f;
    parfor state_num = 1:num_state_vars
        % Create indexing templates
        idx_all = repmat({':'}, 1, num_state_vars + 1);

        % Forward difference (except last index)
        idx_f = idx_all;        idx_f{state_num} = 1:I-1;
        idx_next = idx_all;     idx_next{state_num} = 2:I;
        temp = zeros(size(v_reshaped));
        temp(idx_f{:}) = v_reshaped(idx_next{:}) - v_reshaped(idx_f{:});

        % At boundary: use backward difference
        idx_last = idx_all;     idx_last{state_num} = I;
        idx_prev = idx_all;     idx_prev{state_num} = I-1;
        temp(idx_last{:}) = v_reshaped(idx_last{:}) - v_reshaped(idx_prev{:});

        % Store reshaped result and divide by d_state_space
        dv_f(:, state_num, :) = reshape(temp, len_Sigma, 1, num_networks)/d_state_space(state_num);

        % Backward difference (except first index)
        idx_b = idx_all;        idx_b{state_num} = 2:I;
        idx_prev = idx_all;     idx_prev{state_num} = 1:I-1;
        temp2 = zeros(size(v_reshaped));
        temp2(idx_b{:}) = v_reshaped(idx_b{:}) - v_reshaped(idx_prev{:});

        % At boundary: use forward difference
        idx_first = idx_all;    idx_first{state_num} = 1;
        idx_next = idx_all;     idx_next{state_num} = 2;
        temp2(idx_first{:}) = v_reshaped(idx_next{:}) - v_reshaped(idx_first{:});

        % Store reshaped result
        dv_b(:, state_num, :) = reshape(temp2, len_Sigma, 1, num_networks) / d_state_space(state_num);
    end
    clear v_reshaped state_num
