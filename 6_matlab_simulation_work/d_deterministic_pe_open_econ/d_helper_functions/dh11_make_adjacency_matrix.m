function [adjacency_matrix] = dh11_make_adjacency_matrix(len_Sigma,num_state_vars,I) 
index_array = reshape(1:len_Sigma, repmat(I, 1, num_state_vars));
index_array_b = zeros(len_Sigma, num_state_vars);
index_array_f= zeros(len_Sigma, num_state_vars);

    for state_num = 1:num_state_vars
        % Create indexing templates
        idx_all = repmat({':'}, 1, num_state_vars);

        % Forward difference (except last index)
        idx_f = idx_all;        idx_f{state_num} = 1:I-1;
        idx_next = idx_all;     idx_next{state_num} = 2:I;
        temp = zeros(size(index_array));
        temp(idx_f{:}) = index_array(idx_next{:});
        index_array_f(:, state_num) = reshape(temp, len_Sigma, 1);

        % Backward difference (except first index)
        idx_b = idx_all;        idx_b{state_num} = 2:I;
        idx_prev = idx_all;     idx_prev{state_num} = 1:I-1;
        temp2 = zeros(size(index_array));
        temp2(idx_b{:}) = index_array(idx_prev{:});

        index_array_b(:,state_num) = reshape(temp2, len_Sigma, 1); 
    end
    adjacency_matrix = [index_array_b, index_array_f];
end

