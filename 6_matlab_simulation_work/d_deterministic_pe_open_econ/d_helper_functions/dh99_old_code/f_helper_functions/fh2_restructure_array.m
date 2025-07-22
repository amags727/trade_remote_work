function restructured = fh2_restructure_array(input, to_mat, num_mkts)
% transform the vector version of the input to the matrix version
if to_mat
    len_input = size(input, 1);

    % Build index matrix for symmetric structure
    mask = triu(true(num_mkts));
    index_matrix = zeros(num_mkts);
    index_matrix(mask) = 1:nnz(mask);
    index_matrix = index_matrix + index_matrix.' - diag(diag(index_matrix));

    % Determine whether we're dealing with one or multiple networks
    if ndims(input) == 2
        % Single network case
        restructured = reshape(input(:, index_matrix(:)), len_input, num_mkts, num_mkts);
        restructured = permute(restructured, [2 3 1]);  % (i,j,t)
    else
        % Multi-network case
        num_networks = size(input, 3);
        restructured = zeros(num_mkts, num_mkts, len_input, num_networks);
        for h = 1:num_networks
            temp = reshape(input(:, index_matrix(:), h), len_input, num_mkts, num_mkts);
            restructured(:,:,:,h) = permute(temp, [2 3 1]);  % (i,j,t,h)
        end
    end

    % transform the matrix version of the input to the vector
else
    rev_indeces= reshape(1:num_mkts^2,num_mkts, num_mkts);
    rev_indeces = rev_indeces(triu(true(size(rev_indeces)), 0));
    [i_idx, j_idx] = ind2sub([num_mkts, num_mkts], rev_indeces);
    rev_index_matrix = [i_idx, j_idx];
    num_state_vars = size(rev_index_matrix,1);
    if ndims(input)==2
        restructured = zeros(num_state_vars,1);
        for i = 1:num_state_vars
            restructured(i) = input(rev_index_matrix(i,1), rev_index_matrix(i,2));
        end
    elseif ndims(input) == 3
        len_Sigma = size(input, 3);
        restructured = zeros(len_Sigma, num_state_vars);
         for i = 1:num_state_vars
            restructured(:, i) = input(rev_index_matrix(i,1), rev_index_matrix(i,2), :);
         end
    else 
        len_Sigma = size(input, 3);
        num_networks = size(input,4);
        restructured = zeros(len_Sigma, num_state_vars, num_networks);
        for i = 1:num_state_vars
            restructured(:, i, :) = input(rev_index_matrix(i,1), rev_index_matrix(i,2), :, :);
        end
    end
end
end

