function dv_final = dh6_upwind(dv_b, dv_f, params)

fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end

%=== setup ===  
dv_min_base = min(dv_b,dv_f); dv_max_base =  max(dv_b,dv_f);
%=== make initial fwd/bkwd assignments by iteratively checking best / worst case scenarios===  
Ib = false(size(dv_b)); If = Ib; I_final = Ib;
for i = 1:num_state_vars

    % dv_max is worst case scenario for negative Sigma_dot
    dv_max = dv_max_base; dv_max(Ib) = dv_b(Ib); dv_max(If) = dv_f(If);
    if num_networks == 1; int_indices = find(any(~(Ib | If),2)); else; int_indices = find(any(any(~(Ib | If), 2),3)); end
    upwind_params = gen_upwind_params(params, int_indices);
    optim_max = dh5_optim_calc(dv_max(int_indices, :,:), upwind_params);
    Ib(int_indices, :,:) = optim_max.drift <= 0;

    %dv_min is worse case scenario for postive Sigma_dot
    dv_min = dv_min_base; dv_min(Ib) = dv_b(Ib); dv_min(If) = dv_f(If);
    if num_networks == 1; int_indices = find(any(~(Ib | If),2)); else; int_indices = find(any(any(~(Ib | If), 2),3)); end
    upwind_params = gen_upwind_params(params, int_indices);
    optim_min = dh5_optim_calc(dv_min(int_indices, :,:), upwind_params);
    If(int_indices, :,:) = optim_min.drift > 0;

    if  isequal(I_final, Ib | If) || all(Ib(:) | If(:))
        I_final = (Ib | If);
        break
    else
        I_final = (Ib | If);
    end
end
dv_final = zeros(size(dv_b)); dv_final(Ib) = dv_b(Ib); dv_final(If) = dv_f(If);

for network = 1:num_networks
    I_final_network = I_final(:,:,network);
    rows_with_ambiguity = any(~I_final_network, 2);
    unique_keys = unique(I_final_network(rows_with_ambiguity, :), 'rows');

    for key_index = 1:size(unique_keys,1)
        key = unique_keys(key_index,:); unknown_cols = find(~key);
        k = numel(unknown_cols); num_combos = 2^k;
        key_rows = find(all(I_final_network == key,2));

        % Build combo derivatives
        combos = repelem(logical(dec2bin(0:num_combos-1) - '0'),length(key_rows),1);
        combo_mat = zeros(size(combos,1), num_state_vars);
        combo_mat(:,unknown_cols) = combos;
        dv_b_key = dv_b(key_rows, :,network); dv_b_key(:,~unknown_cols) = dv_final(key_rows, ~unknown_cols, network);
        dv_f_key = dv_f(key_rows, :,network); dv_f_key(:,~unknown_cols) = dv_final(key_rows, ~unknown_cols, network);
        dv_combos = repmat(dv_b_key,num_combos,1) + repmat(dv_f_key - dv_b_key, num_combos, 1).*combo_mat;


        l_params = params;
        l_params.networks = networks(network,:);
        l_params.num_networks = 1;
        l_params.Sigma_mat = repmat(params.Sigma_mat(:, :, key_rows,network), 1, 1, num_combos);
        l_params.E_x = repmat(params.E_x(key_rows, :,network), num_combos,1);
        l_params.E_pi = repmat(params.E_pi(key_rows, :, network), num_combos,1);
        l_params.xi = repmat(params.xi(key_rows, :,network), num_combos,1);


        optim = dh5_optim_calc(dv_combos, l_params);
        g = repmat((1:length(key_rows))', num_combos,1);
        [~, max_idx] = splitapply(@(v) max(v), optim.ham, g);
        row_indices = (max_idx-1)*length(key_rows) + (1:length(key_rows))';
        dv_final(key_rows,:,network) = dv_combos(row_indices, :);
    end
end

end

function upwind_params= gen_upwind_params(params, int_indices)
upwind_params = params;
upwind_params.len_Sigma = length(int_indices);
upwind_params.Sigma = params.Sigma(int_indices, :);
upwind_params.Sigma_mat = params.Sigma_mat(:,:,int_indices,:);
upwind_params.E_x = params.E_x(int_indices, :,:);
upwind_params.E_pi = params.E_pi(int_indices, :,:);
upwind_params.xi = params.xi(int_indices, :,:);
end
