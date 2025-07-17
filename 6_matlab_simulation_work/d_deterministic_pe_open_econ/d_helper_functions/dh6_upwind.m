function dv_final = dh6_upwind(dv_b, dv_f, Sigma_mat, Q, D, w, ...
    phi_d, alpha_1, alpha_2, sigma_a, networks, E_x, E_pi, xi, fc)

%=== setup ===  
num_state_vars = size(dv_b,2);
num_networks = size(networks,1);
dv_min_base = min(dv_b,dv_f); dv_max_base =  max(dv_b,dv_f);
%=== make initial fwd/bkwd assignments by iteratively checking best / worst case scenarios===  
Ib = false(size(dv_b)); If = Ib; I_final = Ib;
for i = 1:num_state_vars
    % dv_max is worst case scenario for negative Sigma_dot
    dv_max = dv_max_base; dv_max(Ib) = dv_b(Ib); dv_max(If) = dv_f(If);
    optim_max = dh5_optim_calc(dv_max, Sigma_mat, Q, D, w, phi_d, alpha_1, alpha_2, sigma_a, networks, E_x, E_pi, xi, fc);
    Ib = optim_max.drift <= 0;

    %dv_min is worse case scenario for postive Sigma_dot
    dv_min = dv_min_base; dv_min(Ib) = dv_b(Ib); dv_min(If) = dv_f(If);
    optim_min = dh5_optim_calc(dv_min, Sigma_mat, Q, D, w, phi_d, alpha_1, alpha_2, sigma_a, networks, E_x, E_pi, xi, fc);
    If = optim_min.drift > 0;

    if  isequal(I_final, Ib | If) || all(Ib(:) | If(:))
        I_final = (Ib | If);
        break
    else
        I_final = (Ib | If);
    end
end
dv_final = zeros(size(dv_b)); dv_final(Ib) = dv_b(Ib); dv_final(If) = dv_f(If);

%=== resolve rows that still have ambiguity===
for network = 1:num_networks
    I_final_network = squeeze(I_final(:, :, network));
    rows_with_ambiguity = find(any(~I_final_network, 2));

    % Preallocate outside parfor
    dv_final_rows = zeros(length(rows_with_ambiguity), num_state_vars);

    for idx = 1:length(rows_with_ambiguity)
        i = rows_with_ambiguity(idx);

        dv_b_row = dv_b(i, :, network);
        dv_f_row = dv_f(i, :, network);
        dv_partial = dv_final(i, :, network);  % slice is OK in parfor
        I_mask = I_final_network(i, :);

        known_cols = find(I_mask);
        unknown_cols = find(~I_mask);
        k = numel(unknown_cols);
        num_combos = 2^k;

        combos = logical(dec2bin(0:num_combos-1) - '0');  % (2^k × k)

        % Build combo derivatives
        dv_combos = repmat(dv_b_row, num_combos, 1);
        dv_f_minus_b = dv_f_row(unknown_cols) - dv_b_row(unknown_cols);
        dv_combos(:, unknown_cols) = dv_combos(:, unknown_cols) + combos * diag(dv_f_minus_b);

        % Plug in known values
        dv_combos(:, known_cols) = repmat(dv_partial(known_cols), num_combos, 1);

        % Pull inputs for this point
        Sigma_block = Sigma_mat(:, :, i, network);
        E_x_block = E_x(i, :, network);
        E_pi_block = E_pi(i, :, network);
        xi_block = xi(i, :, network);
        net_row = networks(network, :);

        % Expand inputs
        Sigma_rep = repmat(Sigma_block, 1, 1, num_combos, 1);
        E_x_rep = repmat(E_x_block, num_combos, 1);
        E_pi_rep = repmat(E_pi_block, num_combos, 1);
        xi_rep = repmat(xi_block, num_combos, 1);

        % Evaluate Hamiltonian
        optim = dh5_optim_calc(dv_combos, Sigma_rep, Q, D, w, ...
            phi_d, alpha_1, alpha_2, sigma_a, net_row, ...
            E_x_rep, E_pi_rep, xi_rep, fc);

        [~, best_idx] = max(optim.ham, [], 'omitnan');
        dv_final_rows(idx, :) = dv_combos(best_idx, :);
    end
    % Reassign in one go after loop
    dv_final(rows_with_ambiguity, :, network) = dv_final_rows;
end
end