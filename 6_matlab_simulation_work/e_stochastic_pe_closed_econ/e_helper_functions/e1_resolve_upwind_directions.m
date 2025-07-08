function dv_final = e1_resolve_upwind_directions(dv_b, dv_f, dv_2, ...
    z_act, z_hat, Sigma, xi, E_x, E_pi, ...
    w, alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, e1_optim_calc)

% Resolve upwind directions for HJB using analytical and exhaustive logic

[len_state, num_state_vars] = size(dv_b);
dv_final = zeros(size(dv_b));
dv_min = min(dv_b, dv_f); 
dv_max = max(dv_b, dv_f);
Ib = false(len_state, num_state_vars); 
If = Ib; 
I_final = Ib;
z_vec = zeros(len_state, 1);

% Precompute logical comparisons
z_greater_z_hat_mat = repmat(z_act > z_hat, 1, num_state_vars);

% === 1. Deterministic Drift ===
Ib(z_act > 0, 1) = true;
If(z_act <= 0, 1) = true;

Ib(dv_max(:,3) < 0, 3) = true;
If(dv_min(:,3) > 0, 3) = true;

I_final(:,[1,3]) = true;
dv_final(Ib) = dv_b(Ib);
dv_final(If) = dv_f(If);

% === 2. Iterative Worst-case for z_hat and Σ ===
for iter = 1:2
    % z_hat (column 2)
    dv_min_z_hat = I_final .* dv_final + (~I_final) .* [z_vec, dv_min(:,2), z_vec, dv_max(:,4)];
    temp = e1_optim_calc(dv_min_z_hat, dv_2, w, z_act, z_hat, Sigma, xi, ...
                         alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, E_x, E_pi);
    If(temp.drift.z_hat >= 0, 2) = true;

    dv_max_z_hat = I_final .* dv_final + (~I_final) .* [z_vec, dv_max(:,2), z_vec, dv_min(:,4)];
    temp = e1_optim_calc(dv_max_z_hat, dv_2, w, z_act, z_hat, Sigma, xi, ...
                         alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, E_x, E_pi);
    Ib(temp.drift.z_hat < 0, 2) = true;

    I_final(:,2) = Ib(:,2) | If(:,2);
    dv_final(Ib) = dv_b(Ib);
    dv_final(If) = dv_f(If);

    % Σ (column 4) only on first pass
    if iter == 1
        % drift ≥ 0 worst case
        dv_min_Sigma = I_final .* dv_final ...
                     + (~I_final & z_greater_z_hat_mat) .* [z_vec, dv_max(:,2), z_vec, dv_min(:,4)] ...
                     + (~I_final & ~z_greater_z_hat_mat) .* [z_vec, dv_min(:,2), z_vec, dv_min(:,4)];
        temp = e1_optim_calc(dv_min_Sigma, dv_2, w, z_act, z_hat, Sigma, xi, ...
                             alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, E_x, E_pi);
        If(temp.drift.Sigma >= 0, 4) = true;

        % drift < 0 worst case
        dv_max_Sigma = I_final .* dv_final ...
                     + (~I_final & z_greater_z_hat_mat) .* [z_vec, dv_min(:,2), z_vec, dv_max(:,4)] ...
                     + (~I_final & ~z_greater_z_hat_mat) .* [z_vec, dv_max(:,2), z_vec, dv_max(:,4)];
        temp = e1_optim_calc(dv_max_Sigma, dv_2, w, z_act, z_hat, Sigma, xi, ...
                             alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, E_x, E_pi);
        Ib(temp.drift.Sigma < 0, 4) = true;

        I_final(:,4) = Ib(:,4) | If(:,4);
        dv_final(Ib) = dv_b(Ib);
        dv_final(If) = dv_f(If);
    end
end

% === 3. Fallback Exhaustive Evaluation ===
rows_with_ambiguity = find(any(~I_final, 2));
parfor i = rows_with_ambiguity
    unknown_cols = find(I_final(i,:) == 0);
    num_unknown = numel(unknown_cols);
    num_combos = 2^num_unknown;

    % All 2^k combos for unknown columns
    combos = dec2bin(0:num_combos-1) - '0';  % (2^k) × k
    combo_matrix = repmat(dv_b(i,:), num_combos, 1);
    
    for j = 1:num_unknown
        col = unknown_cols(j);
        combo_matrix(:,col) = (1 - combos(:,j)) * dv_b(i,col) + combos(:,j) * dv_f(i,col);
    end

    optim = e1_optim_calc(combo_matrix, ...
        repmat(dv_2(i,:), num_combos, 1), w, ...
        repmat(z_act(i), num_combos, 1), ...
        repmat(z_hat(i), num_combos, 1), ...
        repmat(Sigma(i), num_combos, 1), ...
        repmat(xi(i), num_combos, 1), ...
        alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, ...
        repmat(E_x(i), num_combos, 1), ...
        repmat(E_pi(i), num_combos, 1));
    
    [~, best_combo] = max(optim.ham, [], 'omitnan');
    dv_final(i,:) = combo_matrix(best_combo,:);
end
end