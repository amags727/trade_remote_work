function dv_final = e1_resolve_upwind_directions(dv_b, dv_f, dv_2, ...
    z_act, z_hat, Sigma, xi, E_x, E_pi, ...
    w, alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, ...
    len_state, num_state_vars)

% Carry out the updwind 
% === SETUP ===
Ib = false(len_state,num_state_vars); If = Ib; I_final = Ib;
dv_min = min(dv_b, dv_f); dv_max = max(dv_b, dv_f); dv_final = zeros(size(dv_b));
z_vec = zeros(len_state, 1);
z_greater_z_hat_mat = repmat(z_act > z_hat, 1, num_state_vars);

% === 1. Deterministic drift directions ===

% (1) z: drift = -theta * z → backward if z > 0
Ib(z_act > 0,1) = true; If(z_act <= 0 , 1) = true;

% (3) a: drift ∝ sign of v_a → use direction only if sign is consistent
Ib(dv_max(:,3) < 0, 3) = true; If(dv_min(:,3) > 0, 3) = true; 

% mark finalized entries and set finalized values 
I_final(:,[1,3]) = true;
dv_final(Ib) = dv_b(Ib); dv_final(If) = dv_f(If);

% resolve ẑ and Σ by iteratively checking best / work case scenarios 
for place_holder = 1:2
    % === 2. Resolve ẑ (column 2) via worst-case derivative combinations ===
    % For z_hat: b(z_hat) ⬆️ in v_zhat and ⬇️ in v_Sigma

    % Worst-case for drift ≥ 0
    dv_bzhat_min = I_final.*dv_final...
        + (~I_final).*[z_vec, dv_min(:,2), z_vec, dv_max(:,4)];
    temp = e1_optim_calc(dv_bzhat_min, dv_2, w, z_act, z_hat, Sigma, xi, alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, E_x, E_pi);
    If(temp.drift.z_hat >= 0,2) = true;

    % Worst-case for drift < 0
    dv_bzhat_max = I_final.*dv_final...
        + (~I_final).*[z_vec, dv_max(:,2), z_vec, dv_min(:,4)];
    temp = e1_optim_calc(dv_bzhat_max , dv_2, w, z_act, z_hat, Sigma, xi, alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, E_x, E_pi);
    Ib(temp.drift.z_hat < 0,2) = true;

    I_final(:,2) = Ib(:,2) | If(:,2);
    dv_final(Ib) = dv_b(Ib); dv_final(If) = dv_f(If);

    if place_holder ~=2
        % === 3. Resolve Σ (column 4) via conditional worst-case logic ===
        % For Sigma:
        % If z > z_hat: b(Sigma) ⬇️ in v_zhat and ⬆️ in v_Sigma
        % If z < z_hat: b(Sigma) ⬆️ in v_zhat and ⬆️ in v_Sigma
        % Worst-case for drift ≥ 0
        dv_bSigma_min = I_final.*dv_final...
            + (~I_final & z_greater_z_hat_mat) .*[z_vec, dv_max(:,2), z_vec, dv_min(:,4)]...
            + (~I_final & ~z_greater_z_hat_mat) .*[z_vec, dv_min(:,2), z_vec, dv_min(:,4)];
        temp = e1_optim_calc(dv_bSigma_min, dv_2, w, z_act, z_hat, Sigma, xi, alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, E_x, E_pi);
        If(temp.drift.Sigma >= 0,4) = true;

        % Worst-case for drift < 0
        dv_bSigma_max = I_final.*dv_final...
            + (~I_final & z_greater_z_hat_mat) .*[z_vec, dv_min(:,2), z_vec, dv_max(:,4)]...
            + (~I_final & ~z_greater_z_hat_mat) .*[z_vec, dv_max(:,2), z_vec, dv_max(:,4)];
        temp = e1_optim_calc(dv_bSigma_max, dv_2, w, z_act, z_hat, Sigma, xi, alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, E_x, E_pi);
        Ib(temp.drift.Sigma < 0,4) = true;
        I_final(:,4) = Ib(:,4) | If(:,4);
        dv_final(Ib) = dv_b(Ib); dv_final(If) = dv_f(If);
    end
end

% resolve rows that still have ambiguity
rows_with_ambiguity = find(any(~I_final, 2));
for idx = 1:length(rows_with_ambiguity)
     i = rows_with_ambiguity(idx);
    known_columns = find(I_final(i,:)==1);
    unknown_columns = find(I_final(i,:)==0);
    num_unknown = numel(unknown_columns);
    num_combos = 2^num_unknown;
    combos = dec2bin(0:num_combos-1) - '0';   % size: (2^k) × k
    combo_matrix = repmat(dv_b(i,:), num_combos, 1);  % default to dv_b
    
    for j = 1:num_unknown
        col = unknown_columns(j);
        combo_matrix(:,col) = (1 - combos(:,j)) .* dv_b(i,col) + combos(:,j) .* dv_f(i,col);
    end

    optim = e1_optim_calc(combo_matrix, ...
        repmat(dv_2(i,:), num_combos, 1),w, ...
        repmat(z_act(i),num_combos,1), ...
        repmat(z_hat(i), num_combos,1), ...
        repmat(Sigma(i), num_combos,1), ...
        repmat(xi(i),num_combos, 1), ...
        alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, ...
        repmat(E_x(i), num_combos, 1), ...
        repmat(E_pi(i), num_combos, 1));
    [~, best_combo] = max(optim.ham, [], 'omitnan');
    dv_final(i,:) = combo_matrix(best_combo,:);
end
end
