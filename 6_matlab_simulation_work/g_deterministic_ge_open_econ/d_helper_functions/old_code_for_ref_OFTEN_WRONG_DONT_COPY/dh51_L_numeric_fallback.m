function [L_opt, fval, exitflag] = dh51_L_numeric_fallback(L_guess, phi_d, alpha_1,alpha_2, ...
    E_x, networks, sigma_a, Sigma_mat, D, Q, dv, E_pi, w, fc, ...
    problem_rows, network, num_mkts, num_prows)

    % Optimization target: L >= 0, maximize Hamiltonian

    % Setup
    num_vars = length(L_guess);
    lb = zeros(size(L_guess));   % L_i >= 0
    ub = [];                     % no upper bounds

    % Objective: negative Hamiltonian (since fmincon minimizes)
    objective = @(L) compute_hamiltonian(L, phi_d, alpha_1,alpha_2, ...
        E_x, networks, sigma_a, Sigma_mat, D, Q, dv, ...
        E_pi, w, fc, problem_rows, network, num_mkts, num_prows);

    % Optimization options
    options = optimoptions('fmincon', 'Display', 'iter', 'Algorithm', 'sqp');

    % Run optimization
    [L_opt, fval, exitflag] = fmincon(objective, L_guess, ...
        [], [], [], [], lb, ub, [], options);
end

function ham_scalar = compute_hamiltonian(L, phi_d, alpha_1, alpha_2,...
    E_x, networks, sigma_a, Sigma_mat, D, Q, dv, ...
    E_pi, w, fc, problem_rows, network, num_mkts, num_prows)

    % ==== Construct R matrix ====
    R_vec = phi_d * L.^alpha_1 .* E_x(problem_rows,:,network).^alpha_2 + ...
            networks(network,:) .* (sigma_a.^-2);

    R_mat = zeros(num_mkts, num_mkts, size(R_vec, 1));
    for i = 1:num_mkts
        R_mat(i, i, :) = squeeze(R_vec(:, i));
    end

    % ==== Construct drift ====
    p_Sigma = Sigma_mat(:,:,problem_rows,network);
    p_D = repmat(D, 1, 1, num_prows);
    p_Q = repmat(Q, 1, 1, num_prows);

    drift = pagemtimes(p_D, p_Sigma) + pagemtimes(p_Sigma, p_D) + p_Q ...
          - pagemtimes(pagemtimes(p_Sigma, R_mat), p_Sigma);

    drift_vec = dh2_restructure_array(drift, false, num_mkts);

    % ==== Compute profit ====
    profit = sum(E_pi(num_prows,:,network) - w*L - fc .* networks(network,:), 2);

    % ==== Compute Hamiltonian ====
    dv_mat = dh2_restructure_array(dv(problem_rows, :,network),true,num_mkts);
    ham = profit + squeeze(sum(drift .* dv_mat, [1 2]));

    % Return negative scalar for minimization
    ham_scalar = -sum(ham);  % sum over rows to get scalar
end