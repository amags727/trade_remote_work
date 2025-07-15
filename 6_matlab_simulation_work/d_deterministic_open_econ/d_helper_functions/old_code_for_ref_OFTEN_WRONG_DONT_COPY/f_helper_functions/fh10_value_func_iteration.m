function [v, optim,z, best_alt,dv_final] = fh10_value_func_iteration(...
    v0,smoother,LCP_yn, maxit, crit, ...
    I, num_state_vars, num_networks, d_state_space, ...
    state_space, Sigma_mat, Q, D, ...
    w, phi_d, alpha_1, alpha_2, sigma_a, ...
    networks, E_x, E_pi, xi, fc, ...
    rho, Delta, ec, rev_ec)

v= v0;
for n = 1:maxit
    % Step 1: Derivatives
    [dv_f, dv_b] = fh5_make_derivatives(v, I, num_state_vars, num_networks, d_state_space);

    % Step 2: Upwind scheme
    dv_final = fh6_upwind(dv_b, dv_f, state_space, Sigma_mat, Q, D, ...
        w, phi_d, alpha_1, alpha_2, sigma_a, ...
        networks, E_x, E_pi, xi, fc);

    % Step 3: Optimal policy
    optim = fh7_optim_calc(dv_final, state_space, Sigma_mat, Q, D, ...
        w, phi_d, alpha_1, alpha_2, sigma_a, ...
        networks, E_x, E_pi, xi, fc);

    % Step 4: Transition matrix
    [A_matrix, A_matrix_cells] = fh8_make_A_matrix(optim.drift, d_state_space);

    % Step 5: Bellman update
    [V, z, best_alt] = fh9_update_V(LCP_yn, optim.profit_w_actions, v, ...
        A_matrix, A_matrix_cells, rho, Delta, networks, ec, rev_ec);

    % Step 6: convergence check
    Vchange = abs(V - v);
    max_change = max(Vchange(:));
    fprintf('max div: %g, avg div: %g\n', ...
    max_change, max(mean(Vchange)));

    dist = max(mean(Vchange));
    if dist < crit
        fprintf('Value Function Converged, Iteration = %g\n', n);
        break
    end

    %Step 7: update if necessary 
    v = smoother * v + (1-smoother) * V;
end
end
