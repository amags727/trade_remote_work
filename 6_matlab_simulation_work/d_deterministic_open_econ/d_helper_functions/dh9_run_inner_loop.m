function [v, optim, z, best_alt,converged] = dh9_run_inner_loop(v, LCP_yn, params)
% Inputs:
%   v       - initial value function
%   LCP_yn  - flag for LCP version
%   params  - struct containing all required parameters

    for n = 1:params.maxit
        % Step 1: Derivatives
        [dv_f, dv_b] = dh4_make_derivatives(v, ...
            params.I, params.num_state_vars, params.num_networks, params.d_Sigma);

        % Step 2: Upwind
        dv_final = dh6_upwind(dv_b, dv_f, ...
            params.Sigma_mat, params.Q, params.D, params.w, ...
            params.phi_d, params.alpha_1, params.alpha_2, params.sigma_a, ...
            params.networks, params.E_x, params.E_pi, params.xi, params.fc);

        % Step 3: Optimal policy
        optim = dh5_optim_calc(dv_final, ...
            params.Sigma_mat, params.Q, params.D, params.w, ...
            params.phi_d, params.alpha_1, params.alpha_2, params.sigma_a, ...
            params.networks, params.E_x, params.E_pi, params.xi, params.fc);

        % Step 4: Transition matrix
        [A_matrix, A_matrix_cells] = dh7_make_A_matrix(optim.drift, params.d_Sigma);

        % Step 5: Value update
        [V, z, best_alt] = dh8_update_V(LCP_yn, optim.profit_w_actions, ...
            v, A_matrix, A_matrix_cells, ...
            params.rho, params.Delta, params.networks, params.ec, params.rev_ec);

        Vchange = V - v; baseline = mean(abs(Vchange(:)));
        % Step 6: Relaxation and convergence check
       
        problem_indices = abs(Vchange) > 1e2 *baseline & abs(Vchange) < 1e4*baseline;
        garbage_indices = abs(Vchange) > 1e4*baseline;
        normal_indices = ~problem_indices & ~garbage_indices;


        v(normal_indices) = 0.9 * v(normal_indices) + 0.1 * V(normal_indices);
        v(problem_indices)  = 0.995 * v(problem_indices) + 0.005 * V(problem_indices);
        v(garbage_indices)  = 0.999 * v(garbage_indices) + 0.001 * V(garbage_indices);


        [max_val, max_index] = max(abs(Vchange(:)));
        dist(n) = max_val;
        if mod(n, 25) == 0
        fprintf('%g: max divergence: %g; mean divergence: %g\n',n, ...
            Vchange(max_index), mean(Vchange(:)));
        end

        if dist(n) < params.crit
            fprintf('Value Function Converged, Iteration = %g\n', n);
            converged = true;
            break;
        elseif n == params.maxit
            converged = false;
        end
    end
end