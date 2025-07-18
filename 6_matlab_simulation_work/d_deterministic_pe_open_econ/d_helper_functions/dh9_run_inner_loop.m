function [v, optim, z, best_alt,converged] = dh9_run_inner_loop(v, LCP_yn, params)
% Inputs:
%   v       - initial value function
%   LCP_yn  - flag for LCP version
%   params  - struct containing all required parameters
if ~LCP_yn
v_out = zeros(params.len_Sigma, params.num_networks);
converged =false(params.num_networks, 1);
optim_list = cell(params.num_networks,1);
parfor i = 1:params.num_networks
    % update params to focus on network of choice
    local_params = params;
    local_params.num_networks = 1;
    local_params.Sigma_mat = params.Sigma_mat(:,:,:,1);
    local_params.networks = params.networks(i,:);
    local_params.E_x = params.E_x(:,:,i);
    local_params.E_pi = params.E_pi(:,:,i);
    local_params.xi = params.xi(:,:,i);
    [v_out(:,i), optim_list{i}, ~,~,converged(i)] = inner_loop(v(:,i), false, local_params);
end

v = v_out; converged = all(converged);
optim.L = zeros(params.len_Sigma,params.num_mkts, params.num_networks);
optim.profit_w_actions = zeros(params.len_Sigma, params.num_networks);
optim.ham = zeros(params.len_Sigma, params.num_networks);

for i = 1:params.num_networks
    optim.L(:,:,i) = optim_list{i}.L;
    optim.drift(:,:,i) = optim_list{i}.drift;
    optim.profit_w_actions(:,i) = optim_list{i}.profit_w_actions;
    optim.ham(:,i) = optim_list{i}.ham;
end
else 
   [v, optim, z, best_alt,converged] = inner_loop(v,LCP_yn, params);
end
end

function[v, optim, z, best_alt,converged] = inner_loop(v,LCP_yn, params)


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
        A_matrix = dh7_make_A_matrix(optim.drift, params.d_Sigma);

        % Step 5: Value update
        [V, z, best_alt] = dh8_update_V(LCP_yn, optim.profit_w_actions, ...
            v, A_matrix, A_matrix_cells, ...
            params.rho, params.Delta, params.networks, params.ec, params.rev_ec);

        Vchange = V - v; 
        % Step 6: Relaxation and convergence check
        if ~LCP_yn; baseline = median(abs(Vchange)); else; baseline = median(abs(Vchange(:)));end

        problem_indices = abs(Vchange) > 1e2 *baseline & abs(Vchange) < 1e4*baseline;
        garbage_indices = abs(Vchange) > 1e4*baseline;
        normal_indices = ~problem_indices & ~garbage_indices;

      
        if n > 100
            last_updated = n-1; new_vec = (last_updated-98):last_updated; old_vec = new_vec-1;
        end
        if n> 100 && all(dist(new_vec)- dist(old_vec)<0)
             v(normal_indices) = 0.9 * v(normal_indices) + 0.1 * V(normal_indices);
             v(problem_indices)  = 0.99 * v(problem_indices) + 0.01 * V(problem_indices);
             v(garbage_indices)  = 0.995 * v(garbage_indices) + 0.005 * V(garbage_indices);
        else
            v(normal_indices) = 0.9 * v(normal_indices) + 0.1 * V(normal_indices);
            v(problem_indices)  = 0.995 * v(problem_indices) + 0.005 * V(problem_indices);
            v(garbage_indices)  = 0.999 * v(garbage_indices) + 0.001 * V(garbage_indices);
        end


       
        [max_val, max_index] = max(abs(Vchange(:)));
        dist(n) = max_val;
        if mod(n, 25) == 0
        fprintf('%g: max %g, 99th pctile %g, median %g\n',n,...
            Vchange(max_index), prctile(abs(Vchange(:)), 99), ...
            prctile(abs(Vchange(:)), 50));
        end
        Vchange = abs(Vchange);
        if dist(n) < params.crit
            fprintf('Value Function Converged, Iteration = %g\n', n);
            converged = true;
            break;
        elseif n == params.maxit
            converged = false;
        end
    end
end

%% === make an adjacency matrix %%
% idenifies each row identifies all indices that are adjacent (fwd, bwd) in any state to that
% index of the state space
function [adjacency_matrix] = make_adjacency_matrix(len_Sigma,num_state_vars,I) 
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

