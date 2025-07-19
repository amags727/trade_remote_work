function [v, optim, z, best_alt,converged] = dh9_LCP_inner_loop(v, params)

% unpack params
fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end

% set initial int_indices
int_indices = 1:len_Sigma;


for n = 1:maxit
    % make derivatives
    [dv_f, dv_b] = dh4_make_derivatives(v,params);

    % perform upwind
    upwind_params = gen_upwind_params(params, int_indices);
    dv_final = dh6_upwind(dv_b(int_indices,:,:), dv_f(int_indices,:,:),upwind_params);


    % find optimal choices
    if len_Sigma == length(int_indices)
        optim = dh5_optim_calc(dv_final,params);
        optim_old = optim;
    else
        optim_update = dh5_optim_calc(dv_final,upwind_params);
        optim = optim_old;
        optim.drift(int_indices,:,:) = optim_update.drift;
        optim.ham(int_indices,:) = optim_update.ham;
        optim.L(int_indices,:,:) = optim_update.L;
        optim.profit_w_actions(int_indices,:) = optim_update.profit_w_actions;
    end

    % find the transition matrix
    A_matrix = dh7_make_A_matrix(optim.drift, d_Sigma);

    % Use LCP to update value function guess
    [V,z,best_alt] = dh8_lcp_wrapper(v, optim.profit_w_actions, A_matrix, params);
    
    % actually update the value function and set new int indices 
    if n ==1; dist =0; end; int_indices_old = int_indices;
    [v,int_indices, dist] = dh8_update_V(v,V,params, dist, n);

    % 
    if dist(n) < crit
        if length(int_indices_old) == len_Sigma
            fprintf('LCP Converged, Iteration = %g\n', n); converged =  true; break
        else
            int_indices = 1:len_Sigma;
        end
    elseif n == maxit; fprintf('HJB failed to converge \n'); converged = false;
    end
end
end
% gen a restricted version of the state space
function upwind_params= gen_upwind_params(params, int_indices)
upwind_params = params;
upwind_params.len_Sigma = length(int_indices);
upwind_params.Sigma = params.Sigma(int_indices, :);
upwind_params.Sigma_mat = params.Sigma_mat(:,:,int_indices,:);
upwind_params.E_x = params.E_x(int_indices, :,:);
upwind_params.E_pi = params.E_pi(int_indices, :,:);
upwind_params.xi = params.xi(int_indices, :,:);
end

