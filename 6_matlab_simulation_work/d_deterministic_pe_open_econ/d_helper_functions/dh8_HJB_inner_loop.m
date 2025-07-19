function [v, optim, converged] = dh8_HJB_inner_loop(v0, params)

% unpack params
fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end

% set outputs
v_out = zeros(len_Sigma, num_networks);
converged =false(num_networks, 1);
optim_list = cell(num_networks,1);

for i = 1:num_networks
    v = v0(:,i);
    % update params to focus on network of choice
    local_params = params;
    local_params.num_networks = 1;
    local_params.Sigma_mat = Sigma_mat(:,:,:,1);
    local_params.networks = networks(i,:);
    local_params.E_x = E_x(:,:,i);
    local_params.E_pi = E_pi(:,:,i);
    local_params.xi = xi(:,:,i);
    int_indices = 1:len_Sigma;

    %start loop
    for n = 1:maxit
        % make the derivatives
        [dv_f, dv_b] = dh4_make_derivatives(v,local_params);


        % carry out upwind
        upwind_params = gen_upwind_params(local_params, int_indices);
        dv_final = dh6_upwind(dv_b(int_indices,:), dv_f(int_indices,:),upwind_params);

        % find optimal choices
        if len_Sigma == length(int_indices)
            optim = dh5_optim_calc(dv_final,local_params);
            optim_old = optim;
        else
            optim_update = dh5_optim_calc(dv_final,upwind_params);
            optim = optim_old;
            optim.drift(int_indices,:) = optim_update.drift;
            optim.ham(int_indices) = optim_update.ham;
            optim.L(int_indices,:) = optim_update.L;
            optim.profit_w_actions(int_indices) = optim_update.profit_w_actions;
        end

        % find the transition matrix
        A_matrix = dh7_make_A_matrix(optim.drift, local_params.d_Sigma);

        % update guess for value function
        B = (rho + 1/Delta)*speye(len_Sigma) - A_matrix;
        b = optim.profit_w_actions + v./Delta;
        V = B\b;

        % update v; and generate indices to check next time
        if n ==1; dist =0; end; int_indices_old = int_indices;
        [v,int_indices, dist] = dh8_update_V(v,V,local_params, dist, n);
       

        if dist(n) < crit
            if length(int_indices_old) == len_Sigma
                fprintf('HJB Converged, Iteration = %g\n', n);
                converged(i) = true;  v_out(:,i) = v; optim_list{i} = optim;
                break
            else
                int_indices = 1:len_Sigma;
            end
        elseif n == maxit
            fprintf('HJB failed to converge \n')
            converged(i) = false; v_out(:,i) = v; optim_list{i} = optim;
        end
    end
end

% consolidate results
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
end

% gen a restricted version of the state space
function upwind_params= gen_upwind_params(local_params, int_indices)
upwind_params = local_params;
upwind_params.len_Sigma = length(int_indices);
upwind_params.Sigma = local_params.Sigma(int_indices, :);
upwind_params.Sigma_mat = local_params.Sigma_mat(:,:,int_indices);
upwind_params.E_x = local_params.E_x(int_indices, :);
upwind_params.E_pi = local_params.E_pi(int_indices, :);
upwind_params.xi = local_params.xi(int_indices, :);
end
















