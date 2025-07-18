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
    dv_final = dh6_upwind(dv_b, dv_f,local_params);

    % carry find optimal choices
    optim = dh5_optim_calc(dv_final,local_params);

    % find the transition matrix
    A_matrix = dh7_make_A_matrix(optim.drift, local_params.d_Sigma);
    
    % update guess for value function 
     B = (rho + 1/Delta)*speye(len_Sigma) - A_matrix;
     b = optim.profit_w_actions + v./Delta;
     V = B\b;

     % update v; and generate indices to check next time 
     if n ==1; dist =0; end 
     [Vchange, v] = dh8_update_V(v,V,dist, local_params);

     % report progress 
     if mod(n, 25) == 0
         fprintf('HJB: %g: max %g, 99th pctile %g, median %g\n',n,...
             max(Vchange), prctile(Vchange, 99), prctile(Vchange, 50));
     end

    % check for convergence 
    dist(n) = max(Vchange);
    if dist(n) < crit
        fprintf('HJB Converged, Iteration = %g\n', n);
        converged(i) = true;  v_out(:,i) = v; optim_list{i} = optim;
        break
     elseif n == maxit
         fprintf('HJB failed to converge \n')
         converged(i) = false;
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














   


