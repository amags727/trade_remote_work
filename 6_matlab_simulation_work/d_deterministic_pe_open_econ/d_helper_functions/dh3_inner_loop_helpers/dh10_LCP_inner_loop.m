function output = dh10_LCP_inner_loop(v, params, graph_analysis)

% unpack params
fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end

% Set PE variables 
tau_vec = [1,tau];
gamma = params.gamma;
p = (gamma_tilde* w * tau_vec ./phi_g);
x_bar = y.*p.^(-gamma).*P.^(gamma-1);
pi_bar = x_bar.* p./gamma;
E_x = x_bar.*A_tilde.* permute(networks, [3 2 1]);
E_pi = pi_bar.*A_tilde.* permute(networks, [3 2 1]); % Expected working profits (not accounting for fixed costs; data labor)
xi = Sigma_pen*alpha_1*phi_d.*E_x.^alpha_2;
pe_vars = {'E_x', 'E_pi', 'xi'};
for i = 1:length(pe_vars); name = pe_vars{i}; params.(name) = eval(name); end


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
    [V,z,best_alt] = actual_lcp_wrapper(v, optim.profit_w_actions, A_matrix, params, int_indices);
    
    % actually update the value function and set new int indices 
    if n ==1; dist =0; end; int_indices_old = int_indices;
    [v,int_indices, dist] = dh8_update_V(v,V,params, dist, n);

    if n > 25; crit = 1e-3; end
    if dist(n) < crit
        if length(int_indices_old) == len_Sigma
            %fprintf('LCP Converged, Iteration = %g\n', n);
            converged =  true; break
        else
            int_indices = 1:len_Sigma;
        end
    elseif n == maxit; fprintf('LCP failed to converge \n'); converged = false;
    end
end
entrance_v = v(len_Sigma, 1);
output = struct();
output_names = {'v', 'optim','entrance_v', 'p','P'};
output = struct();for i = 1:length(output_names); name = output_names{i}; output.(name) = eval(name); end

if graph_analysis
    preferred_network = repmat(1:num_networks, len_Sigma,1).*(reshape(z,[],num_networks)>0) +best_alt.*(reshape(z,[],num_networks) ==0);
    output = dh11_inner_loop_plotting(optim, params,preferred_network,v, output);
end
end

%% HELPER FUNCTIONS
% gen a restricted version of the state space
function upwind_params= gen_upwind_params(params, int_indices)
upwind_params = params;
upwind_params.len_Sigma = length(int_indices);
upwind_params.Sigma = params.Sigma(int_indices, :);
upwind_params.Sigma_mat = params.Sigma_mat(:,:,int_indices,:);
upwind_params.E_x = params.E_x(int_indices, :,:);
upwind_params.E_pi = params.E_pi(int_indices, :,:);
upwind_params.xi = params.xi(int_indices, :,:);
upwind_params.Sigma_pen = params.Sigma_pen(int_indices, :);
end

function [V,z,best_alt] = actual_lcp_wrapper(v, profit, A_matrix, params, int_indices)
% unpack params
fields = fieldnames(params);
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end
int_indices = [int_indices, int_indices + len_Sigma]; 

B = (rho + 1/Delta)*speye(len_Sigma.*num_networks) - A_matrix;
pi_stacked = reshape(profit, [],1);
v_stacked = reshape(v, [], 1);
vstar = zeros(size(v)); best_alt = vstar;
for network = 1:num_networks
    temp = v; temp(:,network) = -inf;
    temp = temp - sum(max(0, networks - networks(network,:)).*ec,2)'...
        -  sum(max(0, networks(network,:)- networks).*rev_ec,2)';
    [vstar(:,network), best_alt(:,network)] = max(temp, [],2);
end
vstar_stacked = reshape(vstar, [], 1);
vec = pi_stacked + v_stacked/Delta;
q = -vec + B*vstar_stacked;
z0 = v_stacked - vstar_stacked;
l = zeros(size(v_stacked));
u = Inf*ones(size(v_stacked));
z = pathlcp(B,q,l,u,z0);

% update the value function
V= reshape(z+ vstar_stacked, [], num_networks);
z = reshape(z, [], num_networks);

end
