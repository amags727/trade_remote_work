function [graph_output] = find_SS(v, params, start_w_HJB)

% Solve for the Value function 
if start_w_HJB 
    [v] = dh9_run_inner_loop(v, false, params);
    [v, optim, z, best_alt,converged] = dh9_run_inner_loop(v, true, params);
else
    [v, optim, z, best_alt,converged] = dh9_run_inner_loop(v, true, params);
end

% Carry out setup 
[len_Sigma, num_state_vars, num_networks] = size(optim.drift);
num_mkts = size(params.Sigma_mat,1);
T = 1000;                 
Sigma_path = zeros(T, num_state_vars);
data_use_path = zeros(T, num_mkts); 
output_path = zeros(T, num_mkts);
profit_path = zeros(T, 1);
network_path = ones(T,1);
% determine if firm will change networks 
preferred_network = repmat(1:num_networks, len_Sigma,1).*(reshape(z,[],num_networks)>0) +...
                    best_alt.*(reshape(z,[],num_networks) ==0);

% establish starting state 
Sigma_path(1, :)= params.Sigma(len_Sigma,:);
indices_used = [];
for t = 1:T
    Sigma_t = Sigma_path(t,:);
    network_t = network_path(t);
    [indices, weights] = fh11_interp_box(Sigma_t, params.Sigma, 2);
    indices_used = [indices_used; setdiff(indices,1)];
    data_use_path(t,:) =  sum(optim.L(indices, :, network_t) .* weights); 
    output_path(t,:) =  sum(params.E_x(indices, :, network_t) .* weights); 
    profit_path(t,:) = sum(optim.profit_w_actions(indices, network_t).*weights);
    drift_t = sum(optim.drift(indices,:,network_t).*weights);
    if (t ~= T)
        Sigma_path(t+1,:) = Sigma_t + drift_t*1/params.Delta;
        [indices, weights] = fh11_interp_box( Sigma_path(t+1,:),params.Sigma, 2);
        best_score = -inf; best_network = 1; pref_base = [preferred_network(indices,network_t),weights];
        for network = 1:num_networks
          score = sum(pref_base(pref_base(:,1) == network, 2));
          if score > best_score 
              best_score = score; best_network = network;
          end
        end
        network_path(t+1) = best_network;
    end 
end
indices_used = unique(indices_used);
output_fields = [strcat({'Sigma', 'data_use', 'output', 'profit', 'network'}, '_path'), 'v', 'converged', 'indices_used'];
graph_output = struct(); for i = 1:length(output_fields); name = output_fields{i}; graph_output.(name) = eval(name); end

end