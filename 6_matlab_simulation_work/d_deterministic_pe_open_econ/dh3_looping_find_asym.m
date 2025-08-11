function [best_P, dual_output,num_firms] = dh3_looping_find_asym(y, P0, params,cutoff)

params.y = y; 
new_cache = {}; P_in = P0;
[new_cache{1},best_value, P_out] = dual_inner_loop(params, P_in,{});

while best_value > cutoff 
   [new_cache,best_value, P_in, P_out] = grid_iteration(params, new_cache, P_in, P_out);
end

distances = cellfun(@(entry) norm(P_in - entry.P), new_cache);
[~, idx] = min(distances);
dual_v = new_cache{idx}.dual_v;
[~,~, best_P, dual_output,num_firms] = dual_inner_loop(params, P_in, dual_v);
end

%% WRAPPER FOR CHECKING ALL P COMBINAIONS IN THE GRID 
function [new_cache,best_value, best_P_in, best_P_out] = grid_iteration(params,old_cache,P_in, P_out)

% generate the new P matrices
relax =  [linspace(1e-4,.001,10),linspace(.001,.01,10), linspace(.01,1-1e-2,21)]'; 
P_in_matrix = (1-relax)*P_in + (relax)*P_out;
P_out_matrix = zeros(size(P_in_matrix));

new_cache = cell(length(P_in_matrix),1);
values = zeros(size(new_cache)); 
parfor i = 1:size(P_in_matrix,1)
    P_in = P_in_matrix(i,:);
    distances = cellfun(@(entry) norm(P_in - entry.P), old_cache);
    [~, idx] = min(distances);
    dual_v = old_cache{idx}.dual_v;
    [new_cache{i},values(i), P_out_matrix(i,:)] = dual_inner_loop(params, P_in,dual_v);
end

[best_value,min_index] = min(values);
best_P_in = P_in_matrix(min_index,:);
best_P_out = P_out_matrix(min_index,:);
fprintf('value = %g, best P_in = (%g, %g),best P_out = (%g, %g) \n', best_value, best_P_in(1), best_P_in(2), best_P_out(1), best_P_out(2));
end

%% WRAPPER FOR FINDING THE MISS VALUE OF AN INDIVIDUAL P POINT 
function [cache_entry,value, P_out, dual_output,num_firms] = dual_inner_loop(params, P, in_dual_v)
dual_output = cell(1,2); out_dual_v = dual_output; dual_miss_val = dual_output;
for i = 1:2
    l_params = params;
    l_params.P = P;
    if i==2; l_params.P = flip(P); l_params.y = flip(l_params.y); end
    if isempty(in_dual_v)
        v_hjb = dh9_HJB_inner_loop(zeros(params.len_Sigma, params.num_networks),l_params);
        t_output = dh10_LCP_inner_loop(v_hjb, l_params, true);
    else
        t_output = dh10_LCP_inner_loop(in_dual_v{i}, l_params, true);
    end
    dual_output{i} = t_output;
    out_dual_v{i} = t_output.v;
    dual_miss_val{i} = t_output.entrance_v - params.ec(1); %%% KEEP IN MIND THAT THIS WILL FAIL IF WE EVER MAKE EC DIFFERENT 
end

% Determine num firms in mkt 
eta = 100;
m_bar = .1;
z = min(eta*[dual_miss_val{:}], 40); 
num_firms = m_bar * exp(z);

% generate the output guess for the price index
gamma = params.gamma;
p = [dual_output{1}.p; flip(dual_output{2}.p)];
A = [dual_output{1}.A_tilde_out .* params.networks(dual_output{1}.network_ss,:);...
     dual_output{2}.A_tilde_out .* params.networks(dual_output{2}.network_ss,:)];
B = A .* (p.^(1-gamma)); 
P_out = (B*num_firms').^(1/(1-gamma))'; 
value = sum(abs(P_out - P));
P_out = min(P_out, 1.5);
cache_entry = struct('P', {P}, 'dual_v', {out_dual_v});
end

