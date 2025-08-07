clear all; close all; clc;
addpath(genpath('../b_helper_functions'));
addpath(genpath('d_helper_functions'))
addpath(genpath('d_output'))
dbstop if error

%% setup
params = dh0_set_invariant_params();
params.networks = [1,0;1,1];
params.y = [10,9.5];
P0 = [1.25450325, 1.27779503];
new_cache = {};

grid_length = .0005; num_breaks = 5;
[new_cache{1}, best_value, ~] = dual_inner_loop(params, P0,{}, false);
cutoff = 1e-6; best_P = P0;
while best_value > cutoff
    [new_cache, best_P, best_value,grid_length] = grid_iteration(params, new_cache, num_breaks, grid_length, best_P);
end
distances = cellfun(@(entry) norm(best_P - entry.P), new_cache);
[~, idx] = min(distances);
dual_v = new_cache{idx}.dual_v;
[~,~,dual_output] = dual_inner_loop(params, best_P, dual_v, true);



function [new_cache, best_P, best_value,grid_length] = grid_iteration(params,old_cache,num_breaks, grid_length, P0)
% setup the list of P to test 
lb = P0 - grid_length; ub = P0 + grid_length;
P1_vals = linspace(lb(1), ub(1), num_breaks); P2_vals = linspace(lb(2), ub(2), num_breaks);
[P1_grid, P2_grid] = ndgrid(P1_vals, P2_vals);
P_matrix = [P1_grid(:), P2_grid(:)];  

% iterate through P list 
new_cache = cell(size(P_matrix,1),1); values = zeros(size(new_cache));
parfor i = 1:size(P_matrix,1)
    P = P_matrix(i,:);
    distances = cellfun(@(entry) norm(P - entry.P), old_cache);
    [~, idx] = min(distances);
    dual_v = old_cache{idx}.dual_v;
    [new_cache{i}, values(i),~] = dual_inner_loop(params,P, dual_v,false);
end

% find the best value 
[best_value,min_index] = min(values);
best_P = P_matrix(min_index,:);

% update search box 
if all((best_P > lb) & (best_P < ub))
    grid_length = .5*grid_length;
else
    grid_length = 1.5*grid_length;
end
fprintf('P = (%g, %g); value = %g; grid_length = %g\n', best_P(1), best_P(2), best_value, grid_length)
end

function [cache_entry,value,dual_output] = dual_inner_loop(params, P, in_dual_v,graph_analysis)
dual_output = cell(1,2); out_dual_v = dual_output; dual_miss_val = dual_output;
for i = 1:2
    l_params = params;
    l_params.P = P;
    if i==2; l_params.P = flip(P); l_params.y = flip(l_params.y); end
    if isempty(in_dual_v)
        v_hjb = dh9_HJB_inner_loop(zeros(params.len_Sigma, params.num_networks),l_params);
        t_output = dh10_LCP_inner_loop(v_hjb, l_params, graph_analysis);
    else
        t_output = dh10_LCP_inner_loop(in_dual_v{i}, l_params, graph_analysis);
    end
    dual_output{i} = t_output;
    out_dual_v{i} = t_output.v;
    dual_miss_val{i} = abs(t_output.entrance_v - params.ec(1));
end
value = sum([dual_miss_val{:}]);
cache_entry =  struct('P', {P}, 'dual_v', {out_dual_v});
end


