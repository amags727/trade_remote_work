function dual_output = dh2_find_asymmetric_ss(y, P0, grid_length, num_breaks,params)
params.y = y; 
new_cache = {};
[new_cache{1}, best_value, ~] = dual_inner_loop(params, P0,{});
cutoff = 1e-6; best_P = P0;
while best_value > cutoff
    [new_cache, best_P, best_value,grid_length] = grid_iteration(params, new_cache, num_breaks, grid_length, best_P);
end
distances = cellfun(@(entry) norm(best_P - entry.P), new_cache);
[~, idx] = min(distances);
dual_v = new_cache{idx}.dual_v;
[~,~,dual_output] = dual_inner_loop(params, best_P, dual_v, true);
end

%% WRAPPER FOR CHECKING ALL P COMBINAIONS IN THE GRID 
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
    [new_cache{i}, values(i),~] = dual_inner_loop(params,P, dual_v);
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
fprintf('P = (%g, %g); value = %g\n', best_P(1), best_P(2), best_value)
end

%% WRAPPER FOR FINDING THE MISS VALUE OF AN INDIVIDUAL P POINT 
function [cache_entry,value,dual_output] = dual_inner_loop(params, P, in_dual_v)
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
    dual_miss_val{i} = t_output.entrance_v - params.ec(1);
end

% record how far the larger entrance value is from 0;
v_miss = abs(max(dual_miss_val{:})); entered = [dual_miss_val{:}] > 0;

% prepare to do numeric optimization to try and find optimal number of firms 
p = [dual_output{1}.p; flip(dual_output{2}.p)]; 
A_1 = entered(1)* dual_output{1}.A_tilde_out.*params.networks(dual_output{1}.network_ss,:);
A_2 = entered(2)*dual_output{2}.A_tilde_out.*params.networks(dual_output{2}.network_ss,:);
A = [A_1 ; flip(A_2)];

[num_firms, P_resid] = solve_num_firms_numeric(P, A,p, params.gamma, params.sym_num_firms);
value = sum([P_resid; v_miss]);
cache_entry =  struct('P', {P}, 'dual_v', {out_dual_v});
end


function [num_firms, P_resid] = solve_num_firms_numeric(P, A, p, gamma, sym_num_firms)
    % Bounds 
    lb = zeros(2,1); ub = 4*sym_num_firms*ones(2,1);
    
    % Prep
    e = 1 - gamma;  rhs = (P(:)).^e;   B = A .* (p.^e);  
    num_firms_0 = sym_num_firms * ones(2,1);

    % Options
    opts = optimoptions('fmincon',  'Algorithm','interior-point', ...
        'Display','off', 'StepTolerance',1e-10,'OptimalityTolerance',1e-8);

    % Solve: minimize L1 residual
    obj = @(x) sum(abs(B*x - rhs));
    num_firms = fmincon(obj, num_firms_0, [],[],[],[], lb, ub, [], opts);
    P_resid = sum(abs(B*num_firms - rhs));
end
