clear all; close all; clc;
addpath(genpath('../b_helper_functions'));
addpath(genpath('d_helper_functions'))
addpath(genpath('d_output'))

%% set invariant parameters 
params = dh0_set_invariant_params();
fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end


%% set up for symmetric GE
networks = [1,0;1,1];
fc = fc_base *[1,foreign_cost_scaling];
ec = ec_base *[1,foreign_cost_scaling];
y = 200;
p = gamma_tilde*w_g /phi_g *[1, tau];
crit = 1e-2;
eq_crit = 1e-1;
give_up_n = 300;
ge_vars = {'networks' 'fc', 'ec', 'y', 'p','crit', 'give_up_n', 'eq_crit'};
for i = 1:length(ge_vars); name = ge_vars{i}; params.(name) = eval(name); end
P_ss = find_symmetric_ss(params, 1);




P = P_ss;
x_bar = params.y * (params.gamma_tilde * params.p).^(-params.gamma) / (P^(1 - params.gamma));
pi_bar = x_bar * params.w_g * params.phi_g^(-1) * (params.gamma - 1)^(-1);
E_x  = x_bar  .* params.A_tilde .* permute(params.networks, [3 2 1]);
E_pi = pi_bar .* params.A_tilde .* permute(params.networks, [3 2 1]);
xi = params.alpha_1 * params.phi_d * E_x.^params.alpha_2;
pe_vars = {'E_x', 'E_pi', 'xi'};
l_params = params; for i = 1:length(pe_vars); name = pe_vars{i}; l_params.(name) = eval(name); end
output = dh10_LCP_inner_loop(zeros(params.len_Sigma, params.num_networks), l_params, false);


function P_opt = find_symmetric_ss(params, P0)
    % Optimization options with early stopping
    options = optimoptions('patternsearch', 'UseParallel', true, ...
        'Display', 'iter', 'UseCompletePoll', true, ...
        'UseCompleteSearch', true, 'AccelerateMesh', true, ...
        'MaxIterations', 1e7,'MaxFunctionEvaluations', 1e7, ...
        'FunctionTolerance', 1e-6, 'OutputFcn', @stop_if_close);

    % Initial point and bounds
    lb = 1e-6;
    ub = [];

    % Run optimization
    [P_opt, ~] = patternsearch(@objective_wrapper, P0, [], [], [], [], lb, ub, [], options);

    % Objective wrapper with caching
    function value = objective_wrapper(P)
        
        % find closest  v_0 starting point 
        persistent cache  
        if ~isempty(cache)
            distances = cellfun(@(entry) abs(P - entry.P), cache);
            [~, idx] = min(distances); v_0 = cache{idx}.v;
        else
            v_0 = zeros(params.len_Sigma, params.num_networks);
        end
        v_0 = zeros(params.len_Sigma, params.num_networks);

        % generate the local version of params w/ variables that depend on P
        x_bar = params.y * (params.gamma_tilde * params.p).^(-params.gamma) / (P^(1 - params.gamma));
        pi_bar = x_bar * params.w_g * params.phi_g^(-1) * (params.gamma - 1)^(-1); 
        E_x  = x_bar  .* params.A_tilde .* permute(params.networks, [3 2 1]);
        E_pi = pi_bar .* params.A_tilde .* permute(params.networks, [3 2 1]);     
        xi = params.alpha_1 * params.phi_d * E_x.^params.alpha_2;
        pe_vars = {'E_x', 'E_pi', 'xi'};
        l_params = params; for i = 1:length(pe_vars); name = pe_vars{i}; l_params.(name) = eval(name); end


        % Evaluate value function iteration
        output = dh10_LCP_inner_loop(v_0, l_params, false);
        value = abs(output.entrance_v - params.ec(1));
        %fprintf("P %g: distance: %g\n", P, value)

        % Update cache
        new_entry = struct('P', P, 'v', output.v);
        if isempty(cache)
            cache = {new_entry};
        else
            cache{end+1} = new_entry;
            if numel(cache) > 20
                cache = cache(end-19:end);  % Keep last 20
            end
        end
    end

    % Output function to stop when miss_value < 0.01
   function [stop, options, optchanged] = stop_if_close(optimValues, options, flag)
    stop = false;
    optchanged = false;
    if strcmp(flag, 'iter') && optimValues.fval < params.eq_crit
        stop = true;
        fprintf('Stopping early: miss_value = %.5f < 0.01\n', optimValues.fval);
    end
end
end