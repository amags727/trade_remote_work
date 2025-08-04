clear all; close all; clc;
addpath(genpath('../b_helper_functions'));
addpath(genpath('d_helper_functions'))
addpath(genpath('d_output'))
dbstop if error

%% setup 
params = dh0_set_invariant_params();
params.networks = [1,0;1,1];
params.y = [10,10];

output_ss = find_symmetric_ss(params, 1.25529);

function output = find_symmetric_ss(params, P0)
    % Define bounds (adjust upper bound if needed)
    lb = .8*P0;
    ub = 1.15*P0; % Example heuristic for upper bound

    % Optimization options
    opts = optimset('Display', 'off', 'TolX', 1e-6, 'TolFun', 1e-6);

    % Run optimization
    output = [];
    [output.demand_opt, ~] = fminbnd(@objective_wrapper, lb, ub, opts);
    % Objective wrapper with caching
    function value = objective_wrapper(P)
        l_params = params;
        l_params.P = [P,P];

        persistent cache  % Holds past {P, v} pairs
        % Search for close match in cache
        if ~isempty(cache)
            distances = cellfun(@(entry) abs(P - entry.P), cache);
            [~, idx] = min(distances);
            v_0 = cache{idx}.v;
            output = dh10_LCP_inner_loop(v_0, l_params);
        else
            v_hjb = dh9_HJB_inner_loop(zeros(params.len_Sigma, params.num_networks),l_params);
            output  = dh10_LCP_inner_loop(v_hjb, l_params);
        end
        value = abs(output.entrance_v - params.ec(1));
        fprintf('P = %g; time to enter market 2 = %g; excess value = %g\n',P,output.market_2_entrance_t, output.entrance_v - params.ec(1))

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
end
