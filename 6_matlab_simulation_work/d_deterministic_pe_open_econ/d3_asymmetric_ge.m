clear all; close all; clc;
addpath(genpath('../b_helper_functions'));
addpath(genpath('d_helper_functions'))
addpath(genpath('d_output'))
dbstop if error

%% setup
params = dh0_set_invariant_params();
params.networks = [1,0;1,1];

params.y = [10,9.9];
P0 = 1.25529*ones(1,2);
[dual_output, P_opt] = find_asymmetric_ss(params,P0);

function [dual_output, P_opt] = find_asymmetric_ss(params,P0)

% Optimization options with early stopping
options = optimoptions('patternsearch','UseParallel', true, ...
    'Display', 'iter', 'UseCompletePoll', true, ...
    'UseCompleteSearch', true,  'AccelerateMesh', false, ...
    'MaxIterations', 1e7,  'MaxFunctionEvaluations', 1e7, ...
    'MeshTolerance',1e-9, 'FunctionTolerance', 1e-6, 'OutputFcn', @stop_if_close);

lb = .97*P0;
ub = 1.03*P0;

% Run optimization
[P_opt,~] = patternsearch(@objective_wrapper, P0, [], [], [], [], lb, ub, [], options);
m = load('dual_output.mat'); dual_output = m.dual_output;
delete('dual_output.mat'); delete('cache_backup.mat');
    
    function value = objective_wrapper(P)

        % load the cache if it exists
        if exist('cache_backup.mat', 'file'); s = load('cache_backup.mat'); cache = s.cache; else cache = {}; end
        empty_cache = isempty(cache);

        % prepare output shells
        dual_output = cell(2,1);
        dual_miss_val = cell(2,1);
        if ~empty_cache
            distances = cellfun(@(entry) norm(P - entry.P), cache);
            [~, idx] = min(distances);
            dual_v = cache{idx}.dual_v;
        else
            dual_v = cell(2,1);
        end

        % run inner loop for each base country
        parfor i = 1:2
            l_params = params;
            l_params.P = P;
            if i==2; l_params.P = flip(P); l_params.y = flip(l_params.y); end
            if empty_cache

                v_hjb = dh9_HJB_inner_loop(zeros(params.len_Sigma, params.num_networks),l_params);
                t_output = dh10_LCP_inner_loop(v_hjb, l_params);
            else
                t_output = dh10_LCP_inner_loop(dual_v{i}, l_params);
            end
            dual_output{i} = t_output
            dual_miss_val{i} = abs(t_output.entrance_v - params.ec(i));
            dual_v{i} = t_output.v;
        end
        value = sum([dual_miss_val{:}]);
        display_miss = [dual_output{1}.entrance_v - params.ec(1), dual_output{2}.entrance_v - params.ec(2)];
        fprintf('P1 = %g; P2 = %g; miss value = (%g, %g)\n', P(1), P(2), display_miss(1), display_miss(2));
        new_entry = struct('P', {P}, 'dual_v', {dual_v});


        % wait in line to add to cache 
        lockfile = 'cache_backup.lock'; t = 1;max_wait = 500;
        while exist(lockfile, 'file') & t < max_wait; pause(0.05 + rand()*0.05); end
        if t < max_wait
            fclose(fopen(lockfile, 'w'));

            % update to latest version of cache and save
            if exist('cache_backup.mat', 'file'); s = load('cache_backup.mat'); cache = s.cache; else cache = {}; end
            if isempty(cache); cache = {new_entry}; else
                cache{end+1} = new_entry;
                if numel(cache) > 20
                    cache = cache(end-19:end);  % Keep last 20
                end
            end
            save('cache_backup.mat', 'cache'); save('dual_output.mat', 'dual_output');
        end

        %unlock other workers
        delete(lockfile);

    end
    
    % Output function to stop when miss_value < 0.01
    function [stop, options, optchanged] = stop_if_close(optimValues, options, flag)
        stop = false;
        optchanged = false;

        if strcmp(flag, 'iter') && optimValues.fval < 1e-3
            stop = true;
            fprintf('Stopping early: miss_value = %.5f < 0.01\n', optimValues.fval);
        end
    end

end


