function x_opt = fh6_find_ss(params, num_firms_0, A_tilde_0)

    % Optimization options with early stopping
    options = optimoptions('patternsearch', ...
        'UseParallel', true, ...
        'Display', 'iter', ...
        'UseCompletePoll', true, ...
        'UseCompleteSearch', true, ...
        'AccelerateMesh', true, ...
        'MaxIterations', 1e7, ...
        'MaxFunctionEvaluations', 1e7, ...
        'FunctionTolerance', 1e-6, ...
        'OutputFcn', @stop_if_close);

    % Initial point and bounds
    x0 = [num_firms_0, A_tilde_0];
    lb = 1e-6 * ones(1, 2);
    ub = [];

    % Run optimization
    [x_opt, ~] = patternsearch(@objective_wrapper, x0, [], [], [], [], lb, ub, [], options);

    % Objective wrapper with caching
    function value = objective_wrapper(x)
        persistent cache  % Holds past {x, v} pairs

        % Search for close match in cache
        v_0 = [];
        if ~isempty(cache)
            distances = cellfun(@(entry) norm([x(1), x(2)] - entry.x), cache);
            [~, idx] = min(distances);
            v_0 = cache{idx}.v;
        end
        if isempty(v_0)
            v_0 = zeros(params.I, 1);
        end

        % Evaluate value function iteration
        output = fh5_find_value_func(x(1), x(2), params, v_0);
        value = output.miss_value;

        % Update cache
        new_entry = struct('x', [x(1), x(2)], 'v', output.v);
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

    if strcmp(flag, 'iter') && optimValues.fval < 1e-6
        stop = true;
        fprintf('Stopping early: miss_value = %.5f < 0.01\n', optimValues.fval);
    end
end
end