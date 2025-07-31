function P_opt = fh6_find_ss(params, P0)

    % Define bounds (adjust upper bound if needed)
    lb = 1e-6;
    ub = max(P0 * 10, 1);  % Example heuristic for upper bound

    % Optimization options
    opts = optimset('Display', 'off', 'TolX', 1e-6, 'TolFun', 1e-6);

    % Run 1D optimization
    [P_opt, ~] = fminbnd(@objective_wrapper, lb, ub, opts);

    % Objective wrapper with caching
    function value = objective_wrapper(P)
        persistent cache  % Holds past {P, v} pairs

        % Search for close match in cache
        if ~isempty(cache)
            distances = cellfun(@(entry) abs(P - entry.P), cache);
            [~, idx] = min(distances);
            v_0 = cache{idx}.v;
        else
            v_0 = zeros(params.I, 1);
        end

        % Evaluate value function iteration
        output = fh5_find_value_func(P, params, v_0, false);
        value = output.miss_value;

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