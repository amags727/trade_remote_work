function output = dh1_find_symmetric_ss(params, P0, y)
    params.y = [y,y];
    % Define bounds (adjust upper bound if needed)
    lb = .9*P0;
    ub = 1.1*P0; % Example heuristic for upper bound

    % Optimization options
    opts = optimset('Display', 'off', 'TolX', 1e-10, 'TolFun', 1e-6);

    % Run optimization
    output = [];
    [P, ~] = fminbnd(@objective_wrapper, lb, ub, opts);
    params.P = [P,P];
    output = dh10_LCP_inner_loop(output.v, params,true);

    % Determine num firms 
    gamma = params.gamma;
    A_tilde = output.A_tilde_out;
    p = output.p;
    output.num_firms = P^(1-gamma) / (A_tilde(1)*p(1)^(1-gamma) + A_tilde(2)*p(2)^(1-gamma));

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
            output = dh10_LCP_inner_loop(v_0, l_params, false);
        else
            v_hjb = dh9_HJB_inner_loop(zeros(params.len_Sigma, params.num_networks),l_params);
            output  = dh10_LCP_inner_loop(v_hjb, l_params, false);
        end
        value = abs(output.entrance_v - params.ec(1));
        fprintf('P = %g; excess value = %g\n',P, output.entrance_v - params.ec(1))

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
