function output = fh6_find_ss(params, num_firms_0, A_tilde_0)
    % Set up optimization parameters
    lb = 1e-6;
    options = optimoptions('fmincon', 'Display', 'off', 'Algorithm', 'sqp');

    % Outer optimization over A_tilde
    objective = @(A_tilde) inner_loop_ss(params, num_firms_0, A_tilde, options, lb).abs_A_tilde_diff;
    A_tilde_out = fmincon(objective, A_tilde_0, [], [], [], [], 1.2, [], [], options);

    % Final output from optimized A_tilde
    output = inner_loop_ss(params, num_firms_0, A_tilde_out, options, lb);
    output.A_tilde_out = A_tilde_out;
end

function output = inner_loop_ss(params, num_firms_0, A_tilde_in, options, lb)
    % Inner optimization over num_firms
    objective = @(num_firms) fh5_find_value_func(num_firms, A_tilde_in, params).abs_entrance_v;
    num_firms_ss = fmincon(objective, num_firms_0, [], [], [], [], .7, [], [], options);

    % Evaluate function at solution
    output = fh5_find_value_func(num_firms_ss, A_tilde_in, params);
    output.num_firms_ss = num_firms_ss;

    % Interpolate steady-state A_tilde
    drift = output.optimal.drift;
    i = find(drift > 0, 1, 'last');
    w = drift(i) / (drift(i) - drift(i+1));
    output.A_tilde_out = (1 - w) * params.A_tilde(i) + w * params.A_tilde(i+1);

    % Squared deviation from guessed A_tilde
    output.abs_A_tilde_diff = (A_tilde_in - output.A_tilde_out)^2;
end