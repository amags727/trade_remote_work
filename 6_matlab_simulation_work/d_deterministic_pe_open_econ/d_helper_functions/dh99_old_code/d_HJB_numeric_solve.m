function v_opt = d_HJB_numeric_solve(params, v0,network)


v = v0(:,network);
l_params = params;
l_params.num_networks = 1;
l_params.Sigma_mat = params.Sigma_mat(:,:,:,1);
l_params.networks = params.networks(network,:);
l_params.E_x = params.E_x(:,:,network);
l_params.E_pi = params.E_pi(:,:,network);
l_params.xi = params.xi(:,:,network);


options = optimoptions('surrogateopt', 'UseParallel', true);

[v_final, ~] = patternsearch(@objective_wrapper, v0, [], [], [], [], [], [], [], options);


function value = objective_wrapper(v)
    % persistent cache  % Holds past {x, v} pairs
    %  if ~isempty(cache)
    %      init_indices= 1:l_params.len_Sigma;
    %  else
    %      same_vals = cellfun(@(entry) sum(v ==entry.v), cache);
    %      [~, idx] = max(same_vals);
    % 
    %  end

[dv_f, dv_b] = dh4_make_derivatives(v,l_params);
dv_final = dh6_upwind(dv_b, dv_f, l_params);
optim = dh5_optim_calc(dv_final, l_params);
value = sum(abs(l_params.rho*v -  optim.ham));
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





