clear all; close all; 
addpath(genpath('f_helper_functions'))

% set baseline params 
params = fh1_import_fixed_params(1);
base_output = fh6_find_ss(params, 1,1);


% look at phi results 
phi_vec = linspace(0,5,100);
results = zeros(length(phi_vec),10);
results(:,1) = phi_vec;

if isempty(gcp('nocreate'))
    parpool; % uses default settings; you can specify workers if needed
end
parfor i = 1:length(phi_vec)
    disp(i)
    % Make a local copy of params
    local_params = params;
    local_params.phi_d = phi_vec(i);

    % Call optimization function
    output = fh6_find_ss(local_params, base_output.num_firms_ss, base_output.A_tilde_out);

    % Find the steady state index (split between two) 
    drift = output.optimal.drift; 
    j = find(drift > 0, 1, 'last');
    w = drift(j) / (drift(j) - drift(j+1));

    % find steady state outputs 
    gamma = local_params.gamma;
    E_x = output.E_x; Sigma = local_params.Sigma; profit = output.optimal.pi_with_actions;
    L = output.optimal.L; Sigma_ub = local_params.Sigma(local_params.I);
    num_firms = output.num_firms_ss;
    A_tilde_ss = output.A_tilde_out;

    E_x = (1-w)*E_x(j) + w*E_x(j+1);
    Sigma = (1-w)*Sigma(j) + w*Sigma(j+1);
    profit = (1-w)*profit(j) + w*profit(j+1);
    L = (1-w)*L(j) + w*L(j+1);
    certainty = 1- Sigma/Sigma_ub;
    agg_X = E_x * (A_tilde_ss^(1/gamma) * num_firms)^(gamma/(gamma-1));

    % output results 
    results(i, :) = [phi_vec(i),num_firms, A_tilde_ss,E_x, Sigma, profit, L, certainty, agg_X, output.converged];
end
results = array2table(results,...
'VariableNames', {'phi_d','num_firms', 'A_tilde', 'E_x', 'Sigma', 'profit', 'L', 'certainty', 'agg_X', 'converged'});
