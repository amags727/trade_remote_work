clear all; close all; 
addpath(genpath('f_helper_functions'))

% set baseline params 
params = fh1_import_fixed_params(2,1);
last_output = fh6_find_ss(params, 1);
disp(last_output.v_ss)

% examine results from last iteration of 
% look at phi results 
Sigma_scalar_vec = linspace(1,3,100);
results = zeros(length(Sigma_scalar_vec), 3);

for i = 1:length(Sigma_scalar_vec)
    disp(i)
    params = fh1_import_fixed_params(Sigma_scalar_vec(i),1);
    last_output = fh6_find_ss(params,last_output.P_opt);
    results(i,:) = [Sigma_scalar_vec(i), last_output.ss_index, last_output.v_ss];
end
