clear all; close all; clc;
addpath(genpath('../b_helper_functions'));
addpath(genpath('d_helper_functions'))
addpath(genpath('d_output'))

% set invariant parameters 
params = dh0_set_invariant_params();


fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end

%% == variation parameters ==
networks = [1,0;1,1];
x_scale_factor = repmat(20, 1,num_mkts); % what is underlying level of demand 
x_bar = x_scale_factor*phi_g^gamma; % base demand 
pi_bar = x_bar*w_g*phi_g^-1*(gamma-1)^-1; % base profits 

E_x = x_bar.*A_tilde.* permute(networks, [3 2 1]);
E_pi = pi_bar.*A_tilde.* permute(networks, [3 2 1]); % Expected working profits (not accounting for fixed costs; data labor)
xi = alpha_1*phi_d*E_x.^alpha_2;
% Construct V0 fc/ entry costs 
[v0,fc,ec] = dh3_set_v0_fc_ec(Sigma_mat,Sigma, D,Q,sigma_a,pi_bar, A_tilde,networks, fixed_cost_scaling,ec_multiplier, rho);

% construct the base parameter structure 
var_names = { 'I', 'num_state_vars', 'num_networks','num_mkts', 'len_Sigma', 'd_Sigma','Sigma', 'Sigma_mat', 'Q', 'D',... 
    'w', 'phi_d', 'alpha_1', 'alpha_2', 'sigma_a', 'networks', 'E_x', 'E_pi', ...
    'xi', 'fc', 'rho', 'Delta', 'ec', 'rev_ec', 'maxit', 'crit', 'adjacency_matrix'};
base_params = struct();for i = 1:length(var_names); name = var_names{i}; base_params.(name) = eval(name); end

tic
v_hjb_init = dh9_HJB_inner_loop(v0,base_params);
v_LCP_init = dh10_LCP_inner_loop(v_hjb_init, base_params);
toc


params = base_params; lambda_tilde_vec = lambda_tilde * linspace(.9,1.1,50);
params.crit = 1e-2;  graph_output = cell(1, 3);  params.maxit =6000;
last_v = v0;
for i = 1:length(lambda_tilde_vec)
    disp(i)
    l_lambda_tilde = lambda_tilde_vec(i);
    l_params = params;
    [l_params.Q,~,l_params.Sigma] = dh1_make_state_space(num_mkts,I, l_lambda_tilde, sigma_z, theta);
    l_params.Sigma_mat = dh2_restructure_array(repmat(l_params.Sigma,1,1,num_networks),true, num_mkts);
    l_params.d_Sigma =  (l_params.Sigma(len_Sigma, :) - l_params.Sigma(1,:))/(I-1);
    last_v = dh8_HJB_inner_loop(last_v, l_params);
end




%% Solve Problem for Init Values 
if exist('d_output/do1_init_v.mat', 'file') == 2
    init_v = load('d_output/do1_init_v.mat').init_v;
else
v_hjb_init = dh9_run_inner_loop(v0, false, base_params);
[v_lcp_init,~,~,~,~,Vchange] = dh9_run_inner_loop(v_hjb_init, true, base_params);
init_v = struct('hjb', v_hjb_init, 'lcp', v_lcp_init);
save('d_output/do1_init_v.mat', 'init_v')
[graph_output] = dh10_graph_output(init_v.lcp, base_params,false);
save('d_output/do2_base_graph.mat', 'graph_output', '-v7');
end 

