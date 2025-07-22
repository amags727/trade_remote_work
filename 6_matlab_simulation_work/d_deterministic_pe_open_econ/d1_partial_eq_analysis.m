clear all; close all; clc;
addpath(genpath('../b_helper_functions'));
addpath(genpath('d_helper_functions'))
addpath(genpath('d_output'))

% set invariant parameters 
params = dh0_set_invariant_params();

fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end


%% === PE setup ====
networks = [1,0;1,1];
x_scale_factor = 20;
fc = fc_base *[1,foreign_cost_scaling];
ec = ec_base *[1,foreign_cost_scaling];
x_bar = x_scale_factor*phi_g^gamma; % base demand 
pi_bar = x_bar*w_g*phi_g^-1*(gamma-1)^-1; % base profits 


E_x = x_bar.*A_tilde.* permute(networks, [3 2 1]);
E_pi = pi_bar.*A_tilde.* permute(networks, [3 2 1]); % Expected working profits (not accounting for fixed costs; data labor)
xi = alpha_1*phi_d*E_x.^alpha_2;
v0 = zeros(len_Sigma, num_networks);

pe_vars = {'x_scale_factor','networks' 'fc', 'ec', 'E_x', 'E_pi', 'xi'};
for i = 1:length(pe_vars); name = pe_vars{i}; params.(name) = eval(name); end


v_hjb_init = dh9_HJB_inner_loop(v0,params);
output  = dh10_LCP_inner_loop(v_hjb_init, params);

%% == Symmetric setup ===
networks = [1,0;1,1];
fc = fc_base *[1,foreign_cost_scaling];
ec = ec_base *[1,foreign_cost_scaling];
y = 200;

P_0 = 1;


P= P_0;
p = gamma_tilde*w_g /phi_g *[1, tau];
x_bar = y*(gamma_tilde*p).^(-gamma) / (P^(1-gamma));
pi_bar = x_bar*w_g*phi_g^-1*(gamma-1)^-1; % base profits 

E_x = x_bar.*A_tilde.* permute(networks, [3 2 1]);
E_pi = pi_bar.*A_tilde.* permute(networks, [3 2 1]); % Expected working profits (not accounting for fixed costs; data labor)
xi = alpha_1*phi_d*E_x.^alpha_2;

pe_vars = {'networks' 'fc', 'ec', 'E_x', 'E_pi', 'xi'};
for i = 1:length(pe_vars); name = pe_vars{i}; params.(name) = eval(name); end


v_hjb_init = dh9_HJB_inner_loop(zeros(len_Sigma, num_networks),params);
output  = dh10_LCP_inner_loop(zeros(len_Sigma, num_networks), params);




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

