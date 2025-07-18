clear all; close all; clc;
addpath(genpath('../b_helper_functions'))
addpath(genpath('d_helper_functions'))
addpath(genpath('d_output'))

%% == Set key parameters ==
I = 20;
num_mkts = 2;
networks = [1,0;1,1]; %networks = [0,0;1,0;1,1]; %networks = dec2bin(0:(2^num_mkts - 1)) - '0';
num_networks = size(networks,1);

w = 1; % data worker wage
phi_d = 1; % data productivity 
alpha_1 = .5;  % cobb douglas coefficient on data labor 
alpha_2 = .5; % cobb douglas coefficient on raw data 
top_bottom_quality_ratio = 2; % how much data helps improve quality 
x_scale_factor = repmat(20, 1,num_mkts); % what is underlying level of demand 
phi_g = 1;
sigma_z = repmat(1.1,1,num_mkts); % variance of random component of z
theta = .9; % mean reversion parameter of z (closer to one faster mean reversion)
lambda_tilde = .5; % correlation coefficient between mkts 
fixed_cost_scaling = 1.1; %fc * fixed_cost_scaling = foreign market fixed costs 
ec_multiplier = 1.6; % fc * ec_multiplier = ec 


%% == Set other parameters ==
% Simulation Parameters
rho = 0.05; %discount rate
Delta = 1000; % 1/Delta = time_step
crit = 10^(-4);
maxit = 3000;

% production / data parameters
w_g = 1; % wage for production workers
gamma = 4;     % CES parameter (from BEJK)
gamma_tilde = gamma/(gamma-1);
x_bar = x_scale_factor*phi_g^gamma; % base demand 
pi_bar = x_bar*w_g*phi_g^-1*(gamma-1)^-1; % base profits 
sigma_a = repmat(1.1,1,num_mkts); % sd of noise term
rev_ec = repmat(5,1, num_mkts); % exit costs from market 

%% === Construct State Space and Derived Params ===
[Q,D,Sigma] = dh1_make_state_space(num_mkts,I, lambda_tilde, sigma_z, theta);
Sigma_mat = dh2_restructure_array(repmat(Sigma,1,1,num_networks),true, num_mkts);

% ancillary params 
num_state_vars = size(Sigma,2);
len_Sigma = size(Sigma,1);
d_Sigma =  (Sigma(len_Sigma, :) - Sigma(1,:))/(I-1);
diag_indeces = find(ismember(find(triu(true(num_mkts), 0)),1:num_mkts+1:num_mkts^2));

% Construct Expected Quality / Quantity / Profits 
A_tilde  = dh0_gen_A_tilde(top_bottom_quality_ratio,Sigma, sigma_a, diag_indeces);
E_x = x_bar.*A_tilde.* permute(networks, [3 2 1]);
E_pi = pi_bar.*A_tilde.* permute(networks, [3 2 1]); % Expected working profits (not accounting for fixed costs; data labor)
xi = alpha_1*phi_d*E_x.^alpha_2;

% Construct V0 fc/ entry costs 
[v0,fc,ec] = dh3_set_v0_fc_ec(Sigma_mat,Sigma, D,Q,sigma_a,pi_bar, A_tilde,networks, fixed_cost_scaling,ec_multiplier, rho);

% construct the base parameter structure 
var_names = { 'I', 'num_state_vars', 'num_networks','num_mkts', 'len_Sigma', 'd_Sigma','Sigma', 'Sigma_mat', 'Q', 'D',... 
    'w', 'phi_d', 'alpha_1', 'alpha_2', 'sigma_a', 'networks', 'E_x', 'E_pi', ...
    'xi', 'fc', 'rho', 'Delta', 'ec', 'rev_ec', 'maxit', 'crit'};

base_params = struct();for i = 1:length(var_names); name = var_names{i}; base_params.(name) = eval(name); end
tic
v_hjb_init = dh8_HJB_inner_loop(v0,base_params);
toc


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

