clear all; close all; clc;
addpath(genpath('../b_helper_functions'))
addpath(genpath('d_helper_functions'))

%% === SET PARAMETER VALUES ===
%== key parameters ==
I = 20;
num_mkts = 2;
LCP_yn =false;
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
fixed_cost_scaling = 1;

%== other parameters ==
% Simulation Parameters
rho = 0.05; %discount rate
Delta = 1000; % 1/Delta = time_step
crit = 10^(-3);
maxit = 100;

% production / data parameters
w_g = 1; % wage for production workers
gamma = 4;     % CES parameter (from BEJK)
gamma_tilde = gamma/(gamma-1);
x_bar = x_scale_factor*phi_g^gamma; % base demand 
pi_bar = x_bar*w_g*phi_g^-1*(gamma-1)^-1; % base profits 

sigma_a = repmat(1.1,1,num_mkts); % sd of noise term


%% === Construct State Space and Derived Params ===
[Q,D,Sigma] = dh1_make_state_space(num_mkts,I, lambda_tilde, sigma_z, theta);
Sigma_mat = dh2_restructure_array(repmat(Sigma,1,1,num_networks),true, num_mkts);

% ancillary params 
num_state_vars = size(Sigma,2);
len_Sigma = size(Sigma,1);
d_Sigma =  (Sigma(len_Sigma, :) - Sigma(1,:))/(I-1);
diag_indeces = find(ismember(find(triu(true(num_mkts), 0)),1:num_mkts+1:num_mkts^2));

% Construct Expected Quality / Quantity / Profits 
A_tilde = dh0_gen_A_tilde(top_bottom_quality_ratio,Sigma, sigma_a, diag_indeces);
E_x = x_bar.*A_tilde.* permute(networks, [3 2 1]);
E_pi = pi_bar.*A_tilde.* permute(networks, [3 2 1]); % Expected working profits (not accounting for fixed costs; data labor)
xi = alpha_1*phi_d*E_x.^alpha_2;


%% ==== SET INITIAL GUESS FOR VALUE FUNCTION 
% determine zero drift state in network[1,0] 2 w/o data 
init_index = dh3_determine_init_index(Sigma_mat,Sigma, D,Q,sigma_a,num_mkts);
working_profit_base = sum(E_pi(init_index,:,1));
fc = working_profit_base; fc = [fc, repmat(fc*fixed_cost_scaling,1,num_mkts-1)];
ec = fc*2; rev_ec = repmat(.001,1, num_mkts);

% initial guess is zero; value of staying in zero drift state forever 
v0 = zeros(len_Sigma, num_networks);

v = v0;

%% ==== Perform Value Function Iteration ===
for n=1:maxit

% establish derivative approximations
[dv_f, dv_b] = dh4_make_derivatives(v, I, num_state_vars, num_networks, d_Sigma);

% carry out the upwind 
dv_final = dh6_upwind(dv_b, dv_f, Sigma_mat, Q, D, w, phi_d, alpha_1, alpha_2, sigma_a, networks, E_x, E_pi, xi, fc);

% determine optimal behavior 
optim =  dh5_optim_calc(dv_final, Sigma_mat, Q, D, w, phi_d, alpha_1, alpha_2, sigma_a, networks, E_x, E_pi, xi, fc);

% construct the transition matrix 
[A_matrix, A_matrix_cells] = dh7_make_A_matrix(optim.drift,d_Sigma);

% update V
V = dh8_update_V(LCP_yn,optim.profit_w_actions,v, A_matrix,A_matrix_cells, rho, Delta, networks, ec, rev_ec);

% prepare for next iteration 
Vchange = V - v;
v =  .75*v +.25*V;
[max_val, max_index ] = max(abs(Vchange(:)));
dist(n) = max_val;
fprintf('max divergence: %g; mean divergence: %g\n',Vchange(max_index), mean(Vchange(:)));
if dist(n)<crit
    fprintf('Value Function Converged, Iteration = %g\n',n)
    break
end
end

