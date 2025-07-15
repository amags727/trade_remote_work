clear all; close all; clc;

addpath(genpath('../b_helper_functions'))
addpath(genpath('f_helper_functions'))

%% === SET PARAMETER VALUES ===
%== key parameters ==
I = 20;
num_mkts = 2;
LCP_yn =false;
networks = [1,0;0,1]; %networks = [0,0;1,0;1,1]; %networks = dec2bin(0:(2^num_mkts - 1)) - '0';
num_networks = size(networks,1);

w = 1; % data worker wage
phi_d = 1; % data productivity 
alpha_1 = .5;  % cobb douglas coefficient on data labor 
alpha_2 = .5; % cobb douglas coefficient on raw data 
top_bottom_quality_ratio = 2; % how much data helps improve quality 
x_scale_factor = repmat(10, 1,num_mkts); % what is underlying level of demand 
phi_g = 1;
sigma_z = repmat(1.1,1,num_mkts); % variance of random component of z
theta = .9; % mean reversion parameter of z (closer to one faster mean reversion)
lambda_tilde = .5; % correlation coefficient between mkts 
fixed_cost_scaling = 1.1;

%== other parameters ==
% Simulation Parameters
rho = 0.05; %discount rate
Delta = 100; % 1/Delta = time_step
crit = 10^(-4);
maxit = 200;

% production / data parameters
w_g = 1; % wage for production workers
gamma = 4;     % CES parameter (from BEJK)
gamma_tilde = gamma/(gamma-1);
x_bar = x_scale_factor*phi_g^gamma; % base demand 
pi_bar = x_bar*w_g*phi_g^-1*(gamma-1)^-1; % base profits 

sigma_a = repmat(1.1,1,num_mkts); % sd of noise term


%% === Construct State Space and Derived Params ===
[Q,D,state_space,Sigma] = fh1_make_state_space(num_mkts,I, lambda_tilde, sigma_z, theta);
Sigma_mat = fh2_restructure_array(repmat(Sigma,1,1,num_networks),true, num_mkts);

% ancillary params 
num_state_vars = size(state_space,2);
len_Sigma = size(Sigma,1);
d_state_space =  (state_space(len_Sigma, :) - state_space(1,:))/(I-1);

% Construct Expected Quality / Quantity / Profits 
A_tilde = fh3_gen_A_tilde(top_bottom_quality_ratio,state_space, sigma_a, num_mkts);
E_x = x_bar.*A_tilde.* permute(networks, [3 2 1]);
E_pi = pi_bar.*A_tilde.* permute(networks, [3 2 1]); % Expected working profits (not accounting for fixed costs; data labor)
xi = alpha_1*phi_d*E_x.^alpha_2;

% set v0, fc, ec
% FC are set to just cover a firm in one mkt at ss w/o data 
% we assume all firms in each network stay at their 0 data ss forever 
[v0,fc] = fh4_set_v0_and_fc(Sigma_mat,Sigma, D,Q,sigma_a, ...
    pi_bar, A_tilde,networks, fixed_cost_scaling,rho);
ec = repmat(.1,1, num_mkts); rev_ec = repmat(.001,1, num_mkts);

%% ==== Perform Value Function Iteration ===
smoother = .6;
[v_HJB_default,optim,z, best_alt, dv_final] =fh10_value_func_iteration(v0,smoother, false, ...
    maxit, crit, I, num_state_vars, num_networks, d_state_space, state_space, Sigma_mat, Q, D, w, phi_d, alpha_1, alpha_2, sigma_a, networks, E_x, E_pi, xi, fc, rho, Delta, ec, rev_ec);

[v_LCP_default,optim,z, best_alt]  =fh10_value_func_iteration(v_HJB_default,smoother,true, ...
    maxit, crit, I, num_state_vars, num_networks, d_state_space, state_space, Sigma_mat, Q, D, w, phi_d, alpha_1, alpha_2, sigma_a, networks, E_x, E_pi, xi, fc, rho, Delta, ec, rev_ec);


%% ===Simulate firm trajectory starting at Sigma_ub===


% setup 
T = 1000;                 
state_path = zeros(T, num_state_vars);
data_use_path = zeros(T, num_mkts); 
output_path = zeros(T, num_mkts);
profit_path = zeros(T, 1);
network_path = ones(T,1);
% determine if firm will change networks 
z_mat = reshape(z,[],num_networks);
preferred_network = repmat(1:num_networks, len_Sigma,1).*(reshape(z,[],num_networks)>0) +...
                    best_alt.*(reshape(z,[],num_networks) ==0);

% establish starting state 
init_state = fake_layp(D, Q); init_state = init_state([1,2,4]);
[~,init_state_index] = min(sum((Sigma - init_state).^2,2));
state_path(1, :)= state_space(init_state_index,:);
for t = 1:T
    state_t = state_path(t,:);
    network_t = network_path(t);
    [indices, weights] = fh11_interp_box(state_t, state_space, 2);
    data_use_path(t,:) =  sum(optim.L(indices, :, network_t) .* weights); 
    output_path(t,:) =  sum(E_x(indices, :, network_t) .* weights); 
    profit_path(t,:) = sum(optim.profit_w_actions(indices, network_t).*weights);
    drift_t = sum(optim.drift(indices,:,network_t).*weights);

    if (t ~= T)
        state_path(t+1,:) = state_t + drift_t*1/Delta;
        [indices, weights] = fh11_interp_box( state_path(t+1,:),state_space, 2);
        best_score = -inf; best_network = 1; pref_base = [preferred_network(indices,network_t),weights];
        for network = 1:num_networks
          score = sum(pref_base(pref_base(:,1) == network, 2));
          if score > best_score 
              best_score = score; best_network = network;
          end
        end
        network_path(t+1) = best_network;
    end 
end