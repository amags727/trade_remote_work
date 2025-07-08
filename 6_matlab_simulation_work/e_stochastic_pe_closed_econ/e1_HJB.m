clear all; close all; clc;
addpath(genpath('../e_helper_functions'))

% production parameters 
w = 1; % wage
phi_g = 1; % goods productivity
gamma = 4;     % CES parameter (from BEJK)
gamma_tilde = gamma/(gamma-1);
nu = .25;

% data parameters 
phi_d = 1; % data productivity 
alpha_1 = .5; % cobb douglas coefficient on data labor 
alpha_2 = .5; % cobb douglas coefficient on raw data 
sigma_a = 1.1; % sd of noise term 
sqrt_Q = 1.1; % sd of random component of z
Q = sqrt_Q^2;
theta = .25; % mean reversion parameter of z (closer to one faster mean reversion)

% Simulation Parameters
rho = 0.05; %discount rate
Delta = 100; % 1/Delta = time_step
crit = 10^(-6);
maxit = 500;

% define state space boundaries for both z; z_hat; a
z_ub = 2.576*sqrt_Q / sqrt(2*theta); % 99th percentile of stationary distrib
z_lb = -z_ub; %1st percentile of stationary distrib 

% define state boundaries for Sigma 
Sigma_ub = Q /(2*theta); % Sigma_ub is the value such that drift = 0 when the firm doesn't participate in market 
Sigma_lb = 1e-2;

% define the state space; 
I = 12;
num_state_vars = 4;
lb = [repmat(z_lb,1,3), Sigma_lb]; ub = [repmat(z_ub,1,3), Sigma_ub];
grid_vectors = cell(1, num_state_vars);
for i = 1:length(lb)
    grid_vectors{i} = linspace(lb(i), ub(i), I);
end

grid = cell(1, length(lb)); [grid{:}] = ndgrid(grid_vectors{:});
grid_matrix = cellfun(@(x) x(:), grid, 'UniformOutput', false);
state_space = [grid_matrix{:}];
z_act = state_space(:,1); z_hat = state_space(:,2); 
a_act = state_space(:,3); Sigma = state_space(:,4);
len_state = size(state_space,1);
d_state_space = (ub - lb) / (I-1);

% Define Expected Quality (A_tilde) and related values 
C = (Sigma_ub + sigma_a^2);
ndraws = 10000;
eta = randn(len_state,10000) .*repmat((Sigma+ sigma_a^2).^(1/2),1,ndraws) +...
    repmat((a_act - z_hat),1,ndraws);
penalty =  mean(C*eta.^2 ./(eta.^2 +C),2);
A_bar = max(penalty) + (max(penalty)-min(penalty));
A_tilde = A_bar - penalty;
x_bar = 1/min(A_tilde); % base demand 
pi_bar = x_bar*w*phi_g^-1*(gamma-1)^-1; % base profits 
E_x = x_bar*A_tilde;
E_pi = pi_bar*A_tilde;
xi = alpha_1*phi_d*E_x.^alpha_2;

% guess v_0; 
% all states have long term profits of a perfectly forecast a, with
% Sigma set at ss value with no data investment 
R_0 = sigma_a^2;
Sigma_0 = -theta*R_0 + sqrt(theta^2*R_0^2 + Q*R_0);
[~, idx] = min(abs(Sigma - Sigma_0));
idx_0 = find(z_hat == a_act & Sigma== Sigma(idx),1, 'first' );
V_0 = repmat(A_tilde(idx_0)*pi_bar / rho, len_state,1);
v= V_0;


%% Perform value func iteration 
for n=1:maxit
V= v;
[dv_f, dv_b, dv_2] = e1_compute_derivatives(v, d_state_space,I, num_state_vars, len_state);
dv_final = e1_resolve_upwind_directions(dv_b, dv_f, dv_2, ...
    z_act, z_hat, Sigma, xi, E_x, E_pi, ...
    w, alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, len_state, num_state_vars);

optim =  e1_optim_calc(dv_final, dv_2, w, z_act, z_hat, Sigma, xi, alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, E_x, E_pi);
A_matrix = e1_construct_A_matrix(optim, d_state_space,I, num_state_vars, len_state);


 %SOLVE FOR NEW V
B = (rho + 1/Delta)*speye(len_state) - A_matrix;
b =  optim.pi_with_actions + V/Delta;
V = B\b;
Vchange = V - v;
v =  V;


[max_val, max_index ] = max(abs(Vchange));
dist(n) = max_val;
fprintf('max divergence: %g; mean divergence: %g\n',Vchange(max_index), mean(Vchange));

    if dist(n)<crit
            disp('Value Function Converged, Iteration = ')
            disp(n)
            break
    end
end

