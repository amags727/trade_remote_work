function params = fh1_import_fixed_params(num_firms_0)
params = struct();

% key params 
I = 100;
w = 1; % data worker wage
phi_d = 1; % data productivity --> this 
alpha_1 = .5;  % cobb douglas coefficient on data labor 
alpha_2 = .5; % cobb douglas coefficient on raw data 
top_bottom_quality_ratio = 2; % how much data helps improve quality 
Sigma_pen_ratio = 1;
y = 10;  
phi_g = 1;
Q = 1.1.^2; % variance of random component of z
theta = .9; % mean reversion parameter of z (closer to one faster mean reversion)


% other params 
sigma_a = 1.1; % sd of noise term 
w_g = 1; % wage for production workers 
gamma = 4;     % CES parameter (from BEJK)
gamma_tilde = gamma/(gamma-1);
rho = 0.05; %discount rate
Delta = 1000; % 1/Delta = time_step
crit = 10^(-6); % tolerance for value function iteration 
maxit = 500; % max number of value func iterations 

% Define the state space 
Sigma_ub = Q /(2*theta); % Sigma_ub is the value such that drift = 0 when the firm doesn't participate in market 
Sigma_lb = 0;
Sigma = linspace(Sigma_lb,Sigma_ub, I)';
d_Sigma = Sigma(2) - Sigma(1);
A_tilde = fh2_gen_A_tilde(top_bottom_quality_ratio,Sigma, sigma_a);
Sigma_pen = 1 + (Sigma_pen_ratio-1) * ((Sigma - Sigma(1)) ./ (Sigma(I) - Sigma(1)));
Sigma_pen = Sigma_pen.^(-1);
% find the steady state index with no data expenditure 
R_0 = sigma_a^(-2);
Sigma_0 = (-theta + sqrt(theta^2 + Q*R_0))/R_0;
[~, idx_no_data_ss] = min(abs(Sigma - Sigma_0));


% we base fixed costs on our initial guess of num_firms 
optimal_p = gamma_tilde* w/phi_g; 

P = num_firms_0^(1/(1-gamma)).*optimal_p;
x_bar = y*(gamma_tilde*w)^(-gamma)/ (P^(1-gamma));
pi_bar =  x_bar*w_g*phi_g^-1*(gamma-1)^-1;
fc =  3.0051; %fc = pi_bar*A_tilde(idx_no_data_ss) where A_tilde set with P = 1 and standard params 


vars = {'I','w','phi_d','alpha_1','alpha_2','top_bottom_quality_ratio','y','phi_g','Q','theta', ...
        'sigma_a','w_g','gamma','gamma_tilde','rho','Delta','crit','maxit', 'Sigma','d_Sigma', ...
        'A_tilde','Sigma_pen','idx_no_data_ss','optimal_p','fc'};
params = struct();for i = 1:length(vars); name = vars{i}; params.(name) = eval(name); end
