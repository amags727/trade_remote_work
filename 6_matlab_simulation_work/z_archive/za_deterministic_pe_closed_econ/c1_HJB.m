clear all; close all; 
addpath(genpath('c_helper_functions'))

%key parameters
I = 20;
w = 1; % data worker wage
phi_d = 1; % data productivity 
alpha_1 = .5;  % cobb douglas coefficient on data labor 
alpha_2 = .5; % cobb douglas coefficient on raw data 
top_bottom_quality_ratio = 2; % how much data helps improve quality 
x_scale_factor = 20; % what is underlying level of demand 
phi_g = 1;
Q = 1.1.^2; % variance of random component of z
theta = .9; % mean reversion parameter of z (closer to one faster mean reversion)

% Other Data parameters
sigma_a = 1.1; % sd of noise term 

% production parameters 
w_g = 1; % wage for production workers 
gamma = 4;     % CES parameter (from BEJK)
gamma_tilde = gamma/(gamma-1);
x_bar = x_scale_factor*phi_g^gamma; % base demand 
pi_bar = x_bar*w_g*phi_g^-1*(gamma-1)^-1; % base profits 

% Simulation Parameters
rho = 0.05; %discount rate
Delta = 1000; % 1/Delta = time_step
crit = 10^(-6);
maxit = 50;

% Define the state space 
Sigma_ub = Q /(2*theta); % Sigma_ub is the value such that drift = 0 when the firm doesn't participate in market 
Sigma_lb = 1e-2;
Sigma = linspace(Sigma_lb,Sigma_ub, I)';
d_Sigma = Sigma(2) - Sigma(1);

% Define Expected Quality (A_tilde) and related values 
A_tilde = ch1_gen_A_tilde(top_bottom_quality_ratio,Sigma, sigma_a);
E_x = x_bar*A_tilde;
E_pi = pi_bar*A_tilde;
xi = alpha_1*phi_d*E_x.^alpha_2;

% Make an initial guess of value function; get steady state profits
% of no data consumption forever 
R_0 = sigma_a^(-2);
Sigma_0 = (-theta + sqrt(theta^2 + Q*R_0))/R_0;
[~, idx_no_data_ss] = min(abs(Sigma - Sigma_0));
V_0 = repmat(E_pi(idx_no_data_ss) / rho, I,1);
v= V_0;

for n=1:maxit
    V=v;
    % Define forward/ backward difference  
    dv_f = [V(2:I)-V(1:I-1); 0] / d_Sigma;
    dv_f(I) = dv_f(I-1);
    dv_b = [0; V(2:I) - V(1:I-1)] / d_Sigma;
    dv_b(1) = dv_b(2);
    
   
    %carry out upwind procedure 
    %noting that Sigma_dot is decreasing in dv_Sigma
    dv_min = min(dv_b,dv_f); dv_max =  max(dv_b,dv_f);
    optim_min = ch2_optim_calc(dv_min,Sigma,xi, E_x, E_pi, alpha_1, alpha_2,...
    phi_d, sigma_a,Q, w, theta);
    optim_max = ch2_optim_calc(dv_max,Sigma,xi, E_x, E_pi, alpha_1, alpha_2,...
    phi_d, sigma_a,Q, w, theta);

    Ib = false(I,1); If = Ib; I_final = If;
    If(optim_min.drift > 0) = true;
    Ib(optim_max.drift <= 0) = true;
    I_final = Ib | If;
    dv_final = ...
        Ib.*dv_b + If.*dv_f +... 
        (~I_final & optim_min.ham > optim_max.ham).*dv_min + ...
        (~I_final & optim_min.ham <= optim_max.ham).*dv_max;
    optimal = ch2_optim_calc(dv_final,Sigma,xi, E_x, E_pi, alpha_1, alpha_2,...
    phi_d, sigma_a,Q, w, theta);
    
    %CONSTRUCT TRANSITION MATRIX 
    X = - min(optimal.drift,0)/d_Sigma;
    Y = - max(optimal.drift,0)/d_Sigma + min(optimal.drift,0)/d_Sigma;
    Z =   max(optimal.drift,0)/d_Sigma;
    
    A_matrix = sparse(I,I);
    for i = 1:I
        if i == 1
            A_matrix(i, i:(i+1)) = [X(i)+Y(i), Z(i)]; 
        elseif i == I
             A_matrix(i, (i-1):i) =  [X(i),Y(i)+ Z(i)];
        else 
            A_matrix(i, (i-1):(i+1)) = [X(i),Y(i), Z(i)];
        end
    end 
    
    if max(abs(sum(A_matrix,2)))>10^(-12)
        disp('Improper Transition Matrix')
        disp(n)
    end
    
    %SOLVE FOR NEW V
    B = (rho + 1/Delta)*speye(I) - A_matrix;
    b =  optimal.pi_with_actions + V/Delta;
    V = B\b;
    Vchange = V - v;
    v = V;
    
    dist(n) = max(max(abs(Vchange)));
    if dist(n)<crit
        %fprintf('Value Function Converged, Iteration = %g\n',n)
            break
    end
    if n== maxit
        disp('failed to converge')
    end
end

ss_indx =  find(optimal.drift < 0, 1);
L_at_SS = optimal.L(ss_indx);
alpha_determinant = -(1./w) .* xi .* dv_final .* Sigma.^2 /alpha_1;
output_ratio_ss_vs_no_data = round(100*(E_x(ss_indx) / E_x(idx_no_data_ss)- 1));
fprintf('percent output increase from no data: (%g)\n',output_ratio_ss_vs_no_data);
fprintf('S.S L value/position: (%g,%g)\n',round(L_at_SS,3), ss_indx);
%fprintf('S.S alpha determ fac. / dv: (%g,%g)\n',round(alpha_determinant(ss_indx),2), round(dv_final(ss_indx),2));


%% Simulate firm trajectory starting at Sigma_ub

T = 1000;                    % number of periods
Sigma_path = zeros(T, 1);
data_use_path = zeros(T, 1);
profit_path = zeros(T, 1);
output_path = zeros(T, 1);

Sigma_path(1) = Sigma_ub;  % initial condition

% Derivative of value function (finite difference)
for t = 1:T
    Sigma_t = Sigma_path(t);
    data_use_path(t) =  interp1(Sigma, optimal.L, Sigma_t, 'linear', 'extrap');
    output_path(t) = interp1(Sigma, E_x, Sigma_t, 'linear', 'extrap');
    profit_path(t) =  interp1(Sigma, E_pi, Sigma_t, 'linear', 'extrap');
    drift_t =  interp1(Sigma, optimal.drift, Sigma_t, 'linear', 'extrap');
    if (t ~= T)
        Sigma_path(t+1) = Sigma_t + drift_t*1/Delta;
    end 
end

%% Plot the results
%plot(1:T, data_use_path)
