clear all; close all; clc;

% production parameters 
w = 1; % wage
phi_g = 1; % goods productivity
gamma = 4;     % CES parameter (from BEJK)
gamma_tilde = gamma/(gamma-1);
x_bar = 5; % base demand 
pi_bar = x_bar*w*phi_g^-1*(gamma-1)^-1; % base profits 

% data parameters 
phi_d = 1; % data productivity 
alpha_1 = .5; % cobb douglas coefficient on data labor 
alpha_2 = .5; % cobb douglas coefficient on raw data 
sigma_a = 1.1; % sd of noise term 
sqrt_Q = 1.1; % sd of random component of z
theta = .9; % mean reversion parameter of z (closer to one faster mean reversion)

% Simulation Parameters
rho = 0.05; %discount rate
Delta = 1000; % 1/Delta = time_step
crit = 10^(-6);
maxit = 500;

% Define the state space 
I = 20;
Sigma_ub = sqrt_Q^2 /(2*theta); % Sigma_ub is the value such that drift = 0 when the firm doesn't participate in market 
Sigma_lb = 1e-2;
Sigma = linspace(Sigma_lb,Sigma_ub, I)';
d_Sigma = Sigma(2) - Sigma(1);

% Define Expected Quality (A_tilde) and related values 
Sigma_importance = .66; 
A_bar = Sigma_ub / Sigma_importance +  sigma_a^2 + .5*Sigma_ub;
A_tilde = A_bar - sigma_a^2 - Sigma;

xi_bar = -Sigma.^2*phi_d*alpha_1.*(A_tilde*x_bar).^alpha_2; %component of xi not a function of v'
c_drift = -2*theta.*Sigma + sqrt_Q.^2; % component of drift not a function of L 

% L needed to make drift equal zero; note for firms with very low precision
% they will always increase in certainty regardless of choice of L
L_zero_drift = ((sqrt_Q^2-2*theta*Sigma) ./ Sigma.^2 - sigma_a^-2) ./ (phi_d*(A_tilde*x_bar).^alpha_2);
L_zero_drift(L_zero_drift >= 0) = L_zero_drift(L_zero_drift >= 0).^(1/alpha_1);
L_zero_drift(L_zero_drift < 0 ) = NaN;
dV_zero_drift = -w ./ (Sigma.^2*phi_d*alpha_1.*L_zero_drift.^(alpha_1-1).*(A_tilde*x_bar).^alpha_2);
L_zero_drift(isnan(L_zero_drift)) = 0;
% Make an initial guess of value function (we get base profits forever
% without expending any data resources) 
v_0 = (A_tilde*pi_bar)/rho;
v = v_0;
for n=1:maxit
    disp(n)

    V=v;
    % Define forward/ backward difference  
    dV_f = [V(2:I)-V(1:I-1); 0] / d_Sigma;
    dV_f(I) = dV_f(I-1);
    dV_b = [0; V(2:I) - V(1:I-1)] / d_Sigma;
    dV_b(1) = dV_b(2);
    
    % construct optimal L 
    % (we constrain it to be positive so if dV <0 we set it to zero)
    L_f = (w./(xi_bar.*dV_f)).^(1/(alpha_1 -1)).*(dV_f<0);
    L_b = (w./(xi_bar.*dV_b)).^(1/(alpha_1 -1)).*(dV_b<0);
    
    drift_b = c_drift - Sigma.^2.*(phi_d * L_b.^alpha_1 .* (A_tilde*x_bar).^alpha_2 + sigma_a^-2);
    drift_f = c_drift - Sigma.^2.*(phi_d * L_f.^alpha_1 .* (A_tilde*x_bar).^alpha_2 + sigma_a^-2);
    
    
    %CARRY OUT THE UPWIND 
    % NB our upwind procedure flips the procedure from the example code,
    % this is because our value function is generally convex, not concave
    % (a result of higher values of the state being worse, instead of
    % better) 
    Ham_f = A_tilde*pi_bar - w*L_f + drift_f .* dV_f;
    Ham_b = A_tilde*pi_bar - w*L_b + drift_b .* dV_b;
    
     Iboth = (drift_f<0).*(drift_b>0); % problematic case 
     I_zero_drift = (drift_b < 0) .*(drift_f>0); % steady state 
     Iunique = (drift_f<0).*(1-(drift_b>0)) + (1-(drift_f<0)).*(drift_b>0); % 
     Ib = Iunique.*(drift_f<0) | Iboth.*(Ham_b>=Ham_f);
     If = Iunique.*(drift_b>0) | Iboth.*(Ham_f>Ham_b);
    
    %make corrections at edge of domain 
    Ib(I) = 1; If(I) = 0;
    Ib(1) = 0; If(1) = 1;
    
    dV_Upwind = dV_f.*If + dV_b.*Ib + dV_zero_drift.*I_zero_drift; %important to include third term
    L = L_f.*If + L_b.*Ib + L_zero_drift.*I_zero_drift;
    pi = A_tilde*pi_bar - w*L ;
    
    %CONSTRUCT TRANSITION MATRIX 
    X = - min(drift_b,0)/d_Sigma;
    Y = - max(drift_f,0)/d_Sigma + min(drift_b,0)/d_Sigma;
    Z =   max(drift_f,0)/d_Sigma;
    
    
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
    b =  pi + V/Delta;
    V = B\b;
    Vchange = V - v;
    v = V;
    
    dist(n) = max(max(abs(Vchange)));
    if dist(n)<crit
            disp('Value Function Converged, Iteration = ')
            disp(n)
            break
    end
end
