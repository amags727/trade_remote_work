function output = fh5_find_value_func(num_firms,A_tilde_in, params, v_0)
% A_tilde_in = our guess of steady state quality 
% num_firms = our guess of the current number of firms in mkt

% unpack fixed parameters 
fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end
gammma =params.gamma;
% gen supplementary variables based on fixed params and num firms 
P = (A_tilde_in*num_firms)^(1/(1-gammma)).*optimal_p; 
x_bar = y*(gamma_tilde*w)^(-gammma)/ (P^(1-gammma));
pi_bar =  x_bar*w_g*phi_g^-1*(gammma-1)^-1;
E_x = x_bar*A_tilde;
E_pi = pi_bar*A_tilde -fc;
xi = alpha_1*phi_d*E_x.^alpha_2;
vars = {'Sigma','xi', 'E_x', 'E_pi', 'alpha_1', 'alpha_2', 'phi_d', 'sigma_a','Q', 'w', 'theta'};
upwind_params = struct();for i = 1:length(vars); name = vars{i}; upwind_params.(name) = eval(name); end

% carry out value func iteration 
if ~exist('v_0', 'var'); v_0 = repmat(E_pi(idx_no_data_ss)/rho, I,1);end
v= v_0;
for n=1:maxit
    % Define forward/ backward difference  
    dv_f = [v(2:I)-v(1:I-1); 0] / d_Sigma;
    dv_f(I) = dv_f(I-1);
    dv_b = [0; v(2:I) - v(1:I-1)] / d_Sigma;
    dv_b(1) = dv_b(2);
    
   
    %carry out upwind procedure 
    %noting that Sigma_dot is decreasing in dv_Sigma
    dv_min = min(dv_b,dv_f); dv_max =  max(dv_b,dv_f);
    optim_min = fh3_optim_calc(dv_min,upwind_params);
    optim_max = fh3_optim_calc(dv_max,upwind_params);

    Ib = false(I,1); If = Ib; 
    If(optim_min.drift > 0) = true;
    Ib(optim_max.drift <= 0) = true;
    I_final = Ib | If;
    dv_final = ...
        Ib.*dv_b + If.*dv_f +... 
        (~I_final & optim_min.ham > optim_max.ham).*dv_min + ...
        (~I_final & optim_min.ham <= optim_max.ham).*dv_max;
    optimal = fh3_optim_calc(dv_final,upwind_params);
    
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
    b =  optimal.pi_with_actions + v/Delta;
    V = B\b;
    Vchange = V - v;
    v = V;
    
    dist(n) = max(abs(Vchange));
    % fprintf('Distance =  %g\n',dist(n));
    if dist(n)<crit
        converged = true;
       % fprintf('Value Function Converged, Iteration = %g\n',n)
            break
    end
    if n== maxit
        converged = false;
        %disp('failed to converge')
    end
end

%% === output results ====
abs_entrance_v = abs(v(I));

drift = optimal.drift;
i = find(drift > 0, 1, 'last');
w = drift(i) / (drift(i) - drift(i+1));
A_tilde_out = (1 - w) * A_tilde(i) + w * A_tilde(i+1);
miss_value = sum([abs_entrance_v, (A_tilde_out - A_tilde_in)*25].^2);

vars = {'v', 'optimal', 'A_tilde_out','miss_value','converged'};
output = struct();for i = 1:length(vars); name = vars{i}; output.(name) = eval(name); end
end
