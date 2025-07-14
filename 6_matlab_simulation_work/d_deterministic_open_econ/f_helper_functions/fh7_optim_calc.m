function optim = fh7_optim_calc(dv,state_space, Sigma_mat, Q, D, w, phi_d, alpha_1, alpha_2, sigma_a, ...
    networks, E_x, E_pi, xi, fc)
% ==== setup  ===== 
[num_networks, num_mkts] = size(networks);
len_Sigma = size(dv, 1);
%=======
% transform dv state space
% from (Sigma_11, Sigma_22, lambda) --> (Sigma_11, Sigma_12, Sigma_22)
%=======
% move Sigma_11 / Sigma_22 over 
dv_fake = zeros(size(dv)); dv_fake(:,[1,3], : ) = dv(:,1:2, :);

% dv_Sigma12 = dv_lambda * (Sigma11*Sigma22)^-(1/2)
dv_fake(:, 2, :)= dv(:,3,:).*(state_space(:,1).* state_space(:,2)).^(-.5);
dv_mat_fake = fh2_restructure_array(dv_fake , true, num_mkts);

% ==== generate L  ===== 
% Compute Sigma * dv * Sigma' and output diagonals in len_sigma x num_mkts x num_networks shape 
Sigma_dv_Sigma_mat = pagemtimes(pagemtimes(Sigma_mat, dv_mat_fake), Sigma_mat);
Id_matrix = repmat(reshape(eye(num_mkts), [num_mkts, num_mkts, 1, 1]), 1, 1, len_Sigma, num_networks);
diag_vals = sum(Sigma_dv_Sigma_mat .* Id_matrix, 2);  
diag_vals = reshape(diag_vals, num_mkts, len_Sigma, num_networks);  
Sigma_dv_Sigma = permute(diag_vals, [2, 1, 3]);  

% generate our choice of L 
L_input = -(1/w)*xi .* Sigma_dv_Sigma;
L_input(L_input<0) = 0; 
L = real(L_input.^(1/(1-alpha_1)));

% === generate drift ====
% generate R and D matrices 
R_vec = phi_d*L.^alpha_1.*E_x.^alpha_2 + (sigma_a.^-2) .* permute(networks, [3, 2, 1]);
R_mat = zeros(num_mkts, num_mkts, len_Sigma, num_networks);
for i = 1:num_mkts
    R_mat(i, i, :, :) = squeeze(R_vec(:, i, :)); 
end
D_mat = repmat(D,1,1,len_Sigma, num_networks);

% calculate drift of Sigma variables 
Sigma_drift_mat = pagemtimes(D_mat,Sigma_mat) + pagemtimes(Sigma_mat, D_mat)...
    + Q -pagemtimes(pagemtimes(Sigma_mat, R_mat),Sigma_mat);
Sigma_drift = fh2_restructure_array(Sigma_drift_mat,false, num_mkts);

% transform Sigma drift into state drift 
state_drift = zeros(size(Sigma_drift));
state_drift(:,[1,2], : ) = Sigma_drift(:,[1,3], :);

% gen lambda_dot 
    % Sigma12_dot / sqrt(Sigma11*Sigma22)
    lambda_pt1 = squeeze(Sigma_drift(:,2,:).*(state_space(:,1).* state_space(:,2)).^(-.5));
    
    % -.5lambda*(Sigma11_dot / Sigma11 + Sigma22_dot/ Sigma_22)
    lambda_pt2 = -.5*state_space(:,3,:).*(squeeze(sum(state_drift(:,1:2,:)./state_space(:,1:2,:),2)));

    % combine and output 
    input = lambda_pt1 + lambda_pt2;
    state_drift(:,3,:) = input;



%=== gen profit and hamiltonian ===
profit_w_actions = squeeze(sum(E_pi - w*L - fc.* permute(networks, [3, 2, 1]),2));
ham = profit_w_actions + squeeze(sum(state_drift.*dv,2));

%=== output results 
optim = struct('L', L, 'drift', state_drift, 'profit_w_actions', profit_w_actions, 'ham', ham);
end
