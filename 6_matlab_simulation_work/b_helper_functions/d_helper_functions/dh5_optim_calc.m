function optim = dh5_optim_calc(dv, Sigma_mat, Q, D, w, phi_d, alpha_1, alpha_2, sigma_a, ...
    networks, E_x, E_pi, xi, fc)
% ==== setup  ===== 
[num_networks, num_mkts] = size(networks);
len_Sigma = size(dv, 1);
% ==== generate L  ===== 

% Compute Sigma * dv * Sigma' and output diagonals in len_sigma x num_mkts x num_networks shape 
dv_mat = dh2_restructure_array(dv, true, num_mkts);
Sigma_dv_Sigma_mat = pagemtimes(pagemtimes(Sigma_mat, dv_mat), Sigma_mat);
Id_matrix = repmat(reshape(eye(num_mkts), [num_mkts, num_mkts, 1, 1]), 1, 1, len_Sigma, num_networks);
diag_vals = sum(Sigma_dv_Sigma_mat .* Id_matrix, 2);  % Now size is [num_mkts x 1 x len_Sigma x num_networks]
diag_vals = reshape(diag_vals, num_mkts, len_Sigma, num_networks);  % Get rid of singleton dimension 2
Sigma_dv_Sigma = permute(diag_vals, [2, 1, 3]);  % [len_Sigma x num_mkts x num_networks]

% generate our choice of L 
softplus = @(x) log1p(exp(-abs(x))) + max(x, 0);
L_input = -(1/w)*xi .* Sigma_dv_Sigma;
L_input(L_input<0) = 0; % L_input = softplus(L_input)
L = real(L_input.^(1/(1-alpha_1)));
%L = real(softplus(L_input).^(1/(1-alpha_1)));
clear Id_matrix diag_vals Sigma_dv_Sigma_mat L_input


% ==== generate the drift ====
% generate R and D matrices 
R_vec = phi_d*L.^alpha_1.*E_x.^alpha_2 + (sigma_a.^-2) .* permute(networks, [3, 2, 1]);
R_mat = zeros(num_mkts, num_mkts, len_Sigma, num_networks);
for i = 1:num_mkts
    R_mat(i, i, :, :) = squeeze(R_vec(:, i, :)); 
end
D_mat = repmat(D,1,1,len_Sigma, num_networks);

% calculate drift 
drift_mat = pagemtimes(D_mat,Sigma_mat) + pagemtimes(Sigma_mat, D_mat)...
    + Q -pagemtimes(pagemtimes(Sigma_mat, R_mat),Sigma_mat);
drift = dh2_restructure_array(drift_mat,false, num_mkts);

% ==== generate profits and hamiltonian ====
profit_w_actions = squeeze(sum(E_pi - w*L - fc.* permute(networks, [3, 2, 1]),2));
ham = profit_w_actions  + squeeze(sum(dv.*drift,2));
optim = struct('L', L, 'drift', drift, 'profit_w_actions', profit_w_actions, 'ham', ham);
end
