function optim = dh5_optim_calc(dv, params)
fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end

% ==== setup  ===== 
[num_networks, num_mkts] = size(networks);
len_Sigma = size(dv, 1);

% prepare dv_mat (zero out lower triangle bc those aren't actually state vars)
dv_mat = dh2_restructure_array(dv, true, num_mkts);
zero_out_lower_tri = tril(true(num_mkts), -1); 
[i_idx, j_idx] = find(zero_out_lower_tri);
for k = 1:length(i_idx)
    i = i_idx(k); j = j_idx(k); dv_mat(i, j, :, :) = 0;
end
% ==== generate L  ===== 
 % if ~all(dv(:)==0)
 %      disp('hi')
 %     t_dv = squeeze(dv_mat(:,:, 500, 2));
 %     t_Sigma = squeeze(Sigma_mat(:,:, 500, 2));
 %     t_Sigma_dv_Sigma_1a = t_dv(1,1)*t_Sigma(1,1)^2 ...
 %                           + t_dv(1,2)*t_Sigma(1,1)*t_Sigma(1,2) ...
 %                           + t_dv(2,2)*t_Sigma(2,1)^2;
 %     t_Sigma_dv_Sigma_1b = t_Sigma'*t_dv*t_Sigma;  t_Sigma_dv_Sigma_1b  =  t_Sigma_dv_Sigma_1b(1,1);
 %     t_L_input_1 = -1/w * xi(500,1,2) * t_Sigma_dv_Sigma_1b ;
 % end

% Compute Sigma * dv * Sigma' and output diagonals in len_sigma x num_mkts x num_networks shape 
Sigma_dv_Sigma_mat = pagemtimes(pagemtimes(Sigma_mat, dv_mat), Sigma_mat);
Id_matrix = repmat(reshape(eye(num_mkts), [num_mkts, num_mkts, 1, 1]), 1, 1, len_Sigma, num_networks);
diag_vals = sum(Sigma_dv_Sigma_mat .* Id_matrix, 2);  % Now size is [num_mkts x 1 x len_Sigma x num_networks]
diag_vals = reshape(diag_vals, num_mkts, len_Sigma, num_networks);  % Get rid of singleton dimension 2
Sigma_dv_Sigma = permute(diag_vals, [2, 1, 3]);  % [len_Sigma x num_mkts x num_networks]

% generate our choice of L 
softplus = @(x) log1p(exp(-abs(x))) + max(x, 0);
L_input = -(1/w)*xi .* Sigma_dv_Sigma;

%when one or more of the Ls is constrained check manually
% L_manual = zeros(size(L_input));
% for network = 1:num_networks
%     problem_rows = find(sum(L_input(:,:,network) <0,2) ==1);
%     num_prows = size(problem_rows,1);
%     if num_prows >0
%         network = 2;
%         problem_rows = [500,500];
%         num_prows = size(problem_rows,1);
%         L_guess = L_input(problem_rows, :,network);
%         L_guess(L_guess <0) = 0;
%         L_guess = real(L_guess.^(1/(1-alpha_1)));
%         [L_problem_rows,~,~] = dh51_L_numeric_fallback(L_guess, phi_d, alpha_1,alpha_2, ...
%             E_x, networks, sigma_a, Sigma_mat, D, Q, dv, E_pi, w, fc, ...
%             problem_rows, network, num_mkts, num_prows);
% 
% 
%         t_dv = squeeze(dv_mat(:,:, 500, 2));
%         t_Sigma = squeeze(Sigma_mat(:,:, 500, 2));
%         L = L_problem_rows(1,:); L = L_problem_rows(1,:);
%         -w - xi(500,:,2).*L.^(alpha_1-1).* diag(t_Sigma'*t_dv*t_Sigma)'
% 
% 
% 
%         % ==== Construct Hamiltonian to maximize ===== 
%         for version = 1:2
%             if version == 1
%                 L = L_problem_rows;
%             else 
%                 L = L_guess;
%             end
%         % build R
%         R_vec = phi_d*L.^alpha_1.*E_x(problem_rows,:,network).^alpha_2+ networks(network,:).* (sigma_a.^-2);
%         R_mat = zeros(num_mkts, num_mkts, size(R_vec,1));
%         for i = 1:num_mkts
%             R_mat(i, i, :) = squeeze(R_vec(:, i));
%         end
% 
%         % generate drift
%         p_Sigma = Sigma_mat(:,:,problem_rows,network);
%         p_D = repmat(D,1,1,num_prows); p_Q = repmat(Q,1,1,num_prows);
%         drift = pagemtimes(p_D, p_Sigma) + pagemtimes(p_Sigma, p_D) + p_Q...
%             - pagemtimes(pagemtimes(p_Sigma, R_mat), p_Sigma);
%         drift_vec = dh2_restructure_array(drift,false,num_mkts);
% 
%         % calc hamiltonian
%         profit = sum(E_pi(num_prows,:,network) - w*L - fc.* networks(network,:),2);
%         ham = profit + sum(dv(problem_rows,:, network).*drift_vec,2);
%         disp(sum(ham))
%         end
% 
%     end
% end

L_input(L_input<0) = 0; % L_input = softplus(L_input)
L = real(L_input.^(1/(1-alpha_1)));
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
ham = profit_w_actions  + squeeze(sum(drift_mat.*dv_mat, [1,2]));
optim = struct('L', L, 'drift', drift, 'profit_w_actions', profit_w_actions, 'ham', ham);
end
