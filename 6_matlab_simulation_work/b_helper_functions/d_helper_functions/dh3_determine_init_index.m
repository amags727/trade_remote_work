function init_index = dh3_determine_init_index(Sigma_mat,Sigma,D,Q,sigma_a,num_mkts)
len_Sigma = size(Sigma_mat, 3);
R = [sigma_a(1)^(-2), 0; 0, 0];
Sigma_ss_guess = squeeze(Sigma_mat(:,:, len_Sigma, 1));
f = @(S) reshape(D*S + S*D' + Q - S*R*S, [], 1);
options = optimoptions('fsolve', 'Display', 'off', 'FunctionTolerance', 1e-8);
Sigma_ss = fsolve(f, Sigma_ss_guess, options);
Sigma_ss = dh2_restructure_array(Sigma_ss, false, num_mkts)';
[~, init_index] = min(sum((Sigma - Sigma_ss).^2, 2));  
end

