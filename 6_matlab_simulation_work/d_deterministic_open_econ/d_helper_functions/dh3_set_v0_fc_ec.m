function [v0,fc,ec] = dh3_set_v0_fc_ec(Sigma_mat,Sigma, D,Q,sigma_a,...
    pi_bar, A_tilde,networks, fixed_cost_scaling,ec_multiplier, rho)

[~,num_mkts,len_Sigma, num_networks] = size(Sigma_mat);
v0 = zeros(len_Sigma, size(networks,1));
for i = 0:num_networks
    if i == 0
        R = diag([sigma_a(1)^(-2), 0]);
    else
        R = diag(sigma_a.^(-2).*networks(i,:));
    end
    Sigma_ss_guess = squeeze(Sigma_mat(:,:, len_Sigma, 1));
    f = @(S) reshape(D*S + S*D' + Q - S*R*S, [], 1);
    options = optimoptions('fsolve', 'Display', 'off', 'FunctionTolerance', 1e-8);
    Sigma_ss = reshape(fsolve(f, Sigma_ss_guess, options),[],num_mkts^2) ;
    [~, init_index] = min(sum((Sigma - Sigma_ss([1,2,4])).^2, 2));

    if i == 0
        fc = pi_bar(1)*A_tilde(init_index,1);
        fc = [fc, repmat(fc*fixed_cost_scaling,1,num_mkts-1)];
        ec = fc*ec_multiplier;
    else
        v0(:,i) = sum((pi_bar.*A_tilde(init_index,:) - fc).*networks(i,:))/ rho;
    end
end
end