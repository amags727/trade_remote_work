function optim = e1_optim_calc(dv, dv_2, w, z_act, z_hat, ...
    Sigma, xi, alpha_1, alpha_2, nu, phi_d, sigma_a, theta, Q, E_x, E_pi)

    %% Softplus smoothing
    softplus = @(x) log1p(exp(-abs(x))) + max(x, 0);

    %% Optimal labor L
    marginal_val = dv(:,2).*(z_act - z_hat) - dv(:,4).*Sigma + 0.5 .* dv_2(:,2) .* Sigma;
    L_input = 1./w .* Sigma .* xi .*  marginal_val;
    L_input(L_input<0) = 0; %L_input = softplus(L_input);
    L = real(L_input.^(1/(1 - alpha_1)));

    %% Optimal μ
    mu = dv(:,3) / (2 * nu);

    %% Store choices
    choice = struct('L', L, 'mu', mu);

    %% Compute R, K
    R = phi_d .* L.^alpha_1 .* E_x.^alpha_2 + sigma_a.^(-2);
    K = Sigma .* R;

    %% Drift terms
    drift = struct();
    drift.z_act = -theta .* z_act;
    drift.z_hat = (-theta - K) .* z_hat + K .* z_act;
    drift.a = mu;
    drift.Sigma = -Sigma.^2 .* R - 2 * theta .* Sigma + Q;
    drift.overall = [drift.z_act, drift.z_hat, drift.a, drift.Sigma];

    %% Diffusion terms
    diffusion = struct();
    diffusion.zz = Q.^(1/2);
    diffusion.zhatzhat = Sigma .* (R.^(1/2));

    % simplified to just be over four diagonal entries since everything else is zero
    diffusion.overall_sq = [repmat(Q, size(drift.a)), diffusion.zhatzhat.^2, zeros(size(drift.a,1),2)];

    %% Hamiltonian
    pi_with_actions =  E_pi - w .* L - nu .* mu.^2;
    ham = pi_with_actions + sum(dv .* drift.overall,2) + ...
          0.5 * (dv_2(:,1) .* diffusion.zz.^2 + dv_2(:,2) .* diffusion.zhatzhat.^2);

    %% Output
    optim = struct();
    optim.choice = choice;
    optim.drift = drift;
    optim.diffusion = diffusion;
    optim.pi_with_actions =  pi_with_actions;
    optim.ham = ham;
    optim.ham_rep = repmat(ham, 1, size(dv,2));
end