function optim = ch2_optim_calc(dv,Sigma,xi, E_x, E_pi, alpha_1, alpha_2,...
    phi_d, sigma_a,Q, w,theta)

softplus = @(x) log1p(exp(-abs(x))) + max(x, 0);
L_input = -(1./w) .* xi .* dv .* Sigma.^2;
L_input(L_input<0) = 0; %L_input = softplus(L_input);
L = real(L_input.^(1/(1 - alpha_1)));

% Calculate R and drift
R = phi_d.*L.^alpha_1.*E_x.^alpha_2 + sigma_a.^(-2);

% Drift / pi / hamiltonian
drift = -Sigma.^2.*R - 2*theta*Sigma + Q;
pi_with_actions =  E_pi - w .* L;
ham = pi_with_actions + drift.*dv;

optim = struct( ...
    'L',L, ...
    'drift', drift, ...
    'pi_with_actions',  pi_with_actions, ...
    'ham', ham);
end