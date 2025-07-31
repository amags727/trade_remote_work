function optim = fh3_optim_calc(dv,upwind_params)
fields = fieldnames(upwind_params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = upwind_params.' fields{idx} ';']); end


L_input = -(1./w) .* xi .* dv .* Sigma.^2;
L_input(L_input<0) = 0;
L = real(L_input.^(1/(1 - alpha_1)));

% Calculate R and drift
R = Sigma_pen.*phi_d.*L.^alpha_1.*E_x.^alpha_2 + sigma_a.^(-2);

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