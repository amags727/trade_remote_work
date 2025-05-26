function Omega_0 = fake_layp(D, Q)
n = size(D, 1); % Assuming D is square
% Construct the Kronecker product
K = kron(D, eye(n)) + kron(eye(n), D);

% Solve for the vectorized Omega
vec_Omega = -K \ reshape(Q, [], 1);

% Reshape the solution back to matrix form
Omega_0 = reshape(vec_Omega, n, n);

end 