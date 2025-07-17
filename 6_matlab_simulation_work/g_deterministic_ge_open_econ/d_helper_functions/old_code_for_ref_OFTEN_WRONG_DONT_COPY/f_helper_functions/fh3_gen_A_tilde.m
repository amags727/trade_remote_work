function A_tilde = fh3_gen_A_tilde(top_bottom_quality_ratio,state_space, sigma_a, num_mkts)

% Compute penalty as usual
penalty = sigma_a.^2 + state_space(:,1:num_mkts);

% Normalize penalty to [0,1]
penalty_norm = (penalty - min(penalty)) ./ (max(penalty) - min(penalty));

% Construct A_tilde with desired ratio: A_tilde = a - b * penalty_norm
% so that: max(A_tilde) / min(A_tilde) = penalty_ratio
% Let min(A_tilde) = x, then max(A_tilde) = x * penalty_ratio
% So: A_tilde =  penalty_ratio - (penalty_ratio - 1) * penalty_norm
A_tilde = top_bottom_quality_ratio - (top_bottom_quality_ratio - 1) * penalty_norm;


