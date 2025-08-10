function [params] = dh0_set_invariant_params()

% base variables 
I = 10;
num_mkts = 2;
num_networks = 2;
networks = [1,0;1,1];

% production parameters 
phi_g = 1;
foreign_cost_scaling = 1.02; %fc * fixed_cost_scaling = foreign market fixed costs 
fc_base = 1.5;
fc = fc_base *[1,foreign_cost_scaling];
ec_multiplier =2;
ec = ec_multiplier*fc; % entry costs to market 
rev_ec = repmat(5,1, num_mkts); % exit costs from market 
w_g = 1; % wage for production workers
gamma = 4;     % CES parameter (from BEJK)
gamma_tilde = gamma/(gamma-1);
initial_demand_assumption = 20;
tau = 1.005; % iceberg trade costs 

%data parameters 
w = 1; % data worker wage
phi_d = 1; % data productivity 
alpha_1 = .5;  % cobb douglas coefficient on data labor 
alpha_2 = .5; % cobb douglas coefficient on raw data 
top_bottom_quality_ratio = 2.5; % how much data helps improve quality 
sigma_z = repmat(1.1,1,num_mkts); % variance of random component of z
theta = .9; % mean reversion parameter of z (closer to one faster mean reversion)
lambda_tilde = .5; % correlation coefficient between mkts 
sigma_a = repmat(1.1,1,num_mkts); % sd of noise term

% Simulation Parameters
rho = 0.05; %discount rate
Delta = 1000; % 1/Delta = time_step
crit = 1e-4; % acceptable value for HJB / LCP convergence  
maxit = 1e2;

% Gen state space 
[Q,D,Sigma] = make_state_space(num_mkts,I, lambda_tilde, sigma_z, theta);
Sigma_mat = dh2_restructure_array(repmat(Sigma,1,1,num_networks),true, num_mkts);
num_state_vars = size(Sigma,2);
len_Sigma = size(Sigma,1);
d_Sigma =  (Sigma(len_Sigma, :) - Sigma(1,:))/(I-1);
diag_indeces = find(ismember(find(triu(true(num_mkts), 0)),1:num_mkts+1:num_mkts^2));
adjacency_matrix = make_adjacency_matrix(len_Sigma,num_state_vars,I) ;
g1 = unique(Sigma(:,1)); g2 = unique(Sigma(:,2)); g3 = unique(Sigma(:,3));

% Gen A_tilde 
A_tilde  = gen_A_tilde(top_bottom_quality_ratio,Sigma, sigma_a, diag_indeces);

% Gen Sigma Penalty 
Sigma_pen_ratio = 1;
Sigma_pen_curve = 1;
ub = Sigma(len_Sigma, [1,3]); lb = Sigma(1, [1,3]);
Sigma_pen_scalar = (Sigma_pen_ratio - 1) *(ub.^Sigma_pen_curve - lb.^Sigma_pen_curve).^(-1);
Sigma_pen = (1+Sigma_pen_scalar.* Sigma(:,[1,3]).^Sigma_pen_curve).^(-1);


var_names = {'I', 'num_mkts', 'num_networks', 'phi_g', 'foreign_cost_scaling', ...
    'w_g', 'gamma', 'gamma_tilde', 'w', 'phi_d', 'alpha_1', 'alpha_2', ...
    'top_bottom_quality_ratio', 'sigma_z', 'theta', 'lambda_tilde', 'rho', ...
    'Delta', 'crit', 'maxit', 'Q', 'D', 'Sigma', 'Sigma_mat', ...
    'num_state_vars', 'len_Sigma', 'd_Sigma', 'diag_indeces', 'A_tilde', ...
    'adjacency_matrix', 'sigma_a','tau','fc', 'ec', 'rev_ec', 'Sigma_pen', 'networks', 'g1', 'g2', 'g3'};
params = struct();for i = 1:length(var_names); name = var_names{i}; params.(name) = eval(name); end
end

function [adjacency_matrix] = make_adjacency_matrix(len_Sigma,num_state_vars,I) 
index_array = reshape(1:len_Sigma, repmat(I, 1, num_state_vars));
index_array_b = zeros(len_Sigma, num_state_vars);
index_array_f= zeros(len_Sigma, num_state_vars);

    for state_num = 1:num_state_vars
        % Create indexing templates
        idx_all = repmat({':'}, 1, num_state_vars);

        % Forward difference (except last index)
        idx_f = idx_all;        idx_f{state_num} = 1:I-1;
        idx_next = idx_all;     idx_next{state_num} = 2:I;
        temp = zeros(size(index_array));
        temp(idx_f{:}) = index_array(idx_next{:});
        index_array_f(:, state_num) = reshape(temp, len_Sigma, 1);

        % Backward difference (except first index)
        idx_b = idx_all;        idx_b{state_num} = 2:I;
        idx_prev = idx_all;     idx_prev{state_num} = 1:I-1;
        temp2 = zeros(size(index_array));
        temp2(idx_b{:}) = index_array(idx_prev{:});

        index_array_b(:,state_num) = reshape(temp2, len_Sigma, 1); 
    end
    adjacency_matrix = [index_array_b, index_array_f];
end

function [Q,D,Sigma] = make_state_space(num_mkts,I, lambda_tilde, sigma_z, theta)

%% Construct D and Q
Q = zeros(num_mkts,num_mkts); 
for index = 1:num_mkts^2
    [i, j] = ind2sub([num_mkts,num_mkts], index);
    if i == j
        Q(index) = sigma_z(i)^2;
    else
        Q(index) =  sigma_z(i)*sigma_z(j)*lambda_tilde;
    end
end

D =  diag(repmat(-theta,1,num_mkts)); % mean reversion parameter term


%% Construct standard version of Sigma 
Sigma_ub = fake_layp(D, Q); % sets drift of Sigma to zero when we're in no markets
Sigma_lb = 1e-1;
state_space_ub =  Sigma_ub(triu(true(size(Sigma_ub)), 0));
vecs = arrayfun(@(x) linspace(Sigma_lb, x, I), state_space_ub, 'UniformOutput', false);
[nvecs{1:numel(vecs)}] = ndgrid(vecs{:});
Sigma = cell2mat(cellfun(@(x) x(:), nvecs, 'UniformOutput', false));
end

function A_tilde = gen_A_tilde(top_bottom_quality_ratio,Sigma, sigma_a, diag_indeces)

% Compute penalty as usual
penalty = sigma_a.^2 + Sigma(:,diag_indeces);

% Normalize penalty to [0,1]
penalty_norm = (penalty - min(penalty)) ./ (max(penalty) - min(penalty));

% Construct A_tilde with desired ratio: A_tilde = a - b * penalty_norm
% so that: max(A_tilde) / min(A_tilde) = penalty_ratio
% Let min(A_tilde) = x, then max(A_tilde) = x * penalty_ratio
% So: A_tilde =  penalty_ratio - (penalty_ratio - 1) * penalty_norm
A_tilde = top_bottom_quality_ratio - (top_bottom_quality_ratio - 1) * penalty_norm;

end
