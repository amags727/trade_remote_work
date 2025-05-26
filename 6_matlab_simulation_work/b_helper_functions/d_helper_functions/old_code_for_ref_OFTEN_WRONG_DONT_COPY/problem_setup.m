
function[exog, starting_point] = problem_setup(N, phi_options, I, num_divisions, P,adjustment, equilib_setting)
exog = struct();

%% establish adjustment parameters 
shock_sds = ones(1,N);     % sd of market shocks 
var_rho = repmat(.95,1,N); % the persistence of shocks in each market 

omega_e = 2;
similiarity_level = .25*ones(N); % 1/ corellation coeff. between shocks 
tau_ij = .231;  % melitz & burstein (2008) .231
fc_multiplier =  .5; %(1+fc_multiplier) higher foreign fixed costs  (Melitz Burstein 2008) 2.85
co = [.1,.1];       % cost parameter outside data (baseline = .1)
ci = .02;       % cost parameter in-house data
entry_multiplier = 10;

eval(adjustment);
%% Establish uncertainty parameters / state space
exog.N = N; % number of countries
exog.num_choice_vars = 2*N+1;
exog.dt = 1/1000;    % timestep
exog.I = I;

% uncertainty  evolution
exog.omega_e = omega_e;
omega_theta = zeros(N,N);
parfor index = 1:N^2
    [i, j] = ind2sub([N,N], index);
    if i == j
       omega_theta(index) =  shock_sds(i)^2;
    else
       omega_theta(index) =  shock_sds(i)*shock_sds(j)*similiarity_level(i,j);
    end
end
exog.omega_theta = omega_theta;
exog.D = diag(log(var_rho));  % .95 persistence parameter

%State Space 
Omega_UB = fake_layp(exog.D, exog.omega_theta);


% omega_a set st we can always ensure positive drift
omega_a = 0;
for i = 1:N
    for j = 1:N
        sum = 0;
        for k = 1:N
            if k~= i && k~=j
                sum = sum + Omega_UB(i,k)*Omega_UB(k,j);
            end
        end
         sum = sqrt(sum / exog.omega_theta(i,j));
         if sum > omega_a
             omega_a = sum;
         end
    end
end
omega_a = 1.1*omega_a;
if N == 1
    omega_a = 1/1.1;
end
exog.omega_a = 1.1*omega_a;
%need a constant omega_a for variation analysis: this is big enough that it's always good
omega_a = 5; exog.omega_a = omega_a;

state_keys =  unique(sort(combvec(1:N,1:N)',2),'rows');
num_state_vars = size(state_keys,1);
exog.diag_state_vars = state_keys(:,1) == state_keys(:,2);


for i = 1:num_state_vars
    state_UBS(i) = Omega_UB(state_keys(i,1),state_keys(i,2));
    state_inputs{i} =  linspace(0,state_UBS(i),I);
    step_size(i) = state_inputs{i}(2) - state_inputs{i}(1);
end

state_space = combvec(state_inputs{:})'; 
num_states = size(state_space,1);
exog.state_keys = state_keys; exog.num_state_vars = num_state_vars;

state_plus_1 = zeros(num_states,num_state_vars); state_minus_1 = zeros(num_states,num_state_vars);
for state_var = 1:num_state_vars
    state_plus_1(:,state_var) = ((1:num_states) + (I)^(state_var-1))' .* (state_space(:,state_var) ~= state_UBS(state_var));
    state_minus_1(:,state_var) = ((1:num_states) - (I)^(state_var-1))' .* (state_space(:,state_var) ~= 0);
end
exog.state_space = state_space; exog.num_states = num_states; exog.step_size = step_size;
exog.state_plus_1 = state_plus_1; exog.state_minus_1 = state_minus_1; exog.num_states = num_states;



%% establish production parameters
exog.Y = 10;        % demand level 
exog.w = 1.5;       % wage cost
exog.rho = .05;     % discount rate
exog.beta = .005;   % exogenous death rate
exog.kappa = .005;  % drift penalty
exog.sigma = 4;     % CES parameter (from BEJK)
exog.sigma_tilde = exog.sigma/(exog.sigma-1);

% A = A_bar - omega_a^2 - Omega;
% rescaled so that omega_UB gives quality of 1; 
A_bar = max((repmat(exog.omega_a^2,N,1) + diag(Omega_UB))) +1; exog.A_bar = A_bar;

% this A_bar will always work for all variation levels 
%A_bar = 19.6563; exog.A_bar = A_bar;

exog.A = repmat(A_bar- exog.omega_a^2, [exog.num_states, N])...
    - exog.state_space(:,exog.diag_state_vars); % quality adjustment from certainty level
exog.A = exog.A.^(1/2.5); 
% rescaled so zero to full certainty doubles quality
% A_bar = max((repmat(exog.omega_a^2,N,1) + diag(Omega_UB))); 
% exog.A = repmat(A_bar- exog.omega_a^2, [exog.num_states, N])...
%     - exog.state_space(:,exog.diag_state_vars); % quality adjustment from certainty level
% exog.A = exog.A ./ max(exog.A) +1;

% iceberg trade costs 
exog.tau = -tau_ij*eye(N)+ repmat(1+tau_ij, N,N);

%fixed costs of production 
fc_base = .1;
exog.fc = ones(N, N) * (fc_base * (1+fc_multiplier)) + eye(N, N) * (fc_base - fc_base * (1+fc_multiplier));

% Data parameters
exog.co = co;
exog.ci = ci;
exog.zi_max = 1; % upper bound on transaction scraping ability
exog.zi_min = 0; % lower bound on transaction scraping ability

% Entry
exog.c_entry = fc_base*entry_multiplier; %% melitz and Burstein (2008)
exog.m_bar = 1;
exog.eta = 1000;    % elasticity of entry


% Generate Possible Networks
for i = 1:N; in_countries{i} = 0:1; end
network_list = combvec(in_countries{:})';
exog.num_networks = size(network_list,1);
exog.array_network_list = network_list;
exog.network_list = mat2cell(network_list, ones(1, size(network_list, 1)), size(network_list, 2));

% Add Pareto Distribution
pareto_shape = 8.25; %% arkolakis market penetration 
pareto_scale = 1;    % arbitrary 
exog.phi_options = phi_options;
[exog.g_of_phi, exog.phi_distrib] = discretize_pareto(pareto_shape,pareto_scale,.999, exog.phi_options);

% add N_only which equals 1 if in PE or N if GE 
if equilib_setting == "PE"; N_only =1; else N_only = N;end 
exog.N_only = N_only;
% add divisions necessary for parallelization
potential_indeces = num_states*phi_options*N_only*exog.num_networks; division_size = ceil(potential_indeces/ num_divisions);
divisions = cell(1, num_divisions);
for i = 1:num_divisions
    starting = (i-1)*division_size +1;
    ending = min(i*division_size, potential_indeces);
    divisions{i} = starting:ending;
end
exog.divisions = divisions; exog.num_divisions = num_divisions; 
exog.v_size = [num_states, phi_options, N_only, exog.num_networks];

fields = fieldnames(exog); % Get the field names of the structure
    for i = 1:length(fields)
        fieldName = fields{i};
        eval([fieldName ' = exog.' fieldName ';']);
    end

border_adjustment = cell(num_states, exog.num_networks);
parfor index = 1:num_states*exog.num_networks
    [state_num, network] = ind2sub([num_states, exog.num_networks], index)
     border_adjustment{index} = do_nothing_drift(exog,network,state_num);
end
exog.border_adjustment = border_adjustment;

choice_vec = [repmat(1000,1,N), zeros(1, 2*N)];
adjustments_needed = 0;
%index = sub2ind([num_states, exog.num_networks],161,2);
for index = 1:num_states*exog.num_networks
    temp = border_adjustment{index};
    adjustments_needed = adjustments_needed + any(temp(:)>0);
    [state_num, network] = ind2sub([num_states, exog.num_networks], index);
     drift = drift_calc(exog,choice_vec, P, network, state_num);
     state_drift = zeros(1,num_state_vars);
        for i = 1:num_state_vars
            state_drift(i) = drift(state_keys(i,1),state_keys(i,2));
        end
        if any(state_space(state_num,:) == 0 & state_drift < 0)
            disp(['not adjusting correctly at state: ', num2str(state_num), ' network: ', num2str(network)])
        end
end
disp(['adjustments needed: ', num2str(adjustments_needed)])
%% now also set the starting point
 

 v = zeros(num_states, phi_options, N_only, exog.num_networks); v_size = size(v);
 choices_up = cellfun(@(x) zeros(1, exog.num_choice_vars), cell(exog.v_size), 'UniformOutput', false);

% initial starting point comes from ignoring data choice, and setting
% according to standard CES markup 
for index = 1:numel(v)
    [state_num, phi_idx, home_country, network] = ind2sub(v_size,index);
    phi = exog.phi_distrib(phi_idx);
    choices_up{index}(1:size(exog.tau,1)) = exog.sigma_tilde*exog.tau(home_country,:)*exog.w/phi;
    profit =  profit_calc(exog,choices_up{index}, P, home_country, network, state_num, phi);
    v(index) = profit/ (exog.beta + exog.rho);
end

starting_point.v =v; starting_point.choices_up = choices_up; starting_point.choices_b = choices_up;
starting_point.choices_f = choices_up;
starting_point.completely_finished = 0; starting_point.need_to_launch_jobs = 1;
% for the inner loop calculations the different county - phi combination can
% run independently. This marks if any of their resepctive inner loops have
% converged 
starting_point.change = Inf; 
end

function [g_of_phi, phi] = discretize_pareto(shape, scale, percentile, num_points)
        % Calculate scale_max based on the given percentile
        scale_max = scale * ((1 - percentile)^(-1/shape));
        
        % Discretization
        x_values = linspace(scale, scale_max, num_points+1); % interval boundaries

        % Calculate midpoints
        midpoints = (x_values(1:end-1) + x_values(2:end)) / 2;

        % Calculate probabilities for each interval
        probabilities = diff(1 - (scale ./ x_values).^shape);

        % Normalize probabilities
        probabilities = probabilities / sum(probabilities);
        
        % Store results
        g_of_phi = probabilities';
        phi= midpoints';
end
function Omega_0 = fake_layp(D, Q)
n = size(D, 1); % Assuming D is square
% Construct the Kronecker product
K = kron(D, eye(n)) + kron(eye(n), D);

% Solve for the vectorized Omega
vec_Omega = -K \ reshape(Q, [], 1);

% Reshape the solution back to matrix form
Omega_0 = reshape(vec_Omega, n, n);
end 

 

