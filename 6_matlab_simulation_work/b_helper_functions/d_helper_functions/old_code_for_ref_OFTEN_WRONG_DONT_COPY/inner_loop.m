function inner_loop_output = inner_loop(exog,P, starting_point, doing_lcp, local)

%% unpack the structures
structures = {'exog', 'starting_point'};
for idx = 1:length(structures)
    fields = fieldnames(eval(structures{idx})); % Get the field names of the structure
    for idx2 = 1:length(fields)
        fieldName = fields{idx2};
        eval([fieldName ' = ' structures{idx} '.' fieldName ';']);
    end
end


%% generate our forward and backward approximations of the value function derivs
V  = v; v_size = size(V);
dv_b = zeros(num_states, phi_options, num_state_vars, N_only, num_networks);
dv_f = zeros(num_states, phi_options, num_state_vars, N_only, num_networks);
state_list = (1:num_states)';

for index = 1:phi_options*num_state_vars*N_only*num_networks
    [phi_idx, state_var,home_country, network] = ind2sub([phi_options,num_state_vars,N_only,num_networks],index);
    indeces_b = state_list(exog.state_minus_1(:,state_var) > 0);
    indeces_b_backward = exog.state_minus_1(indeces_b, state_var);

    dv_b(indeces_b, phi_idx, state_var, home_country, network) = ...
        (V(indeces_b, phi_idx, home_country, network) - ...
        V(indeces_b_backward, phi_idx, home_country, network)) / step_size(state_var);

    indeces_f = state_list(exog.state_plus_1(:,state_var) > 0);
    indeces_f_forward = exog.state_plus_1(indeces_f, state_var);

    dv_f(indeces_f, phi_idx, state_var, home_country, network) = ...
        (V(indeces_f_forward, phi_idx, home_country, network) - ...
        V(indeces_f, phi_idx, home_country, network)) / step_size(state_var);
end
dv_b(dv_b ==0) = dv_f(dv_b ==0); dv_f(dv_f ==0) = dv_b(dv_f==0);
starting_point.dv_b = dv_b; starting_point.dv_f = dv_f; starting_point.P = P;

%% determine our optimal behavior given value func. and perform upwind procedure
 output = inner_loop_optimization_calcs(exog,starting_point,1);
    structures = {'output'};
    for idx = 1:length(structures)
        fields = fieldnames(eval(structures{idx})); % Get the field names of the structure
        for idx2 = 1:length(fields)
            fieldName = fields{idx2};
            eval([fieldName ' = ' structures{idx} '.' fieldName ';']);
        end
    end 

% %version for if we divide up the work 
% if local
%    output = inner_loop_optimization_calcs(exog,starting_point,1);
%     structures = {'output'};
%     for idx = 1:length(structures)
%         fields = fieldnames(eval(structures{idx})); % Get the field names of the structure
%         for idx2 = 1:length(fields)
%             fieldName = fields{idx2};
%             eval([fieldName ' = ' structures{idx} '.' fieldName ';']);
%         end
%     end 
% else 
%     for space_saver = 1:1
%         % save the new approximations of the value func derivs
%         save('outputs/inner_loop_intermediates/current_starting_point.mat','starting_point');
% 
%         % clear out outputs from last round
%         system("rm outputs/slurm/*");
% 
%         %if necessary, launch jobs for performing inner loop optimization
%         if starting_point.need_to_launch_jobs
%             command = ['sbatch --array=1-',num2str(num_divisions), ' helper_functions/batch_submission.sh'];
%             system(command);
%         end
%         % wait till all the jobs are done
%         while sum(endsWith({dir('outputs/slurm/').name}, '.mat'))< num_divisions
%         end
% 
%         % import the results from each divisions' optimization
%         drift_b  =  cell(v_size); drift_f = cell(v_size);
%         choices_inner = cell(v_size); drift_inner = cell(v_size); profits_inner = zeros(v_size);
%         choices_f = choices_inner; choices_b = choices_inner; 
% 
%         for division_num = 1:num_divisions
%             failed = 1;
%             while failed
%                 try
%                     output = load(['outputs/slurm/output_',num2str(division_num),'.mat']).output;
% 
%                     for index = exog.divisions{division_num}
%                         choices_b{index} = output.choices_b{index};
%                         choices_f{index} = output.choices_f{index};
%                         choices_inner{index} = output.choices_inner{index};
%                         profits_inner(index) = output.profits_inner(index);
%                         drift_b{index} = output.drift_b{index}; drift_f{index} = output.drift_f{index};
%                         drift_inner{index} = output.drift_inner{index};
%                     end
%                     failed = 0;
% 
%                 catch
%                     % if we can't load a particular output; delete it and then wait for it to reload
%                     system(['rm outputs/slurm/output_',num2str(division_num),'.mat']);
%                     while sum(endsWith({dir('outputs/slurm/').name}, '.mat'))< num_divisions
%                     end
%                 end
%             end
%         end
%     end
% end
%
%% generate A matrices
A_matrices = cell(phi_options,N_only, num_networks); B = A_matrices; divisor = A_matrices;
for index = 1:phi_options*N_only*num_networks
    [phi_idx, home_country, network] = ind2sub([phi_options,N_only,num_networks],index);
    A_matrices{phi_idx,home_country, network} = sparse(num_states, num_states);
    temp_drift_b = cell2mat(drift_b(:,phi_idx, home_country, network));
    temp_drift_f = cell2mat(drift_f(:,phi_idx, home_country, network));

    for state_var = 1:exog.num_state_vars
        % Construct X diagonal
        X_backward_indeces = exog.state_minus_1(exog.state_minus_1(:,state_var)>0,state_var);
        X_base_indeces = state_list(exog.state_minus_1(:,state_var)>0);
        X_indeces = sub2ind([num_states, num_states], X_base_indeces, X_backward_indeces);
        X_vec =  -min(temp_drift_b(X_base_indeces, state_var),0) / step_size(state_var);
        A_matrices{phi_idx,home_country,network}(X_indeces) = A_matrices{phi_idx, home_country,network}(X_indeces) +...
            X_vec;

        % Construct Y Diagonal
        Y_indeces = sub2ind([num_states, num_states], 1:num_states, 1:num_states);
        Y_vec = (-max(temp_drift_f(:,state_var),0) + min(temp_drift_b(:, state_var),0))...
            / step_size(state_var);
        A_matrices{phi_idx,home_country,network}(Y_indeces) = A_matrices{phi_idx, home_country,network}(Y_indeces)' +...
            Y_vec;

        % Construct Z Diagonal
        Z_base_indeces = state_list(exog.state_plus_1(:,state_var)>0);
        Z_forward_indeces = exog.state_plus_1(exog.state_plus_1(:,state_var)>0,state_var);
        Z_indeces = sub2ind([num_states, num_states], Z_base_indeces, Z_forward_indeces);
        Z_vec =  max(temp_drift_f(Z_base_indeces,state_var),0) /step_size(state_var);
        A_matrices{phi_idx,home_country,network}(Z_indeces) = A_matrices{phi_idx,home_country,network}(Z_indeces) +...
            Z_vec;
    end
end

%% for each phi-home_country-network combination solve the LCP (or just solve normal problem)
if ~doing_lcp
    sub_V = cell(phi_options, N_only, num_networks);
    for index = 1:phi_options*N_only*num_networks
        [phi_idx, home_country, network] = ind2sub([phi_options,N_only, num_networks],index);
        B = (rho + exog.beta + dt)*speye(num_states) - A_matrices{phi_idx,home_country,network};
        divisor = squeeze(profits_inner(:, phi_idx, home_country,  network)) + V(:, phi_idx, home_country, network)*dt;
        sub_V{index} = B\divisor;
    end
    V = reshape(cell2mat(sub_V), v_size);
else
    env = exog; env.A_matrices = A_matrices; env.V = V;
    env.profits_inner = profits_inner;  env.stack_length = num_networks*num_states;
    V_stacked = cell(phi_options, N_only);
    inaction_region_stacked = V_stacked; best_alternative_stacked = V_stacked;
    inaction_region = zeros(v_size); best_alternative = zeros(v_size);

    parfor index = 1:phi_options*N_only
        [phi_idx, home_country] = ind2sub([env.phi_options,env.N_only],index);
        [V_stacked{index},inaction_region_stacked{index},best_alternative_stacked{index}] = LCP_wrapper(env, home_country, phi_idx);
    end
  
    for index = 1:phi_options*N_only*num_networks
        [phi_idx, home_country, network] = ind2sub([phi_options,N_only],index);
        current_indeces = (1:num_states) + num_states*(network-1);

        V(:, phi_idx, home_country, network) = V_stacked{phi_idx, home_country}(current_indeces);
        inaction_region(:, phi_idx, home_country, network) = inaction_region_stacked{phi_idx, home_country}(current_indeces);
        best_alternative(:, phi_idx, home_country, network) = best_alternative_stacked{phi_idx, home_country}(current_indeces);
    end
end

% included for diagnostic purposes could just do: change = max(abs(V(:)-v(:))
change = zeros(1,num_networks);
for network = 1:num_networks
    diff = abs(V(:,:,:,network)- v(:,:,:,network));
    change(network) = max(diff(:));
end
%disp(change)
change = max(change);
%% output results 
v= V; choices_up = choices_inner;
fields = {'profits_inner', 'drift_inner', 'drift_b', 'drift_f', 'change', 'v', 'choices_up',...
    'A_matrices', 'choices_b', 'choices_f'};
if doing_lcp; fields{end+1} = 'best_alternative'; fields{end+1} = 'inaction_region'; end 

for i = 1:size(fields,2)
    eval('inner_loop_output.' + string(fields{i}) + ' = ' +string(fields{i}) +';')  
end


%% cluster maintanence
if ~local
    inner_loop_output.need_to_launch_jobs = 0; % jobs already running
    inner_loop_output.completely_finished = 0; % jobs shouldn't be killed yet; will be turned on manually later
    starting_point = inner_loop_output;
    save('outputs/inner_loop_intermediates/current_starting_point.mat', 'starting_point');
end
end
 