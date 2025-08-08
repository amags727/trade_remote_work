%% Carry Out Graphical Analysis 
parpool()
% phi_d variation 
params = base_params; phi_vec = linspace(.9,1.1,3); 
params.crit = 1e-2; 
graph_output = cell(1, 3);  % use cell array instead of struct array
parfor i = 1:length(phi_vec)
 l_params = params;
 l_params.phi_d = phi_vec(i); 
 last_v = dh9_run_inner_loop(init_v.hjb,false, l_params);
 graph_output{i}= dh10_graph_output(last_v, l_params, false);
end
pt_1 = [repelem([.9;1;1.1],1000), repmat((1:1000)',3,1)];
pt_2 = [graph_output{1}.output_path; graph_output{2}.output_path; graph_output{3}.output_path];
phi_d_variations = [pt_1, pt_2];
save('d_output/do3_phi_d_variations.mat', 'phi_d_variations', '-v7');

% lambda variation 
params = base_params; lambda_tilde_vec = lambda_tilde * [.9,1,1.1];
params.crit = 1e-2;  graph_output = cell(1, 3);  
parfor i = 1:length(lambda_tilde_vec)
    l_lambda_tilde = lambda_tilde_vec(i);
    l_params = params;
    [l_params.Q,~,l_params.Sigma] = dh1_make_state_space(num_mkts,I, l_lambda_tilde, sigma_z, theta);
    l_params.Sigma_mat = dh2_restructure_array(repmat(l_params.Sigma,1,1,num_networks),true, num_mkts);
    l_params.d_Sigma =  (l_params.Sigma(len_Sigma, :) - l_params.Sigma(1,:))/(I-1);
    last_v = dh9_run_inner_loop(zeros(len_Sigma, num_networks),false, l_params);
    graph_output{i}= dh10_graph_output(last_v, l_params, false);
end
pt_1 = [repelem(lambda_tilde_vec',1000), repmat((1:1000)',3,1)];
pt_2 = [graph_output{1}.output_path; graph_output{2}.output_path; graph_output{3}.output_path];
lambda_variations = [pt_1, pt_2];
save('d_output/do4_lambda_variations.mat', 'lambda_variations', '-v7');
