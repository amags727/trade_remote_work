clear all; close all; clc;
addpath(genpath('../b_helper_functions'));
addpath(genpath('d_helper_functions'))
addpath(genpath('d_output'))
dbstop if error
out_dir = '../../../3) output/simulation_output/raw/';

%% Solve an initial GE 
sym_P = 0.9672; sym_y = 12;
doing_initial = true;
if doing_initial
    base_lambda = .5;
    params = dh0_set_invariant_params(base_lambda);
    y = sym_y;
    P0 = 1.25;
    output = dh1_find_symmetric_ss(params, P0,12); 
    writetable(output.graph_output, fullfile(out_dir, '1_base_ss_progression.csv'));
    params.sym_num_firms = output.num_firms;
    pe_params = params; pe_params.y = [y,y]; pe_params.P = output.P;
end

%% 2) plot the ss spend levels of different phi_g
doing_anal_2 = true;
if doing_anal_2
    grid_points = 100; phi_d_vec = linspace(.9*params.phi_g, 1.1*params.phi_g,grid_points)';
    results_mat = [phi_d_vec, zeros(grid_points,2)]; 
    parfor i =1:length(results_mat)
        disp(i)
        l_params = pe_params;
        l_params.phi_g = phi_d_vec(i);
        l_output = dh10_LCP_inner_loop(output.v, l_params, true);
        results_mat(i,2:3) = l_output.graph_output{end, {'l_1', 'l_2'}};
    end
    results_mat = array2table(results_mat,'VariableNames',{'phi_g', 'l_1','l_2'});
    writetable(results_mat, fullfile(out_dir, '2_phi_g_x_data.csv'))
end

%% 3) Plot progression to ss at three different phi_d values 
doing_anal_3 = false; 
if doing_anal_3
scales = [ 0.8, .9, 1]; results_mat = [];
for s = scales
    l_params = pe_params; l_params.phi_d = s * pe_params.phi_d;
    out = dh10_LCP_inner_loop(output.v, l_params, true); g_out = out.graph_output;
    results_mat = [results_mat; [l_params.phi_d * ones(length(g_out), 1), (1:length(g_out))', g_out(:, 9:10)]];
end
  results_mat = array2table(results_mat, 'VariableNames',{'phi_d', 't', 'x1', 'x2'});
  writetable(results_mat, fullfile(out_dir, '3_phi_d_x_pe_growth.csv'));
end

%% 4) Plot progression to ss at three different Lambda tilde values
doing_anal_4 = true; 
if doing_anal_4
scales = [ 0.8, .9, 1]; results_mat = [];
for s = scales
    l_params = dh0_set_invariant_params(s*base_lambda);
    l_params.y =  pe_params.y; 
    l_params.P = pe_params.P;

    out = dh10_LCP_inner_loop(output.v, l_params, true); g_out = out.graph_output_tbl;
    results_mat = [results_mat; [s*base_lambda * ones(height(g_out), 1), (1:height(g_out))', g_out{:, {'x_1','x_2'}}]];
end
  results_mat = array2table(results_mat, 'VariableNames',{'lambda', 't', 'x1', 'x2'});
  writetable(results_mat, fullfile(out_dir, '4_lambda_x_pe_growth.csv'));
end


%% A
doing_asym_stuff = true;
if doing_asym_stuff 
    test_lambda = .4;
    params = dh0_set_invariant_params(test_lambda);
    P0 = sym_P*ones(1,2);
    grid_points = 30; phi_d_vec = linspace(0,params.phi_d*1.5,grid_points)';
    results = [phi_d_vec, zeros(grid_points,4)]; cutoff = 1e-2; 
    results = array2table(results, 'VariableNames',{'phi_d', 'num_firms1', 'num_firms2', 'trade_share1', 'trade_share2'});
    
    for i =1:height(results)
        fprintf('round %g / %g\n', i, grid_points)
        l_params = params;
        y = [sym_y, 0.99922*sym_y];
        l_params.phi_d = results.phi_d(i);
      
        [P_out,dual_output,num_firms] =  dh2_find_asymmetric_ss(y, P0, l_params, cutoff);
        e_x1 =  num_firms(1) * dual_output{1}.graph_output(end,9:10);
        e_x2 =  num_firms(2) * flip(dual_output{2}.graph_output(end,9:10));
        trade_share = e_x1 ./(e_x1 +e_x2);
        results{i,{'num_firms1', 'num_firms2', 'trade_share1', 'trade_share2'}} = [num_firms,trade_share];  
        disp(results(i,:))
        if any(trade_share >.99)
            fprintf("market 2 dropped out")
            break
        end
    end
    results = results(1:i,:);
    save('../../../3) output/simulation_output/raw/3_home_ctry.mat','results')
end
