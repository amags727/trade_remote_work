clear all; close all; clc;
addpath(genpath('../b_helper_functions'));
addpath(genpath('d_helper_functions'))
addpath(genpath('d_output'))
%dbstop if error
out_dir = '../../../3) output/simulation_output/raw/';

%% Solve an initial GE 
sym_P = 0.9672; sym_y = 12; base_lambda = .5;
doing_initial = true;
if doing_initial
    params = dh0_set_invariant_params(base_lambda);
    output = dh1_find_symmetric_ss(params, sym_P,sym_y); 
    writetable(output.graph_output, fullfile(out_dir, '1_base_ss_progression.csv'));
    params.sym_num_firms = output.num_firms;
    pe_params = params; pe_params.y = [sym_y,sym_y]; pe_params.P = output.P;
end

%% 2) plot the ss spend levels of different phi_g
doing_anal_2 = false;
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
scales = [ 0.9, 1, 1.1]; results_mat = [];
for s = scales
    l_params = pe_params; l_params.phi_d = s * pe_params.phi_d;
    out = dh10_LCP_inner_loop(output.v, l_params, true); g_out = out.graph_output;
    results_mat = [results_mat; [l_params.phi_d * ones(height(g_out), 1), g_out{:, {'t', 'x_1','x_2'}}]];
end
  results_mat = array2table(results_mat, 'VariableNames',{'phi_d', 't', 'x1', 'x2'});
  writetable(results_mat, fullfile(out_dir, '3_phi_d_x_pe_growth.csv'));
end

%% 4) Plot progression to ss at three different Lambda tilde values
doing_anal_4 = false; 
if doing_anal_4
scales = [ 0.9, 1, 1.1]; results_mat = [];
for s = scales
    l_params = dh0_set_invariant_params(s*base_lambda);
    l_params.y =  pe_params.y; 
    l_params.P = pe_params.P;

    out = dh10_LCP_inner_loop(output.v, l_params, true); g_out = out.graph_output;
    results_mat = [results_mat; [s*base_lambda * ones(height(g_out), 1), g_out{:, {'t','x_1','x_2'}}]];
end
  results_mat = array2table(results_mat, 'VariableNames',{'lambda', 't', 'x1', 'x2'});
  writetable(results_mat, fullfile(out_dir, '4_lambda_x_pe_growth.csv'));
end


%% 5) Plot impact on concentration at different phi_d levels in symmetric ss
doing_anal_5 = false;

if doing_anal_5
    params = dh0_set_invariant_params(base_lambda); 
    grid_points = 10;
    phi_d_vec = linspace(0,params.phi_d*1.5,grid_points)';
    results = array2table([phi_d_vec, zeros(grid_points,4)], 'VariableNames',...
        {'phi_d', 'num_firms', 'x_1', 'x_2', 'time_to_ss'});
    graph_results = [];
    P0 = sym_P;
    for i = 1:grid_points; disp(i)
        l_params = params;
        l_params.phi_d =phi_d_vec(i);
        output = dh1_find_symmetric_ss(l_params, P0,sym_y); g_out = output.graph_output;
        
        % update initial P 
        %P0 = output.P(1);
        
        % log results 
        graph_results = [graph_results; [phi_d_vec(i)* ones(height(g_out), 1), g_out{:, {'t','x_1','x_2'}}]];
        results.num_firms(i) = output.num_firms;
        results{i, {'time_to_ss','x_1','x_2'}} = g_out{end, {'t','x_1','x_2'}};
    end
    graph_results = array2table(graph_results,'VariableNames',{'phi_d', 't', 'x_1', 'x_2'});
    writetable(results, fullfile(out_dir, '5a_concentration_anal_summ_stats.csv'));
    writetable(graph_results, fullfile(out_dir, '5b_concentration_anal_graph_results.csv'));
end


%% Round 6 Home Country Analysis 
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
        e_x1 =  num_firms(1) * dual_output{1}.graph_output{end,{'x_1','x_2'}};
        e_x2 =  num_firms(2) * flip(dual_output{2}.graph_output{end,{'x_1','x_2'}});
        trade_share = e_x1 ./(e_x1 +e_x2);
        results{i,{'num_firms1', 'num_firms2', 'trade_share1', 'trade_share2'}} = [num_firms,trade_share];  
        disp(results(i,:))
        if any(trade_share >.99)
            fprintf("market 2 dropped out")
            break
        end
    end
    results = results(1:i,:);
    writetable(results, fullfile(out_dir, '6_home_ctry.csv'));
end
