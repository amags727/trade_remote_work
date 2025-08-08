clear all; close all; clc;
addpath(genpath('../b_helper_functions'));
addpath(genpath('d_helper_functions'))
addpath(genpath('d_output'))
dbstop if error

%% Solve an initial GE 
params = dh0_set_invariant_params();
y = 12;
P0 = 1.25;
output = dh1_find_symmetric_ss(params, P0,12);
output.graph_output


%% Asymmetric Equilibrium mess
doing_asym_stuff = false;
if doing_asym_stuff
    params = dh0_set_invariant_params();
    y = [10,9.5];
    P0 = [1.25450325, 1.27779503];
    grid_length = .0005; num_breaks = 5;
    dual_output = dh2_find_asymmetric_ss(y, P0, grid_length, num_breaks, params);
end
