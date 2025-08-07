clear all; close all; clc;
addpath(genpath('../b_helper_functions'));
addpath(genpath('d_helper_functions'))
addpath(genpath('d_output'))
dbstop if error

%% setup 
params = dh0_set_invariant_params();
params.networks = [1,0;1,1];
params.y = [10,10];
P0 = 1.25532;
eq_crit = 1e-5;
P = P0; cache = {};

[value, cache] = eval_P(P,cache, params); value_new = Inf;
if value > 0; adjust = .999; else; adjust = 1.001; end
while all([abs(value), abs(value_new )] > eq_crit)
P_new = P*adjust;
[value_new, cache] = eval_P(P_new,cache, params, false);
if value_new*value < 0
    if(abs(value_new) < abs(value))
        adjust = 1+ (1-adjust);value = value_new; P = P_new;
    else
        adjust = .9*adjust + .1;
    end
else
    value = value_new; P = P_new;
end
end

function [value, cache] = eval_P(P, cache, params, graph_analysis)
params.P = [P,P];
if isempty(cache)
      v_hjb = dh9_HJB_inner_loop(zeros(params.len_Sigma, params.num_networks),params);
      output  = dh10_LCP_inner_loop(v_hjb, params, false);
else
    distances = cellfun(@(entry) norm(P - entry.P), cache);
    [~, idx] = min(distances);
    v0 = cache{idx}.v;
    output = dh10_LCP_inner_loop(v0, params, graph_analysis);
end
value = output.entrance_v- params.ec(1);
cache{end+1} = struct('P', {P}, 'v', output.v);
fprintf('P = %g; excess value = %g\n',P, output.entrance_v - params.ec(1))
end

