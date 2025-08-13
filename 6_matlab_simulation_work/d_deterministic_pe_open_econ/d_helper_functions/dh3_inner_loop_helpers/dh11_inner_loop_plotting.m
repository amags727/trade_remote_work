function output= dh11_inner_loop_plotting(optim, params,preferred_network,v, output)

%% UNPACK PARAMS 
fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end

% setup the while loop
Sigma_t = Sigma(len_Sigma,:); network_t = 1; drift_mag = Inf; drift_crit = 7e-3; max_tsteps = 1e4;
graph_output = array2table([(1:max_tsteps)', zeros(max_tsteps,16)], 'VariableNames',...
    {'t','network', 'Sigma_11','Sigma_12', 'Sigma_22', 'Cert_11', 'Cert_12','Cert_22',...
    'pi_tot', 'pi_1', 'pi_2', 'x_1', 'x_2', 'l_1', 'l_2', 'R_1', 'R_2'});

t = 1;
% iterate until we hit ss 
while (drift_mag > drift_crit) && (t < max_tsteps)
    [idx, wgt] = interp8_trilinear(Sigma_t, g1,g2,g3,I);

    % compute each component of graph_output
    graph_output{t, {'network', 'Sigma_11', 'Sigma_12', 'Sigma_22'}} = [network_t, Sigma_t];
    graph_output{t, {'x_1', 'x_2'}} = wgt' * E_x(idx,:,network_t);
    graph_output{t, {'l_1', 'l_2'}} = wgt' * optim.L(idx,:,network_t);
    graph_output{t, {'R_1', 'R_2'}} = wgt' * optim.R_vec(idx,:,network_t);
    graph_output{t, 'pi_tot'} = wgt' * optim.profit_w_actions(idx, network_t);
    graph_output{t, {'pi_1', 'pi_2'}} = wgt' * (E_pi(idx,:,network_t) - optim.L(idx,:,network_t)*w - fc.*networks(network_t,:));
    
    % account for entry 
    %if t>1 && network_t==2 && graph_output{t-1,network}==1
    %  graph_output.pi_tot(t) =graph_output.pi_tot(t) - ec(2);
    %  graph_output.pi_2(t) = graph_output.pi_2(t) - ec(2) {t,'prof'};
    %end 

   
    % stop if we've converged despite fluctuation around ss 
    if t > 40 && mod(t,10)==0
        oldm = mean(graph_output{t-39:t-20,{'Sigma_11','Sigma_22'}},1);
        newm = mean(graph_output{t-19:t,{'Sigma_11','Sigma_22'}},1);
        if max(abs(oldm - newm)) < 1e-4, break; end
    end

     % drift step
    drift_t  = wgt' * (optim.drift(idx,:,network_t));  
    drift_mag = sum(abs(drift_t([1,3])));
    Sigma_t   = Sigma_t + drift_t / Delta;

    % choose next network via accumarray (no loop)
    pref = preferred_network(idx, network_t);
    best_w_by_net = accumarray(pref, wgt, [num_networks,1], @sum, 0);
    [~, network_t] = max(best_w_by_net);

    t = t + 1;
end
if t == max_tsteps; disp('firm never reached a ss'); end
graph_output.t = graph_output.t /100;
graph_output = graph_output(1:find(graph_output.network == 0, 1, 'first')-1,:);
graph_output{:, {'Cert_11','Cert_12','Cert_22'}} = 1- graph_output{:,{'Sigma_11','Sigma_12','Sigma_22'}} ./ Sigma(end,:);
network_ss = network_t;
v_ss = wgt' * v(idx,network_t);
A_tilde_out = wgt'* A_tilde(idx, :).*networks(network_ss,:);
market_2_entrance_t = find(graph_output.network == 2, 1 ,'first');
output_names = {'graph_output', 'network_ss','v_ss', 'A_tilde_out','market_2_entrance_t'};
for i = 1:length(output_names); name = output_names{i}; output.(name) = eval(name); end
end


function [idx8, w8] = interp8_trilinear(x, g1, g2, g3, I)
    % Clamp x into domain
    x1 = min(max(x(1), g1(1)), g1(end));
    x2 = min(max(x(2), g2(1)), g2(end));
    x3 = min(max(x(3), g3(1)), g3(end));

    % Lower cell indices in [1, I-1]
    i1 = min(max(findlastle(g1, x1), 1), I-1);
    j1 = min(max(findlastle(g2, x2), 1), I-1);
    k1 = min(max(findlastle(g3, x3), 1), I-1);

    % Local coordinates t in [0,1] with denom guards
    d1 = g1(i1+1) - g1(i1); t1 = 0; if d1 ~= 0, t1 = (x1 - g1(i1))/d1; end
    d2 = g2(j1+1) - g2(j1); t2 = 0; if d2 ~= 0, t2 = (x2 - g2(j1))/d2; end
    d3 = g3(k1+1) - g3(k1); t3 = 0; if d3 ~= 0, t3 = (x3 - g3(k1))/d3; end
    t1 = min(max(t1,0),1); t2 = min(max(t2,0),1); t3 = min(max(t3,0),1);

    % Corner index vectors 
    Ii = [i1, i1+1]; Jj = [j1, j1+1]; Kk = [k1, k1+1];

    % Linear indices (pick one: sub2ind or sub3)
    % Using sub2ind:
    I2 = I*I;                  
    base = i1 + (j1-1)*I + (k1-1)*I2;  
    idx8 = base + [0; 1; I; 1 + I; I2; 1 + I2; I + I2;1 + I + I2];

    % Weights (Kronecker of [1-t; t])
    w1 = [1-t1; t1]; w2 = [1-t2; t2]; w3 = [1-t3; t3];
    w8 = [ w1(1)*w2(1)*w3(1);
           w1(2)*w2(1)*w3(1);
           w1(1)*w2(2)*w3(1);
           w1(2)*w2(2)*w3(1);
           w1(1)*w2(1)*w3(2);
           w1(2)*w2(1)*w3(2);
           w1(1)*w2(2)*w3(2);
           w1(2)*w2(2)*w3(2) ];
end

function k = findlastle(g, x)
    % binary search for max k with g(k) <= x (much faster than find/unique per step)
    lo = 1; hi = numel(g)-1;
    if x <= g(1), k = 1; return; end
    if x >= g(end-1), k = numel(g)-1; return; end
    while lo <= hi
        mid = bitshift(lo+hi,-1);
        if g(mid) <= x, lo = mid+1; else, hi = mid-1; end
    end
    k = hi;
end


